-module(cache_server).

-export([start_link/1, new/1, insert/4, lookup/2, lookup_by_date/3]).

-export([init/1, handle_cast/2, handle_call/3, terminate/2, code_change/3, handle_info/2]).

-behaviour(gen_server).

-define(DEFAULT_INTERVAL, 3600).

-define(NOW, calendar:datetime_to_gregorian_seconds(calendar:universal_time())).

-include("../include/cache_server.hrl").

start_link(Opts) ->
    case proplists:get_value(drop_interval, Opts) of
        undefined -> 
            gen_server:start_link({local, ?MODULE}, ?MODULE, [{drop_interval, ?DEFAULT_INTERVAL} | Opts], []);
        Interval when is_integer(Interval), Interval > 0 -> 
            gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, [])
    end.

new(TableName) ->
    gen_server:cast(?MODULE, {new, TableName}).

insert(TableName, Key, Value, LifeTime) ->
    gen_server:cast(?MODULE, {insert, TableName, Key, Value, LifeTime}).

lookup(TableName, Key) ->
    gen_server:call(?MODULE, {lookup, TableName, Key}).

lookup_by_date(TableName, DateFrom, DateTo) ->
    gen_server:call(?MODULE, {lookup_by_date, TableName, DateFrom, DateTo}).

init(Opts) ->
    DropInterval = proplists:get_value(drop_interval, Opts) * 1000,
    timer:send_after(DropInterval, self(), clean),
    {ok, #{drop_interval => DropInterval , table_names => []}}.

handle_cast({new, TableName}, #{table_names := TableNames, drop_interval := DropInterval} = State)
    when is_atom(TableName) ->
    case lists:member(TableName, maps:get(table_names, State)) of
        false ->
            ets:new(TableName, [named_table, {keypos, #kv.key}]),
            {noreply, #{drop_interval => DropInterval, table_names => [TableName | TableNames]}};
        _ -> {noreply, State}
    end;
handle_cast({insert, TableName, Key, Value, LifeTime}, #{table_names := TableNames} = State) ->
    case lists:member(TableName, TableNames) of
        true ->
            Expire = ?NOW + LifeTime,
            ets:insert(TableName, #kv{key = Key, value = Value, timestamp = ?NOW, expire = Expire});
        _ -> ok
    end,
    {noreply, State}.

handle_call({lookup, TableName, Key}, _From, #{table_names := TableNames} = State) ->
    case lists:member(TableName, TableNames) of
        false -> {reply, undefined, State};
        _ -> case ets:lookup(TableName, Key) of
            [Value] -> {reply, {ok, not_expired(Value)}, State};
            _ -> {reply, undefined, State}
        end
    end;

handle_call({lookup_by_date, TableName, DateFrom, DateTo}, _From, #{table_names := TableNames} = State) ->
    case lists:member(TableName, TableNames) of
        false -> {reply, undefined, State};
        _ -> {reply, {ok, do_lookup_by_date(TableName, DateFrom, DateTo)}, State}
    end.

handle_info(clean, State) ->
    delete_obsolete(maps:get(table_names, State)),
    timer:send_after(maps:get(drop_interval, State), self(), clean),
    {noreply, State}.

terminate(normal, _State) -> ok.

code_change(_Old, State, _Extra) -> {ok, State}.

delete_obsolete([TableName | Rest]) ->
    Now = ?NOW,
    ets:select_delete(TableName, [{#kv{expire = '$1', _ = '_'}, [{'<', '$1', Now}], [true]}]),
    delete_obsolete(Rest);
delete_obsolete([]) ->
    ok.

not_expired(#kv{value = Value, expire = Expire}) ->
    case ?NOW =< Expire of
        true -> Value;
        _ -> undefined
    end.

do_lookup_by_date(TableName, DateFrom, DateTo) ->
    Now = ?NOW,
    From = calendar:datetime_to_gregorian_seconds(DateFrom),
    To = calendar:datetime_to_gregorian_seconds(DateTo),
    ets:select(TableName, [
        {#kv{key = '$1', value = '$2', expire = '$3', timestamp = '$4'}, 
        [{'>=', '$3', Now}, {'>=', '$4', From}, {'=<', '$4', To}], 
        [{{'$1', '$2'}}]}
    ]).



