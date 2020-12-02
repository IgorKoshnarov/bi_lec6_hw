-module(cache_server_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).
-compile(nowarn_export_all).

all() -> [
    % {group, api}
    api_test
].

groups() -> [
    % {api, [sequence], [new_test, insert_test, lookup_test, lookup_by_date_test]}
].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

%% Actual tests

api_test(_Config) ->
    cache_server:start_link([{drop_interval, 5}]),
    cache_server:new(numbers),
    [cache_server:insert(numbers, Key, Value, LifeTime) 
    || #{key := Key, value := Value, lifetime := LifeTime} <- data_samples()],
    FirstInsertTime = calendar:universal_time(),
    [1, 2, 3, 4] = [cache_server:lookup(numbers, Key) || #{key := Key} <- data_samples()],
    timer:sleep(2000),
    [undefined, undefined, 3, 4] = [cache_server:lookup(numbers, Key) || #{key := Key} <- data_samples()],
    [cache_server:insert(numbers, Key, Value, LifeTime) 
    || #{key := Key, value := Value, lifetime := LifeTime} <- data_samples1()],
    Now = calendar:universal_time(),
    Seconds = calendar:datetime_to_gregorian_seconds(FirstInsertTime),
    SecondAfterInsertTime = calendar:gregorian_seconds_to_datetime(Seconds + 1),
    Result1 = cache_server:lookup_by_date(numbers, FirstInsertTime, Now),
    Result2 = cache_server:lookup_by_date(numbers, SecondAfterInsertTime, Now),
    true = lists:member({3, 3}, Result1),
    true = lists:member({4, 4}, Result1),
    true = lists:member({5, 5}, Result1),
    true = lists:member({6, 6}, Result1),
    true = lists:member({5, 5}, Result2),
    true = lists:member({6, 6}, Result2),
    4 = length(Result1),
    2 = length(Result2),
    timer:sleep(6000),
    Result3 = cache_server:lookup_by_date(numbers, FirstInsertTime, Now),
    2 = length(Result3).        

% new_test(_Config) ->
%     cache_server:new(numbers).

% insert_test(_Config) ->
%     [cache_server:insert(numbers, Key, Value, LifeTime) 
%     || #{key := Key, value := Value, lifetime := LifeTime} <- data_samples()].

% lookup_test(_Config) ->
%     [1, 2, 3, 4] = [cache_server:lookup(numbers, Key) || #{key := Key} <- data_samples()],
%     timer:sleep(2000),
%     [undefined, undefined, 3, 4] = [cache_server:lookup(numbers, Key) || #{key := Key} <- data_samples()].

% lookup_by_date_test(Config) ->
%     [cache_server:insert(numbers, Key, Value, LifeTime) 
%     || #{key := Key, value := Value, lifetime := LifeTime} <- data_samples1()],
%     Now = calendar:universal_time(),
%     FirstInsertTime = ?config(first_insert_time, Config),
%     Seconds = calendar:gregorian_datetime_to_seconds(FirstInsertTime),
%     SecondAfterInsertTime = calendar:gregorian_seconds_to_datetime(Seconds + 1),
%     Result1 = cache_server:lookup_by_date(numbers, FirstInsertTime, SecondAfterInsertTime),
%     Result2 = cache_server:lookup_by_date(numbers, FirstInsertTime, Now),
%     4 = length(Result1),
%     2 = length(Result2). 

%% fixtures for testing

data_samples() -> [
    #{key => 1, value => 1, lifetime => 1},
    #{key => 2, value => 2, lifetime => 1},
    #{key => 3, value => 3, lifetime => 5},
    #{key => 4, value => 4, lifetime => 5}
].
data_samples1() -> [
    #{key => 5, value => 5, lifetime => 10},
    #{key => 6, value => 6, lifetime => 10}
].