-module(keylist_test).

-include("keylist.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(MONITORED, monitored).
-define(TEST_KEY, "test_key").
-define(TEST_VALUE, "test_value").
-define(TEST_COMMENT, "test_comment").

keylist_test_() ->
    {
        setup,
        fun start/0,
        fun stop/1,
        [fun add/0, fun is_member/0, fun take/0, fun find/0, fun delete/0]
    }.

add() ->
    {Pid, add, ?TEST_KEY, ?TEST_VALUE, ?TEST_COMMENT} = keylist:add(
        ?MONITORED, ?TEST_KEY, ?TEST_VALUE, ?TEST_COMMENT
    ),
    Pid.

is_member() ->
    {Pid, is_member, ?TEST_KEY} = keylist:is_member(?MONITORED, ?TEST_KEY),
    Pid.

take() ->
    {Pid, take, ?TEST_KEY} = keylist:take(?MONITORED, ?TEST_KEY),
    Pid.

find() ->
    {Pid, find, ?TEST_KEY} = keylist:find(?MONITORED, ?TEST_KEY),
    Pid.

delete() ->
    {Pid, delete, ?TEST_KEY} = keylist:delete(?MONITORED, ?TEST_KEY),
    Pid.

start() ->
    {ok, Pid} = keylist:start(?MONITORED),
    Pid.

stop(Pid) ->
    keylist:stop(Pid).

% c(keylist_test).
% keylist_test:test().
