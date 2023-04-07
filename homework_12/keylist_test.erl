-module(keylist_test).

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun setup/0, fun teardown/1, F}).

-define(TEST_KEYLIST_1, keylist1).
-define(TEST_NONEXISTING_KEY, test_nonexistent_key).
-define(TEST_KEY, test_key).
-define(TEST_VALUE, test_value).
-define(TEST_COMMENT, test_comment).

keylist_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            fun test_add_is_member/0,
            fun test_add_take_is_member/0,
            fun test_add_find/0,
            fun test_add_delete_is_member/0
        ]
    }.

setup() ->
    {ok, Pid} = keylist:start_link(?TEST_KEYLIST_1),
    #{pid => Pid, name => ?TEST_KEYLIST_1}.

teardown(#{pid := Pid}) ->
    erlang:monitor(process, Pid),
    keylist:stop(?TEST_KEYLIST_1),
    receive
        {'DOWN', _Ref, process, _Pid, _Reason} ->
            ok
    end.

test_add_is_member() ->
    ?assertMatch(
        {ok, {state, [], 1}},
        keylist:add(?TEST_KEYLIST_1, ?TEST_KEY, ?TEST_VALUE, ?TEST_COMMENT)
    ),
    ?assertMatch({ok, 2, false}, keylist:is_member(?TEST_KEYLIST_1, ?TEST_NONEXISTING_KEY)),
    ?assertMatch({ok, 3, true}, keylist:is_member(?TEST_KEYLIST_1, ?TEST_KEY)).

test_add_take_is_member() ->
    ?assertMatch(
        {ok, {state, [], 1}},
        keylist:add(?TEST_KEYLIST_1, ?TEST_KEY, ?TEST_VALUE, ?TEST_COMMENT)
    ),
    ?assertMatch({ok, 2, false}, keylist:take(?TEST_KEYLIST_1, ?TEST_NONEXISTING_KEY)),
    ?assertMatch(
        {ok, 3, {?TEST_KEY, ?TEST_VALUE, ?TEST_COMMENT}}, keylist:take(?TEST_KEYLIST_1, ?TEST_KEY)
    ),
    ?assertMatch({ok, 4, false}, keylist:is_member(?TEST_KEYLIST_1, ?TEST_KEY)).

test_add_find() ->
    ?assertMatch(
        {ok, {state, [], 1}},
        keylist:add(?TEST_KEYLIST_1, ?TEST_KEY, ?TEST_VALUE, ?TEST_COMMENT)
    ),
    ?assertMatch({ok, 2, false}, keylist:find(?TEST_KEYLIST_1, ?TEST_NONEXISTING_KEY)),
    ?assertMatch(
        {ok, 3, {?TEST_KEY, ?TEST_VALUE, ?TEST_COMMENT}}, keylist:find(?TEST_KEYLIST_1, ?TEST_KEY)
    ).

test_add_delete_is_member() ->
    ?assertMatch(
        {ok, {state, [], 1}},
        keylist:add(?TEST_KEYLIST_1, ?TEST_KEY, ?TEST_VALUE, ?TEST_COMMENT)
    ),
    ?assertMatch({ok, 2}, keylist:delete(?TEST_KEYLIST_1, ?TEST_KEY)),
    ?assertMatch({ok, 3, false}, keylist:is_member(?TEST_KEYLIST_1, ?TEST_KEY)).

% c(keylist_test).
% keylist_test:test().
