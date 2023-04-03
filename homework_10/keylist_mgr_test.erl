-module(keylist_mgr_test).

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun setup/0, fun teardown/1, F}).

-define(TEST_KEYLIST_1, keylist1).

keylist_mgr_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            fun test_start_child_get_names/0,
            fun test_start_stop_child/0,
            fun test_stop_restart_permanent/0
        ]
    }.

setup() ->
    {ok, Pid, _MonitorRef} = keylist_mgr:start(),
    #{pid => Pid}.

teardown(#{pid := _Pid}) ->
    %%    erlang:monitor(process, Pid),
    keylist_mgr:stop(),
    receive
        {'DOWN', _Ref, process, _, _Reason} ->
            ok
    end.

test_start_child_get_names() ->
    keylist_mgr:start_child(#{name => ?TEST_KEYLIST_1, restart => permanent}),
    ?assertMatch({ok, _Pid}, wait_result()),

    keylist_mgr:get_names(),
    ?assertMatch({ok, [?TEST_KEYLIST_1]}, wait_result()).

test_start_stop_child() ->
    keylist_mgr:start_child(#{name => ?TEST_KEYLIST_1, restart => permanent}),
    ?assertMatch({ok, _Pid}, wait_result()),

    keylist_mgr:stop_child(?TEST_KEYLIST_1),
    ?assertMatch({ok, _}, wait_result()),

    keylist_mgr:get_names(),
    ?assertMatch({ok, []}, wait_result()).

test_stop_restart_permanent() ->
    keylist_mgr:start_child(#{name => ?TEST_KEYLIST_1, restart => permanent}),
    ?assertMatch({ok, _Pid}, wait_result()),

    % {ok, Pid} = wait_result(),
    % ?assertMatch({ok, Pid}, wait_result()),
    % keylist_mgr:exit(Pid, killed),

    exit(whereis(?TEST_KEYLIST_1), killed),

    keylist_mgr:get_names(),
    ?assertMatch({ok, [?TEST_KEYLIST_1]}, wait_result()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wait_result() ->
    receive
        Msg ->
            Msg
    end.

% c(keylist_mgr_test).
% keylist_mgr_test:test().
