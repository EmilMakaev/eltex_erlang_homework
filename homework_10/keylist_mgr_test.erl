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
            fun test_start_stop_child/0
        ]
    }.

setup() ->
    {ok, Pid, Monitor_ref} = keylist_mgr:start(),
    #{pid => Pid, monitor_ref => Monitor_ref}.

teardown(#{pid := Pid}) ->
    erlang:monitor(process, Pid),
    keylist_mgr:stop(),
    receive
        {'DOWN', _Ref, process, _Pid, _Reason} ->
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

% test_stop_restart_permanent() ->
%     {Pid, start_child, _} = keylist_mgr:start_child(
%         #{
%             name => ?TEST_KEYLIST_1, restart => permanent
%         }
%     ),
%     ?assertMatch({ok, _Pid}, wait_result()),

%     keylist_mgr:exit(Pid, killed),

%     % exit(whereis(?TEST_KEYLIST_1), killed),
%     % ?assertMatch({ok, _}, wait_result()),

%     keylist_mgr:get_names(),
%     ?assertMatch({ok, [?TEST_KEYLIST_1]}, wait_result()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wait_result() ->
    receive
        Msg ->
            Msg
    end.

% c(keylist_mgr_test).
% keylist_mgr_test:test().
