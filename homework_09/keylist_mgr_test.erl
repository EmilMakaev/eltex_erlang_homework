-module(keylist_mgr_test).

-include("keylist_mgr.hrl").

-include_lib("eunit/include/eunit.hrl").

keylist_mgr_test_() ->
    {
        setup,
        fun start/0,
        fun stop/1,
        [fun get_names/0]
    }.

start() ->
    {ok, Pid, _MonitorRef} = keylist_mgr:start(),
    Pid.

stop(_Pid) ->
    keylist_mgr:stop().

get_names() ->
    {Pid, get_names} = keylist_mgr:get_names(),
    {Pid, get_names}.

% test1() -> ok.
% test2() -> ok.

% 2> c(keylist).
% {ok,keylist}
% 3> keylist_mgr:start().
% {ok,<0.95.0>,#Ref<0.2936251198.292290561.23955>}
% 4> keylist_mgr:start_child(#{name => keylist1, restart => permanent}).
% {<0.81.0>,start_child,
%  #{name => keylist1,restart => permanent}}
% 5> keylist_mgr:get_names().
% {<0.81.0>,get_names}

% keylist_mgr_test_() ->
%     [
%         ?_assertMatch(
%             {ok, _Pid, _MonitorRef}, keylist_mgr:start()
%         ),
%         ?_assertMatch(
%             {ok, _Pid, _MonitorRef}, keylist_mgr:start()
%         )
%     ].

% c(keylist_mgr_test).
% keylist_mgr_test:test().

% converter: map_to_rub(#{type => usd, amount => 100, commission => 0.01}).
% converter: map_to_rub(#{type => peso, amount => 12, commission => 0.02}).
% converter: map_to_rub(#{type => yene, amount => 30, commission => 0.02}).
% converter: map_to_rub(#{type => euro, amount => -15, commission => 0.02}).
