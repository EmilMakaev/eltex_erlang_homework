-module(keylist_mgr).

-include("keylist_mgr.hrl").

-export([start/0, loop/1]).

start() ->
    {Pid, MonitorRef} = spawn_monitor(?MODULE, loop, [#state{}]),
    % process_flag(Pid, save_calls, 10000),
    register(?MODULE, Pid),
    {ok, Pid, MonitorRef}.

loop(#state{children = Children} = State) ->
    process_flag(trap_exit, true),

    receive
        {From, start_child, Name} ->
            case lists:keymember(Name, 1, Children) of
                false ->
                    {ok, Pid} = keylist:start_link(Name),
                    NewState = State#state{children = [{Name, Pid} | Children]},
                    From ! {ok, Pid},
                    loop(NewState);
                true ->
                    loop(State)
            end;
        {From, stop_child, Name} ->
            case lists:keymember(Name, 1, Children) of
                true ->
                    exit(whereis(Name), killed),
                    NewState = State#state{children = lists:keydelete(Name, 1, Children)},
                    From ! {ok, atom_to_list(Name) ++ " has been removed"},
                    loop(NewState);
                false ->
                    loop(State)
            end;
        stop ->
            ok;
        {From, get_names} ->
            From ! {ok, lists:map(fun({Name, _Pid}) -> Name end, Children)},
            loop(State);
        {'Exit', Pid, Reason} ->
            io:format("Pid ~p ended due to ~p~n", [Pid, Reason]),
            NewState = State#state{children = lists:keydelete(Pid, 2, Children)},
            loop(NewState)
    end.

% c(keylist_mgr).
% c(keylist).

% process_flag(trap_exit, true).

% keylist_mgr:start().
% keylist_mgr ! {self(), start_child, keylist1}.
% keylist_mgr ! {self(), start_child, keylist2}.
% keylist_mgr ! {self(), start_child, keylist3}.
% keylist_mgr ! {self(), stop_child, keylist2}.
% keylist_mgr ! {self(), get_names}.
% keylist_mgr ! {'Exit', start_child, keylist1}.
% keylist_mgr ! stop.

% whereis(keylist_mgr). 
% exit(whereis(keylist1), killed).
% exit(whereis(keylist_mgr), killed).

% keylist3 ! {self(), add, "first_key", "first_value", "first_comment"}.

% rr("keylist_mgr.hrl").

% start_child() ->
