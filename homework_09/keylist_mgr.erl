%%%-----------------------------------------------------------------------------
%%% @doc This module allows you to create a process manager with the ability
%%% to add processes and restart them on failure.
%%%
%%% The "children" list holds a tuple of process name and pid {atom(), pid()},
%%% and the permanent list holds pids that we will restart on failure.
%%% @end
%%%-----------------------------------------------------------------------------
-module(keylist_mgr).

-include("keylist_mgr.hrl").
%

% API
-export([start/0, loop/1, start_child/1, stop_child/1, stop/0, get_names/0]).
% Callbacks
-export([init/0]).

-type restart() :: permanent | temporary.
-type child() :: #{name => atom(), restart => restart()}.

% @doc start_child fun - creates a child and associates it with the manager
%
-spec start_child(Params :: child()) -> {pid(), atom(), child()}.
start_child(Params) ->
    ?MODULE ! {self(), start_child, Params}.

% @doc stop_child fun - stops the child and removes it from the manager's store
%
-spec stop_child(Name :: atom()) -> {pid(), atom(), atom()}.
stop_child(Name) ->
    ?MODULE ! {self(), stop_child, Name}.

% @doc stop fun - stops the manager and all his children
%
-spec stop() -> atom().
stop() ->
    ?MODULE ! stop.

% @doc get_names fun - returns a list of names that the manager currently has
%
-spec get_names() -> {pid(), atom()}.
get_names() ->
    ?MODULE ! {self(), get_names}.

% @doc start fun - starts a process with a monitor manager that is waiting for
% a message to control the state of inherited processes.
%
-spec start() -> {atom(), pid(), reference()}.
start() ->
    {Pid, MonitorRef} = spawn_monitor(?MODULE, init, []),
    register(?MODULE, Pid),
    {ok, Pid, MonitorRef}.

init() ->
    process_flag(trap_exit, true),
    loop(#state{}).

loop(#state{children = Children, permanent = Permanent} = State) ->
    receive
        {From, start_child, #{name := Name, restart := Restart}} ->
            case lists:keymember(Name, 1, Children) of
                false ->
                    {ok, Pid} = keylist:start_link(Name),
                    NewState = State#state{
                        children = [{Name, Pid} | Children],
                        permanent = get_type_restart_pid({Restart, Pid}) ++ Permanent
                    },
                    From ! {ok, Pid},
                    loop(NewState);
                true ->
                    loop(State)
            end;
        {From, stop_child, Name} ->
            case lists:keymember(Name, 1, Children) of
                true ->
                    exit(whereis(Name), killed),
                    NewState = State#state{
                        children = lists:keydelete(Name, 1, Children),
                        permanent = lists:delete(whereis(Name), Permanent)
                    },
                    From ! {ok, atom_to_list(Name) ++ " has been removed"},
                    loop(NewState);
                false ->
                    loop(State)
            end;
        stop ->
            exit(kill);
        {From, get_names} ->
            % From ! {ok, lists:map(fun({Name, _Pid}) -> Name end, Children)},
            From ! {ok, State},
            loop(State);
        {'Exit', Pid, Reason} ->
            io:format("Pid ~p ended due to ~p~n", [Pid, Reason]),
            NewState = State#state{
                children = lists:keydelete(Pid, 2, Children),
                permanent = lists:delete(Pid, Permanent)
            },
            loop(NewState);
        {'DOWN', _Ref, process, _Pid, Why} ->
            io:format("Process died: ~p, restarting it~n", [Why]),
            loop(State)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_type_restart_pid({permanent, Pid}) ->
    [Pid];
get_type_restart_pid({temporary, _Pid}) ->
    [].

% {'DOWN', #Ref<0.2811899199.2874408961.128723>, process, <0.104.0>, killed}

% c(keylist_mgr).
% c(keylist).

% process_flag(trap_exit, true).

% keylist_mgr:start().

% keylist_mgr:start_child(#{name => keylist1, restart => permanent}).
% keylist_mgr:start_child(#{name => keylist2, restart => temporary}).
% keylist_mgr:start_child(#{name => keylist3, restart => permanent}).

% keylist_mgr:stop_child(keylist1).
% keylist_mgr:stop().
% keylist_mgr:get_names().

% keylist_mgr ! {'Exit', start_child, keylist1}.

% whereis(keylist_mgr).
% whereis(keylist2).
% exit(whereis(keylist1), kill).
% exit(whereis(keylist_mgr), killed).

% keylist:add(keylist2, "first_key", "first_value", "first_comment").

% rr("keylist_mgr.hrl").

% start_child() ->
