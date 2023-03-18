-module(keylist).

-include("keylist.hrl").

-export([loop/1, start/1, start_link/1]).

returnResult(From, NewState) ->
    From ! {ok, NewState#state.counter},
    loop(NewState).
returnResult(From, NewState, Value) ->
    From ! {ok, NewState#state.counter, Value},
    loop(NewState).

increaseCounter(Counter) when is_integer(Counter) -> Counter + 1.

keytake(From, #state{list = List, counter = Counter} = State, false) ->
    NewState = State#state{list = List, counter = increaseCounter(Counter)},
    returnResult(From, NewState, false);
keytake(From, #state{counter = Counter} = State, {value, Tuple, NewTupleList}) ->
    NewState = State#state{list = NewTupleList, counter = increaseCounter(Counter)},
    returnResult(
        From,
        NewState,
        Tuple
    ).

loop(#state{list = List, counter = Counter} = State) ->
    receive
        {From, add, Key, Value, Comment} ->
            NewState = State#state{
                list = [{Key, Value, Comment} | List], counter = increaseCounter(Counter)
            },
            returnResult(From, NewState);
        {From, is_member, Key} ->
            NewState = State#state{list = List, counter = increaseCounter(Counter)},
            returnResult(From, NewState, lists:keymember(Key, 1, List));
        {From, take, Key} ->
            keytake(From, State, lists:keytake(Key, 1, List));
        {From, find, Key} ->
            NewState = State#state{list = List, counter = increaseCounter(Counter)},
            returnResult(From, NewState, lists:keyfind(Key, 1, List));
        {From, delete, Key} ->
            NewState = State#state{
                list = lists:keydelete(Key, 1, List), counter = increaseCounter(Counter)
            },
            returnResult(From, NewState)
    end.

start(Name) ->
    Pid = spawn(keylist, loop, [#state{}]),
    monitor(process, Pid),
    MonitorRef = register(Name, Pid),
    {ok, Pid, MonitorRef}.

start_link(Name) ->
    Pid = spawn_link(keylist, loop, [#state{}]),
    register(Name, Pid),
    Pid.

% c(keylist).
% keylist:start_link(linked).
% keylist:start(monitored).

% linked ! {self(), add, "first_key", "first_value", "first_comment"}.
% linked ! {self(), add, "second_key", "second_value", "second_comment"}.
% linked ! {self(), add, "third_key", "third_value", "third_comment"}.
% linked ! {self(), add, "fourth_key", "fourth_value", "fourth_comment"}.

% linked ! {self(), status}.

% linked ! {self(), is_member, "nonexistent_key"}.
% linked ! {self(), is_member, "first_key"}.

% linked ! {self(), take, "second_key"}.
% linked ! {self(), take, "nonexistent_key"}.

% linked ! {self(), find, "third_key"}.
% linked ! {self(), find, "nonexistent_key"}.

% linked ! {self(), delete, "nonexistent_key"}.
% linked ! {self(), delete, "third_key"}.

% exit(whereis(linked), killed).
% exit(whereis(monitored), killed).

% process_flag(trap_exit, true).

% ## TASK_5
% process_flag(trap_exit, false).
% keylist:start_link(linked1).
% keylist:start_link(linked2).
% self().
% exit(whereis(linked1), killed).
% self().
% whereis(linked2).
