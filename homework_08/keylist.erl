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
    register(Name, Pid),
    {ok, Pid}.

start_link(Name) ->
    Pid = spawn_link(keylist, loop, [#state{}]),
    monitor(process, Pid),
    register(Name, Pid),
    {ok, Pid}.
