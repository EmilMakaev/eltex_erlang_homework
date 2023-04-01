-module(keylist).

-include("keylist.hrl").

% API
-export([start/1, stop/1, start_link/1, add/4, is_member/2, take/2, find/2, delete/2]).
% Callback
-export([loop/1]).

add(Name, Key, Value, Comment) ->
    Name ! {self(), add, Key, Value, Comment}.

is_member(Name, Key) ->
    Name ! {self(), is_member, Key}.

take(Name, Key) ->
    Name ! {self(), take, Key}.

find(Name, Key) ->
    Name ! {self(), find, Key}.

delete(Name, Key) ->
    Name ! {self(), delete, Key}.

loop(#state{list = List, counter = Counter} = State) ->
    receive
        {From, add, Key, Value, Comment} ->
            NewState = State#state{
                list = [{Key, Value, Comment} | List], counter = increaseCounter(Counter)
            },
            From ! {ok, NewState#state.counter},
            loop(NewState);
        {From, is_member, Key} ->
            NewState = State#state{list = List, counter = increaseCounter(Counter)},
            From ! {ok, NewState#state.counter, lists:keymember(Key, 1, List)},
            loop(NewState);
        {From, take, Key} ->
            case lists:keytake(Key, 1, List) of
                false ->
                    NewState = State#state{list = List, counter = increaseCounter(Counter)},
                    From ! {ok, NewState#state.counter, false},
                    loop(NewState);
                {value, Tuple, NewTupleList} ->
                    NewState = State#state{list = NewTupleList, counter = increaseCounter(Counter)},
                    From ! {ok, NewState#state.counter, Tuple},
                    loop(NewState)
            end;
        {From, find, Key} ->
            NewState = State#state{list = List, counter = increaseCounter(Counter)},
            From ! {ok, NewState#state.counter, lists:keyfind(Key, 1, List)},
            loop(NewState);
        {From, delete, Key} ->
            NewState = State#state{
                list = lists:keydelete(Key, 1, List), counter = increaseCounter(Counter)
            },
            From ! {ok, NewState#state.counter},
            loop(NewState)
    end.

start(Name) ->
    Pid = spawn(keylist, loop, [#state{}]),
    monitor(process, Pid),
    register(Name, Pid),
    {ok, Pid}.

stop(Pid) ->
    exit(Pid, killed).

start_link(Name) ->
    Pid = spawn_link(keylist, loop, [#state{}]),
    register(Name, Pid),
    {ok, Pid}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

increaseCounter(Counter) when is_integer(Counter) -> Counter + 1.

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

% 7 > keylist:start(monitored).
% {ok,<0.143.0>}
% 8> monitored ! {self(), add, "first_key", "first_value", "first_comment"}.
% {<0.141.0>,add,"first_key","first_value","first_comment"}
% 9> monitored ! {self(), status}.       
% {<0.141.0>,status}
% 10> monitored ! {self(), take, "second_key"}.
% {<0.141.0>,take,"second_key"}
% 11> monitored ! {self(), is_member, "first_key"}.
% {<0.141.0>,is_member,"first_key"}
% 12> monitored ! {self(), find, "third_key"}.
% {<0.141.0>,find,"third_key"}
% 13> monitored ! {self(), delete, "nonexistent_key"}.
% {<0.141.0>,delete,"nonexistent_key"}
% 14> 
