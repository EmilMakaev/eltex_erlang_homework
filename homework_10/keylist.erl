%%%-----------------------------------------------------------------------------
%%% @doc This module is used in the keylist_mgr module. The module creates a spawn
%%% or spawn_link and allows you to state as a list with Key, Value, Comment.
%%% Each change in the module, in loop we increase the counter.
%%% @end
%%%-----------------------------------------------------------------------------
-module(keylist).

-record(state, {
    list = [],
    counter = 0
}).

% API
-export([start/1, stop/1, start_link/1, add/4, is_member/2, take/2, find/2, delete/2]).
% Callback
-export([init/0]).

-type key() :: {string(), string(), string()}.

%%% @doc start fun - starts a process with a monitor manager that is waiting for
%%% a message to control the state.
%%%
-spec start(Name :: atom()) -> {ok, pid()}.
start(Name) ->
    Pid = spawn(keylist, init, []),
    monitor(process, Pid),
    register(Name, Pid),
    {ok, Pid}.

%%% @doc start_link fun - starts a process with a link manager that is waiting for
%%% a message to control the state.
%%%
-spec start_link(Name :: atom()) -> {ok, pid()}.
start_link(Name) ->
    Pid = spawn_link(keylist, init, []),
    register(Name, Pid),
    {ok, Pid}.

init() ->
    loop(#state{}).

terminate(_State) ->
    ok.

%%% @doc stop fun - stops the process
%%%
-spec stop(Name :: atom()) -> atom().
stop(Name) ->
    Name ! stop.

%%% @doc add fun - stops the manager and all his children, plus increases the counter
%%%
-spec add(Name :: atom(), Key :: string(), Value :: string(), Comment :: string()) ->
    {ok, number()}.
add(Name, Key, Value, Comment) ->
    Name ! {self(), add, Key, Value, Comment}.

%%% @doc is_member fun - checks if the specified Key is in the list, plus increases the counter
%%%
-spec is_member(Name :: atom(), Key :: string()) -> {ok, number(), atom()}.
is_member(Name, Key) ->
    Name ! {self(), is_member, Key}.

%%% @doc take fun - removes and returns Tuple if the specified Key exists otherwise returns false, plus increases the counter
%%%
-spec take(Name :: atom(), Key :: string()) -> {ok, number(), false | key()}.
take(Name, Key) ->
    Name ! {self(), take, Key}.

%%% @doc find fun - returns Tuple if the specified Key exists otherwise returns false, plus increases the counter
%%%
-spec find(Name :: atom(), Key :: string()) -> {ok, number(), false | key()}.
find(Name, Key) ->
    Name ! {self(), find, Key}.

%%% @doc delete fun - removes specified Key, plus increases the counter
%%%
-spec delete(Name :: atom(), Key :: string()) -> {ok, number()}.
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
            loop(NewState);
        stop ->
            terminate(State),
            ok
    end.

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
