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
-export([start/1, start_link/1, stop/1, add/4, is_member/2, take/2, find/2, delete/2]).
% -export([add/4, is_member/2, take/2, find/2, delete/2]).

% Callback
-export([init/1, handle_call/3, handle_info/2]).

-type key() :: {string(), string(), string()}.

-spec start(Name :: atom()) -> {ok, pid()}.
start(Name) ->
    gen_server:start({local, Name}, ?MODULE, [], []).

-spec start_link(Name :: atom()) -> {ok, pid()}.
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

-spec stop(Name :: atom()) -> shutdown_ok.
stop(Name) ->
    gen_server:call(Name, stop).

% API
-spec add(Name :: atom(), Key :: string(), Value :: string(), Comment :: string()) ->
    {ok, {state, list(key()), number()}}.
add(Name, Key, Value, Comment) ->
    gen_server:call(Name, {add, Key, Value, Comment}).

-spec is_member(Name :: atom(), Key :: string()) -> {ok, number(), true | false}.
is_member(Name, Key) ->
    gen_server:call(Name, {is_member, Key}).

-spec take(Name :: atom(), Key :: string()) -> {ok, number(), false | key()}.
take(Name, Key) ->
    gen_server:call(Name, {take, Key}).

-spec find(Name :: atom(), Key :: string()) -> {ok, number(), false | key()}.
find(Name, Key) ->
    gen_server:call(Name, {find, Key}).

-spec delete(Name :: atom(), Key :: string()) -> {ok, number()}.
delete(Name, Key) ->
    gen_server:call(Name, {delete, Key}).

% Callback
init(_Args) ->
    {ok, #state{}}.

% Call
handle_call(
    {add, Key, Value, Comment} = _Msg, _From, #state{list = List, counter = Counter} = State
) ->
    NewState = State#state{
        list = [{Key, Value, Comment} | List], counter = increaseCounter(Counter)
    },
    {reply, {ok, State#state{counter = Counter + 1}}, NewState};
handle_call(
    {is_member, Key} = _Msg, _From, #state{list = List, counter = Counter} = State
) ->
    NewState = State#state{list = List, counter = increaseCounter(Counter)},
    {reply, {ok, NewState#state.counter, lists:keymember(Key, 1, List)}, NewState};
handle_call(
    {take, Key} = _Msg, _From, #state{list = List, counter = Counter} = State
) ->
    case lists:keytake(Key, 1, List) of
        false ->
            NewState = State#state{list = List, counter = increaseCounter(Counter)},
            {reply, {ok, NewState#state.counter, false}, NewState};
        {value, Tuple, NewTupleList} ->
            NewState = State#state{list = NewTupleList, counter = increaseCounter(Counter)},
            {reply, {ok, NewState#state.counter, Tuple}, NewState}
    end;
handle_call(
    {find, Key} = _Msg, _From, #state{list = List, counter = Counter} = State
) ->
    NewState = State#state{list = List, counter = increaseCounter(Counter)},
    {reply, {ok, NewState#state.counter, lists:keyfind(Key, 1, List)}, NewState};
handle_call(
    {delete, Key} = _Msg, _From, #state{list = List, counter = Counter} = State
) ->
    NewState = State#state{
        list = lists:keydelete(Key, 1, List), counter = increaseCounter(Counter)
    },
    {reply, {ok, NewState#state.counter}, NewState};
handle_call(stop, _From, State) ->
    {stop, normal, shutdown_ok, State}.

% Info
handle_info({added_new_child, _Pid, _Name} = Msg, State) ->
    io:format("Received message ~p~n", [Msg]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

increaseCounter(Counter) when is_integer(Counter) -> Counter + 1.

% c(keylist).
% keylist:start_link(linked).
% keylist:start(monitored).

% keylist:add(linked, "first_key", "first_value", "first_comment").
% keylist:add(linked, "second_key", "second_value", "second_comment").
% keylist:add(linked, "third_key", "third_value", "third_comment").

% keylist:is_member(linked, "nonexistent_key").
% keylist:is_member(linked, "first_key").

% keylist:take(linked, "nonexistent_key").
% keylist:take(linked, "second_key").

% keylist:find(linked, "nonexistent_key").
% keylist:find(linked, "first_key").

% keylist:delete(linked, "nonexistent_key").
% keylist:delete(linked, "third_key").

% keylist:stop(linked).

% exit(whereis(linked), killed).
% exit(whereis(monitored), killed).
