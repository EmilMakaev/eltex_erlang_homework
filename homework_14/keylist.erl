%%%-----------------------------------------------------------------------------
%%% @doc This module is used in the keylist_mgr module. The module creates a spawn
%%% or spawn_link and allows you to state as a list with Key, Value, Comment.
%%% Each change in the module, in loop we increase the counter.
%%% @end
%%%-----------------------------------------------------------------------------
-module(keylist).

-behaviour(gen_server).

-record(state, {
    list = [],
    counter = 0
}).

-record(rec_etc_state, {
    key,
    value,
    comment
}).

-define(KEY_LIST_ETS, keylist_ets).

% API
-export([start/1, start_link/1, stop/1, add/4, is_member/2, take/2, find/2, delete/2]).
-export([match/2, match_object/2, select/2]).

% Callback
-export([init/1, handle_call/3, handle_info/2]).

-type key() :: {string(), string(), string()}.

-spec start(Name :: atom()) -> {ok, pid()}.
start(Name) ->
    gen_server:start({local, Name}, ?MODULE, [], []).

-spec start_link(Name :: atom()) -> {ok, pid()}.
start_link(Name) ->
    ets:new(?KEY_LIST_ETS, [public, set, named_table, {keypos, #rec_etc_state.key}]),
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

-spec match(Name :: atom(), Pattern :: ets:match_pattern()) -> {ok, list()}.
match(Name, Pattern) -> gen_server:call(Name, {match, Pattern}).

-spec match_object(Name :: atom(), Pattern :: ets:match_pattern()) -> {ok, list()}.
match_object(Name, Pattern) -> gen_server:call(Name, {match_object, Pattern}).

-spec select(Name :: atom(), Filter :: fun()) -> {ok, list()}.
select(Name, Filter) -> gen_server:call(Name, {select, Filter}).

% Callback
init(_Args) ->
    {ok, #state{}}.

% Call
handle_call({add, Key, Value, Comment} = _Msg, _From, State) ->
    ets:insert(?KEY_LIST_ETS, #rec_etc_state{key = Key, value = Value, comment = Comment}),
    {reply, ok, State};
handle_call({is_member, Key} = _Msg, _From, State) ->
    IsMember =
        case ets:lookup(?KEY_LIST_ETS, Key) of
            [] -> false;
            _ -> true
        end,
    {reply, {ok, IsMember}, State};
handle_call({take, Key} = _Msg, _From, State) ->
    Element = ets:take(?KEY_LIST_ETS, Key),
    case Element of
        [] ->
            {reply, {ok, false}, State};
        _ ->
            {reply, {ok, Element}, State}
    end;
handle_call({find, Key} = _Msg, _From, State) ->
    {reply, {ok, ets:lookup(?KEY_LIST_ETS, Key)}, State};
handle_call({delete, Key} = _Msg, _From, State) ->
    {reply, {ok, ets:delete(?KEY_LIST_ETS, Key)}, State};
handle_call(stop, _From, State) ->
    {stop, normal, shutdown_ok, State};
handle_call({match, Pattern}, _From, State) ->
    {reply, {ok, lists:append(ets:match(?KEY_LIST_ETS, Pattern))}, State};
handle_call({match_object, Pattern}, _From, State) ->
    {reply, {ok, ets:match_object(?KEY_LIST_ETS, Pattern)}, State};
handle_call({select, Filter}, _From, State) ->
    SelectMsFun = ets:fun2ms(Filter),
    {reply, {ok, lists:append(ets:select(?KEY_LIST_ETS, SelectMsFun))}, State}.

handle_cast(Msg, State) ->
    io:format("Received unexpected cast msg ~p ~n", [Msg]),
    {noreply, State}.

% Info
handle_info({added_new_child, _Pid, _Name} = Msg, State) ->
    io:format("Received message ~p~n", [Msg]),
    {noreply, State}.

% c(keylist).
% keylist:start_link(linked).
% keylist:start(monitored).

% c(keylist). keylist:start_link(linked). keylist:add(linked, "first_key", 2, "first_comment"). keylist:add(linked, "second_key", 2, "second_comment"). keylist:add(linked, "third_key", 3, "third_comment"). keylist:add(linked, "fourth_key", 4, "fourth_comment").

% keylist:match(linked, {'_', '$1', 2, '$2'}).
% keylist:match(linked, {'_', '_', 4, '$1'}).
% keylist:match_object(linked, {'_', '$1', 2, '$2'}).
% keylist:match_object(linked, {'_', '$1', 3, "third_comment"}).
% keylist:select(linked, fun({_, Key, Value, Comment}) when Value =/= 3, Comment =/= "first_comment" -> [Comment] end).
% keylist:select(linked, fun({_, Key, Value, Comment}) when Value =:= 4 -> [Comment] end).


% exit(whereis(linked), killed).
% exit(whereis(monitored), killed).
