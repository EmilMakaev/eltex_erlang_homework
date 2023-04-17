%%%-----------------------------------------------------------------------------
%%% @doc This module is used in the keylist_mgr module. The module creates a spawn
%%% or spawn_link and allows you to state as a list with Key, Value, Comment.
%%% Each change in the module, in loop we increase the counter.
%%% @end
%%%-----------------------------------------------------------------------------
-module(door).

-record(data, {
    code,
    entered = []
}).
% locked | open

% API
-export([start/1, stop/0, enter/1, print_entered/0]).

% Callback
-export([init/1, callback_mode/0, locked/3, open/3, terminate/3]).

start(InitCode) ->
    gen_statem:start({local, ?MODULE}, ?MODULE, [InitCode], []).

stop() ->
    gen_statem:stop(?MODULE).

enter(Num) ->
    gen_statem:call(?MODULE, {enter, Num}).

print_entered() ->
    gen_statem:cast(?MODULE, print_entered).

init([InitCode]) ->
    io:format("Init callback was called ~p ~n", [InitCode]),
    {ok, locked, #data{code = InitCode}}.

callback_mode() ->
    [state_functions, state_enter].

% locked({call, From}, {enter, _Num} = Msg, Data) ->
%     io:format("Received Msg ~p LoopData was called ~p ~n", [Msg, Data]),
%     {keep_state_and_data, [{reply, From, ok}]}.

locked(enter, State, Data) ->
    io:format("Received Msg ~p State ~p LoopData ~p ~n", [enter, State, Data]),
    keep_state_and_data;
locked({call, From}, {enter, Num} = Msg, #data{entered = Entered, code = Code} = Data) ->
    io:format("Received Msg ~p LoopData ~p ~n", [Msg, Data]),
    NewEntered = [Num | Entered],

    case length(NewEntered) == length(Code) of
        false ->
            NewData = Data#data{entered = NewEntered},
            {keep_state, NewData, [{reply, From, {ok, next}}]};
        true ->
            case Code == lists:reverse(NewEntered) of
                false ->
                    {keep_state, Data#data{entered = []}, [{reply, From, {error, wrong_code}}]};
                true ->
                    {next_state, open, Data#data{entered = []}, [
                        {reply, From, {ok, opened}}, {state_timeout, 5000, open_timeout}
                    ]}
            end
    end;
locked(cast, print_entered, #data{entered = Entered}) ->
    io:format("Entered ~p ~n", [Entered]),
    keep_state_and_data;
locked(info, Msg, _Data) ->
    io:format("Received ~p ~n", [Msg]),
    keep_state_and_data.

open(enter, OldState, Data) ->
    io:format("Receiving Msg ~p State ~p LoopData ~p ~n", [enter, OldState, Data]),
    keep_state_and_data;
open(state_timeout, open_timeout, Data) ->
    io:format("Timeout, the door will be locked ~n"),
    {next_state, locked, Data};
open({call, From}, {enter, Num}, _Data) ->
    io:format("Ignore number ~p ~n", [Num]),
    {keep_state_and_data, [{reply, From, {error, already_open}}]}.

% suspended() -> ok.

terminate(Reason, State, Data) ->
    io:format("Terminating reason ~p state ~p data ~p ~n", [Reason, State, Data]),
    ok.

% c(door).
% door:start([1, 2, 3, 4]).

% door:enter(1).
% door:enter(2).
% door:enter(3).
% door:enter(4).

% c(door). door:start([1, 2, 3, 4]). door:enter(1). door:enter(2). door:enter(3). door:enter(4).
% door:print_entered().

% c(door). door:start([1, 2, 3, 4]). door:enter(3). door:enter(3). door:enter(3). door:stop().
