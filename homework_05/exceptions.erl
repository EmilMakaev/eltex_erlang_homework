-module(exceptions).

-export([catch_all/1]).

catch_all(Action) when is_function(Action, 0) ->
    try Action() of
        Result -> {ok, Result}
    catch
        throw:Reason ->
            io:format("Action ~p failed, reason ~p ~n", [Action, Reason]),
            throw;
        error:Reason ->
            io:format("Action ~p failed, reason ~p ~n", [Action, Reason]),
            error;
        exit:Reason ->
            io:format("Action ~p failed, reason ~p ~n", [Action, Reason]),
            exit;
        _:_ ->
            io:format("We covered all cases so this line will never be printed~n"),
            "Never will happen"
    end.

% c(exceptions).
% exceptions:catch_all(fun() -> 1 / 0 end). // We get (error:Reason) because exception error division by 0
% exceptions:catch_all(fun() -> throw(custom_exceptions) end). // We get (throw:Reason) because type of error Throw
% exceptions:catch_all(fun() -> exit(killed) end). // We get (exit:Reason) because type of error Exit
% exceptions:catch_all(fun() -> erlang:error(runtime_exception) end). // We get (error:Reason) because a run-time error or generated error occurs in Erlang
