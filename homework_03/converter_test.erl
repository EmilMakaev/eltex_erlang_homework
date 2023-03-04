-module(converter_test).

-include_lib("eunit/include/eunit.hrl").

converter_test() ->
    {ok, 7550.0} = converter:to_rub({usd, 100}).

converter_test_() ->
    [
        ?_assertMatch({ok, 7550.0}, converter:to_rub({usd, 100})),
        ?_assertMatch({ok, 36}, converter:to_rub({peso, 12})),
        ?_assertMatch({error, badarg}, converter:to_rub({yene, 30})),
        ?_assertMatch({error, badarg}, converter:to_rub({euro, -15}))
    ].

% c(converter_test).
% converter_test:test().
