-module(converter_test).

-include("currency.hrl").

-include_lib("eunit/include/eunit.hrl").

converter_test_() ->
    [
        ?_assertMatch(
            {ok, 7549.0},
            converter:rec_to_rub(#conv_info{type = usd, amount = 100, commission = 0.01})
        ),
        ?_assertMatch(
            {ok, 35.76},
            converter:rec_to_rub(#conv_info{type = peso, amount = 12, commission = 0.02})
        ),
        ?_assertMatch(
            {error, badarg},
            converter:rec_to_rub(#conv_info{type = yene, amount = 30, commission = 0.02})
        ),
        ?_assertMatch(
            {error, badarg},
            converter:rec_to_rub(#conv_info{type = euro, amount = -15, commission = 0.02})
        ),
        ?_assertMatch(
            {ok, 7549.0},
            converter:map_to_rub(#{type => usd, amount => 100, commission => 0.01})
        ),
        ?_assertMatch(
            {ok, 35.76},
            converter:map_to_rub(#{type => peso, amount => 12, commission => 0.02})
        ),
        ?_assertMatch(
            {error, badarg},
            converter:map_to_rub(#{type => yene, amount => 30, commission => 0.02})
        ),
        ?_assertMatch(
            {error, badarg},
            converter:map_to_rub(#{type => euro, amount => -15, commission => 0.02})
        )
    ].

% c(converter_test).
% converter_test:test().

% converter: map_to_rub(#{type => usd, amount => 100, commission => 0.01}).
% converter: map_to_rub(#{type => peso, amount => 12, commission => 0.02}).
% converter: map_to_rub(#{type => yene, amount => 30, commission => 0.02}).
% converter: map_to_rub(#{type => euro, amount => -15, commission => 0.02}).
