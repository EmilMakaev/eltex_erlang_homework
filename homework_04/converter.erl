-module(converter).

-include("currency.hrl").

-export([rec_to_rub/1, map_to_rub/1]).

% Task_1
rec_to_rub(#conv_info{type = usd, amount = Amount, commission = Commission}) when
    is_integer(Amount), Amount > 0
->
    Result = Amount * 75.5,
    CommissionResult = Amount * Commission,
    {ok, Result - CommissionResult};
rec_to_rub(#conv_info{type = Type, amount = Amount, commission = Commission}) when
    Type =:= euro, is_integer(Amount), Amount > 0
->
    Result = Amount * 80,
    CommissionResult = Amount * Commission,
    {ok, Result - CommissionResult};
rec_to_rub(#conv_info{type = Type, amount = Amount, commission = Commission}) when
    Type =:= lari, is_integer(Amount), Amount > 0
->
    Result = Amount * 29,
    CommissionResult = Amount * Commission,
    {ok, Result - CommissionResult};
rec_to_rub(#conv_info{type = peso, amount = Amount, commission = Commission}) when
    is_integer(Amount), Amount > 0
->
    Result = Amount * 3,
    CommissionResult = Amount * Commission,
    {ok, Result - CommissionResult};
rec_to_rub(#conv_info{type = krone, amount = Amount, commission = Commission}) when
    is_integer(Amount), Amount > 0
->
    Result = Amount * 10,
    CommissionResult = Amount * Commission,
    {ok, Result - CommissionResult};
rec_to_rub(Error) ->
    io:format("Can’t convert to rub, error ~p~n", [Error]),
    {error, badarg}.

% c(converter).
% rr("currency.hrl").
% converter:rec_to_rub(#conv_info{type = usd, amount = 100, commission = 0.01}). {ok,7549.0}
% converter:rec_to_rub(#conv_info{type = peso, amount = 12, commission = 0.02}). {ok,35.76}
% converter:rec_to_rub(#conv_info{type = yene, amount = 30, commission = 0.02}). {error,badarg}
% converter:rec_to_rub(#conv_info{type = euro, amount = -15, commission = 0.02}). {error,badarg}

% /////////////////////////////////////////////////

% Task_2
map_to_rub(#{type := usd, amount := Amount, commission := Commission}) when
    is_integer(Amount), Amount > 0
->
    Result = Amount * 75.5,
    CommissionResult = Amount * Commission,
    {ok, Result - CommissionResult};
map_to_rub(#{type := euro, amount := Amount, commission := Commission}) when
    is_integer(Amount), Amount > 0
->
    Result = Amount * 80,
    CommissionResult = Amount * Commission,
    {ok, Result - CommissionResult};
map_to_rub(#{type := lari, amount := Amount, commission := Commission}) when
    is_integer(Amount), Amount > 0
->
    Result = Amount * 29,
    CommissionResult = Amount * Commission,
    {ok, Result - CommissionResult};
map_to_rub(#{type := peso, amount := Amount, commission := Commission}) when
    is_integer(Amount), Amount > 0
->
    Result = Amount * 3,
    CommissionResult = Amount * Commission,
    {ok, Result - CommissionResult};
map_to_rub(#{type := krone, amount := Amount, commission := Commission}) when
    is_integer(Amount), Amount > 0
->
    Result = Amount * 10,
    CommissionResult = Amount * Commission,
    {ok, Result - CommissionResult};
map_to_rub(Error) ->
    io:format("Can’t convert to rub, error ~p~n", [Error]),
    {error, badarg}.

% c(converter).
% converter:map_to_rub(#{type => usd, amount => 100, commission => 0.01}).
% converter:map_to_rub(#{type => peso, amount => 12, commission => 0.02}).
% converter:map_to_rub(#{type => yene, amount => 30, commission => 0.02}).
% converter:map_to_rub(#{type => euro, amount => -15, commission => 0.02}).
