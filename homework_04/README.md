## Task_1

1> c(converter).
{ok,converter}
2> rr("currency.hrl").
[conv_info]
3> converter:rec_to_rub(#conv_info{type = usd, amount = 100, commission = 0.01}).
{ok,7549.0}
4> converter:rec_to_rub(#conv_info{type = peso, amount = 12, commission = 0.02}).
{ok,35.76}
5> converter:rec_to_rub(#conv_info{type = yene, amount = 30, commission = 0.02}).
Can’t convert to rub, error {conv_info,yene,30,0.02}
{error,badarg}
6> converter:rec_to_rub(#conv_info{type = euro, amount = -15, commission = 0.02}).
Can’t convert to rub, error {conv_info,euro,-15,0.02}
{error,badarg}

## Task_2

1> c(converter).
{ok,converter}
2> converter: map_to_rub(#{type => usd, amount => 100, commission => 0.01}).
{ok,7549.0}
3> converter: map_to_rub(#{type => peso, amount => 12, commission => 0.02}).
{ok,35.76}
4> converter: map_to_rub(#{type => yene, amount => 30, commission => 0.02}).
Can’t convert to rub, error #{amount => 30,commission => 0.02,type => yene}
{error,badarg}
5> converter: map_to_rub(#{type => euro, amount => -15, commission => 0.02}).
Can’t convert to rub, error #{amount => -15,commission => 0.02,type => euro}
{error,badarg}

## Task_4

7> Fac = fun recursion:tail_fac/1.
fun recursion:tail_fac/1
8> Fac(5).
120

## Task_5.1

10> MultiplyFunction = fun(A, B) -> A \* B end.
#Fun<erl_eval.41.3316493>
11> MultiplyFunction(2, 3).
6

## Task_5.2

1> ToRub = fun
1> ({usd, Amount}) when is*integer(Amount), Amount > 0 -> {ok, Amount * 75.5};
1> ({euro, Amount}) when is*integer(Amount), Amount > 0 -> {ok, Amount * 80};
1> ({lari, Amount}) when is*integer(Amount), Amount > 0 -> {ok, Amount * 29};
1> ({peso, Amount}) when is*integer(Amount), Amount > 0 -> {ok, Amount * 3};
1> ({krone, Amount}) when is_integer(Amount), Amount > 0 -> {ok, Amount \* 10};
1> (Error) -> {error, badarg}
1> end.
#Fun<erl_eval.42.3316493>
2> ToRub({usd, 100}).
{ok,7550.0}
3> ToRub({peso, 12}).
{ok,36}
4> ToRub({yene, 30}).
{error,badarg}
5> ToRub({euro, -15}).
{error,badarg}
