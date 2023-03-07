-module(recursion).

-export([tail_fac/1, duplicate/1, tail_duplicate/1]).

% Task_3.1
tail_fac(N) when N =:= 0 -> N;
tail_fac(N) when N > 0 -> tail_fac(N, 1).

tail_fac(0, Acc) -> Acc;
tail_fac(N, Acc) when N > 0 -> tail_fac(N - 1, N * Acc).

% c(recursion).
% recursion:tail_fac(5).

% /////////////////////////////////////////////////

% Task_3.2
duplicate([]) -> [];
% duplicate([Head | Tail]) -> [Head, Head | duplicate(Tail)].
duplicate([Head | Tail]) -> [Head, Head] ++ duplicate(Tail).

tail_duplicate([]) -> [];
tail_duplicate(List) -> tail_duplicate(List, []).

tail_duplicate([], Acc) -> lists:reverse(Acc);
tail_duplicate([Head | Tail], Acc) -> tail_duplicate(Tail, [Head, Head | Acc]).

% c(recursion).
% recursion:duplicate([1, 2, 3]).
% recursion:tail_duplicate([1, 2, 3]).

% /////////////////////////////////////////////////

% Task_4
% Fac = fun recursion:tail_fac/1.
% Fac(5).

% /////////////////////////////////////////////////

% Task_5.1
% MultiplyFunction = fun(A, B) -> A * B end.
% MultiplyFunction(2, 3).

% /////////////////////////////////////////////////

% Task_5.2
% ToRub = fun
%     ({usd, Amount}) when is_integer(Amount), Amount > 0 -> {ok, Amount * 75.5};
%     ({euro, Amount}) when is_integer(Amount), Amount > 0 -> {ok, Amount * 80};
%     ({lari, Amount}) when is_integer(Amount), Amount > 0 -> {ok, Amount * 29};
%     ({peso, Amount}) when is_integer(Amount), Amount > 0 -> {ok, Amount * 3};
%     ({krone, Amount}) when is_integer(Amount), Amount > 0 -> {ok, Amount * 10};
%     (Error) -> {error, badarg}
% end.
% ToRub({usd, 100}).
% ToRub({peso, 12}).
% ToRub({yene, 30}).
% ToRub({euro, -15}).
