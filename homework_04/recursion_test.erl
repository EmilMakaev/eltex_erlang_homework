-module(recursion_test).

-include_lib("eunit/include/eunit.hrl").

new_add_test_() ->
    [
        ?_assertEqual(0, recursion:tail_fac(0)),
        ?_assertEqual(6, recursion:tail_fac(3)),
        ?_assertEqual(120, recursion:tail_fac(5)),
        ?_assertEqual([], recursion:duplicate([])),
        ?_assertEqual([1, 1, 2, 2, 3, 3], recursion:duplicate([1, 2, 3])),
        ?_assertEqual([], recursion:tail_duplicate([])),
        ?_assertEqual([1, 1, 2, 2, 3, 3], recursion:tail_duplicate([1, 2, 3]))
    ].

% c(recursion_test).
% recursion_test:test().
