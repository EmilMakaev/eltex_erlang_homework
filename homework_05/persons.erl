-module(persons).

-include("person.hrl").

-export([filter/2, all/2, any/2, update/2, get_average_age/1, updateAgeForAllWomen/0]).

% TASK_1

filter(Fun, Persons) -> lists:filter(Fun, Persons).

all(Fun, Persons) -> lists:all(Fun, Persons).

any(Fun, Persons) -> lists:any(Fun, Persons).

update(Fun, Persons) -> lists:map(Fun, Persons).

% ///
divideTwoNumbers(A, B) when A > 0, B > 0 -> A / B;
divideTwoNumbers(A, B) when A =:= 0; B =:= 0 -> ?DEFAULT_AVERAGE_AGE.

get_average_age([]) ->
    ?DEFAULT_AVERAGE_AGE;
get_average_age(Persons) ->
    {AgeSum, PersonsCount} = lists:foldl(
        fun(#person{age = Age}, {AgeAcc, PersonsAcc}) -> {AgeAcc + Age, PersonsAcc + 1} end,
        {0, 0},
        Persons
    ),

    divideTwoNumbers(AgeSum, PersonsCount).
% ///

updateAgeForAllWomen() ->
    Persons = [
        #person{id = 1, name = "Jack", age = 20, gender = male},
        #person{id = 2, name = "Ana", age = 25, gender = female},
        #person{id = 3, name = "Sveta", age = 30, gender = female},
        #person{id = 4, name = "Sergey", age = 35, gender = male}
    ],

    Fun = fun
        (#person{gender = ?FEMALE, age = Age} = Person) ->
            Person#person{age = Age - 1};
        (Person) ->
            Person
    end,

    update(Fun, Persons).

% c(persons).
% rr("person.hrl").
% Persons = [
%     #person{id = 1, name = "Jack", age = 20, gender = male},
%     #person{id = 2, name = "Ana", age = 25, gender = female},
%     #person{id = 3, name = "Sveta", age = 30, gender = female},
%     #person{id = 4, name = "Sergey", age = 35, gender = male}
% ].

% persons:filter(fun(#person{age = Age}) -> Age >= 30 end, Persons).
% persons:filter(fun(#person{gender = Gender}) -> Gender =:= male end, Persons).
% persons:any(fun(#person{gender = Gender}) -> Gender =:= female end, Persons).
% persons:all(fun(#person{age = Age}) -> Age >= 20 end, Persons).
% persons:all(fun(#person{age = Age}) -> Age =< 30 end, Persons).

% UpdateJackAge = fun(#person{name = "Jack", age = Age} = Person) -> Person#person{age = Age + 1}; (Person) -> Person end.
% persons:update(UpdateJackAge, Persons).

% ////////////////////////////////////////////////////////////////////////////////////////

% TASK_2
% [X || X <- lists:seq(1, 10), X rem 3 =:= 0].

% List = [1, "hello", 100, boo, "boo", 9].
% [round(math:pow(X, 2)) || X <- List, is_number(X)].
