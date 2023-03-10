## TASK_1

46> persons:filter(fun(#person{age = Age}) -> Age >= 30 end, Persons).
[#person{id = 3,name = "Sveta",age = 30,gender = female},
#person{id = 4,name = "Sergey",age = 35,gender = male}]
47>

47> persons:filter(fun(#person{gender = Gender}) -> Gender =:= male end, Persons).
[#person{id = 1,name = "Ivan",age = 20,gender = male},
#person{id = 4,name = "Sergey",age = 35,gender = male}]
48>

48> persons:any(fun(#person{gender = Gender}) -> Gender =:= female end, Persons).
true

50> persons:all(fun(#person{age = Age}) -> Age >= 20 end, Persons).
true

51> persons:all(fun(#person{age = Age}) -> Age =< 30 end, Persons).
false

3> persons:updateAgeForAllWomen().
[#person{id = 1,name = "Jack",age = 20,gender = male},
#person{id = 2,name = "Ana",age = 24,gender = female},
#person{id = 3,name = "Sveta",age = 29,gender = female},
#person{id = 4,name = "Sergey",age = 35,gender = male}]
4>

## TASK_2

38> [X || X <- lists:seq(1, 10), X rem 3 =:= 0].
[3,6,9]

41> List = [1, "hello", 100, boo, "boo", 9].
[1,"hello",100,boo,"boo",9]
42> [round(math:pow(X, 2)) || X <- List, is_number(X)].
[1,10000,81]
43>

## Task_3

% exceptions:catch_all(fun() -> 1 / 0 end). // We get (error:Reason) because exception error division by 0
% exceptions:catch_all(fun() -> throw(custom_exceptions) end). // We get (throw:Reason) because type of error Throw
% exceptions:catch_all(fun() -> exit(killed) end). // We get (exit:Reason) because type of error Exit
% exceptions:catch_all(fun() -> erlang:error(runtime_exception) end). // We get (error:Reason) because a run-time error or generated error occurs in Erlang
