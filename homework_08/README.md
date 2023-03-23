## TASK_2

1> c(keylist_mgr).
{ok,keylist_mgr}
2> c(keylist).
{ok,keylist}
3> keylist_mgr:start().
{ok,<0.95.0>,#Ref<0.1585383003.3306946562.100781>}
4> keylist_mgr ! {self(), start_child, keylist1}.
{<0.81.0>,start_child,keylist1}
5> keylist_mgr ! {self(), start_child, keylist2}.
{<0.81.0>,start_child,keylist2}
6> keylist_mgr ! {self(), start_child, keylist3}.
{<0.81.0>,start_child,keylist3}
7> keylist3 ! {self(), add, "first_key", "first_value", "first_comment"}.
{<0.81.0>,add,"first_key","first_value","first_comment"}
8> flush().
Shell got {ok,<0.97.0>}
Shell got {ok,<0.99.0>}
Shell got {ok,<0.101.0>}
Shell got {ok,1}
ok
9>

## TASK_3

1> c(keylist_mgr).
{ok,keylist_mgr}
2> c(keylist).
{ok,keylist}
3> process_flag(trap_exit, true).
false
4> process_flag(trap_exit, true).
true
5> keylist_mgr:start().
{ok,<0.97.0>,#Ref<0.2113172461.2058092546.19442>}
6> keylist_mgr ! {self(), start_child, keylist1}.
{<0.81.0>,start_child,keylist1}
7> whereis(keylist_mgr).
<0.97.0>
8> self().
<0.81.0>
9> exit(whereis(keylist1), killed).
true
10> self().
<0.81.0>
11> flush().
Shell got {ok,<0.99.0>}
Shell got {'DOWN',#Ref<0.2113172461.2058092546.19442>,process,<0.97.0>,killed}
ok
12> whereis(keylist_mgr).
undefined
13>
