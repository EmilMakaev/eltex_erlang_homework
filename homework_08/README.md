## TASK_2

1> self().
<0.81.0>
2> c(keylist_mgr).
{ok,keylist_mgr}
3> c(keylist).
{ok,keylist}
4> process_flag(trap_exit, true).
false
5> process_flag(trap_exit, true).
true
6> keylist_mgr:start().
{ok,<0.100.0>,#Ref<0.3854547809.235143171.100524>}
7> keylist_mgr ! {self(), start_child, keylist1}.
{<0.98.0>,start_child,keylist1}
8> keylist_mgr ! {self(), start_child, keylist2}.
{<0.98.0>,start_child,keylist2}
9> keylist_mgr ! {self(), start_child, keylist3}.
{<0.98.0>,start_child,keylist3}
10> keylist3 ! {self(), add, "first_key", "first_value", "first_comment"}.
{<0.98.0>,add,"first_key","first_value","first_comment"}
11> flush().
Shell got {ok,<0.102.0>}
Shell got {ok,<0.104.0>}
Shell got {ok,<0.106.0>}
Shell got {ok,1}
ok
12>

## TASK_3

13> exit(whereis(keylist1), killed).
true
14> self().  
<0.81.0>
15> whereis(keylist_mgr).  
<0.98.0>
16> flush().
ok
17>

1. В случае с exit(whereis(keylist1), killed) мы убиваем процесс. Наш процесс в Eshell ничего не знает
   об этом, потому что мы мониторим только родительский процесс keylist_mgr
2. Текущий self() процесс остается прежним потому что используем process_flag(trap_exit, true).
3. убивая процесс таким образом exit(whereis(keylist1), killed). мы не меняем наш State

19> exit(whereis(keylist_mgr), killed).  
true
20> self().
<0.81.0>
21> flush().  
Shell got {'DOWN',#Ref<0.1737979434.3458990081.47782>,process,<0.98.0>,killed}
ok
22>

1. В данном случае мы получаем информацию о падении из-за spawn_monitor
