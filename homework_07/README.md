## TASK_2

1> c(keylist).
{ok,keylist}
2> keylist:start_link(linked).
<0.89.0>
3> linked ! {self(), add, "first_key", "first_value", "first_comment"}.
{<0.81.0>,add,"first_key","first_value","first_comment"}
4> linked ! {self(), add, "second_key", "second_value", "second_comment"}.
{<0.81.0>,add,"second_key","second_value","second_comment"}
5> linked ! {self(), add, "third_key", "third_value", "third_comment"}.
{<0.81.0>,add,"third_key","third_value","third_comment"}
6> linked ! {self(), add, "fourth_key", "fourth_value", "fourth_comment"}.
{<0.81.0>,add,"fourth_key","fourth_value","fourth_comment"}
7> linked ! {self(), is_member, "nonexistent_key"}.
{<0.81.0>,is_member,"nonexistent_key"}
8> linked ! {self(), is_member, "first_key"}.
{<0.81.0>,is_member,"first_key"}
9> linked ! {self(), take, "second_key"}.
{<0.81.0>,take,"second_key"}
10> linked ! {self(), take, "nonexistent_key"}.
{<0.81.0>,take,"nonexistent_key"}
11> linked ! {self(), find, "third_key"}.
{<0.81.0>,find,"third_key"}
12> linked ! {self(), find, "nonexistent_key"}.
{<0.81.0>,find,"nonexistent_key"}
13> linked ! {self(), delete, "nonexistent_key"}.
{<0.81.0>,delete,"nonexistent_key"}
14> linked ! {self(), delete, "third_key"}.
{<0.81.0>,delete,"third_key"}
15> flush().
Shell got {ok,1}
Shell got {ok,2}
Shell got {ok,3}
Shell got {ok,4}
Shell got {ok,5,false}
Shell got {ok,6,true}
Shell got {ok,7,{"second_key","second_value","second_comment"}}
Shell got {ok,8,false}
Shell got {ok,9,{"third_key","third_value","third_comment"}}
Shell got {ok,10,false}
Shell got {ok,11}
Shell got {ok,12}
ok
16>

## TASK_3

1> c(keylist).
{ok,keylist}
2> keylist:start(monitored).
{ok,<0.89.0>,true}
3> self().
<0.81.0>
4> exit(whereis(monitored), killed).
true
5> flush().
Shell got {'DOWN',#Ref<0.4066768075.2041839624.192463>,process,<0.89.0>,
killed}
ok
6> self().
<0.81.0>
7>

Почтовый ящик содержит Ref с содержимым об упавшем связанном процессе. Наш Pid остался прежним,
потому что у односторонняя связь erlang:monitor(process, Pid)

-=-=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-

1> c(keylist).
{ok,keylist}
2> keylist:start_link(linked).
<0.89.0>
3> self().
<0.81.0>
4> exit(whereis(linked), killed).
\*\* exception exit: killed
5> flush().  
ok
6> self().
<0.92.0>
7>

Наш процесс упал и изменился Pid, потому что у нас двухсторонняя связь spawn_link

## TASK_4

1> c(keylist).
{ok,keylist}
2> keylist:start_link(linked).
<0.89.0>
3> self().
<0.81.0>
4> process_flag(trap_exit, true).
false
5> process_flag(trap_exit, true).
true
6> exit(whereis(linked), killed).
true
7> self().
<0.81.0>
8> flush().
Shell got {'EXIT',<0.89.0>,killed}
ok

Процесс получил сообщение, что связанный процесс <0.89.0> упал по причине killed.
Pid остался прежним из-за использования process_flag

process_flag используется для того что бы в момент когда наш процесс упал, он не потянул за собой остальные.
Например, если мы не хотим чтобы процесс (B) упал из-за того что его связанные процесс (A) сломался, то используем
process_flag и при падении процесс (A), процесс (B) останется жив.

## TASK_5

1> c(keylist).
{ok,keylist}
2> process_flag(trap_exit, false).
false
3> keylist:start_link(linked1).
<0.90.0>
4> keylist:start_link(linked2).
<0.92.0>
5> self().
<0.81.0>
6> exit(whereis(linked1), killed).
\*\* exception exit: killed
7> self().
<0.95.0>
8> whereis(linked2).
undefined
9>

Мы отключили process_flag и при создании процессов, все три процесса были связаны start_link.
Мы завершили процесс linked1 в следствии из-за их связи, они все три упали.

-=-=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-
