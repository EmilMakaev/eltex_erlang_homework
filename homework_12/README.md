## Протестируйте работу модулей в Eshell.

# тест

1> c(keylist).
{ok,keylist}
2> keylist:start_link(linked).
{ok,<0.88.0>}
3> keylist:add(linked, "first_key", "first_value", "first_comment").
{ok,{state,[],1}}
4> keylist:add(linked, "second_key", "second_value", "second_comment").
{ok,{state,[{"first_key","first_value","first_comment"}],2}}
5> keylist:add(linked, "third_key", "third_value", "third_comment").
{ok,{state,[{"second_key","second_value","second_comment"},
{"first_key","first_value","first_comment"}],
3}}
6> keylist:is_member(linked, "nonexistent_key").
{ok,4,false}
7> keylist:is_member(linked, "first_key").
{ok,5,true}
8> keylist:take(linked, "nonexistent_key").
{ok,6,false}
9> keylist:take(linked, "second_key").
{ok,7,{"second_key","second_value","second_comment"}}
10> keylist:find(linked, "nonexistent_key").
{ok,8,false}
11> keylist:find(linked, "first_key").
{ok,9,{"first_key","first_value","first_comment"}}
12> keylist:delete(linked, "nonexistent_key").
{ok,10}
13> keylist:delete(linked, "third_key").
{ok,11}
14> keylist:stop(linked).
shutdown_ok
15>

## Протестируйте завершение процессов через API stop и exit/2.

3> keylist_mgr:start().
{ok,<0.93.0>,#Ref<0.2571964258.2665218051.197234>}
4> keylist_mgr:start_child(#{name => keylist1, restart => permanent}).
{<0.81.0>,start_child,
#{name => keylist1,restart => permanent}}
5> keylist_mgr:start_child(#{name => keylist2, restart => temporary}).
Received message {added_new_child,<0.97.0>,keylist2}
{<0.81.0>,start_child,
#{name => keylist2,restart => temporary}}
6> keylist_mgr:stop().
stop
7> whereis(keylist1)
7> .
undefined

5> keylist_mgr:start().
{ok,<0.95.0>,#Ref<0.2109189295.2397306882.153644>}
6> keylist_mgr:start_child(#{name => keylist1, restart => permanent}).
{<0.81.0>,start_child,
#{name => keylist1,restart => permanent}}
7> keylist_mgr:start_child(#{name => keylist2, restart => temporary}).
Received message {added_new_child,<0.99.0>,keylist2}
{<0.81.0>,start_child,
#{name => keylist2,restart => temporary}}
8> self().
<0.81.0>
9> whereis(keylist_mgr).
<0.95.0>
10> whereis(keylist2).
<0.99.0>
11> whereis(keylist1).
<0.97.0>
12> exit(whereis(keylist2), killed).
Pid <0.99.0> ended due to killed
true
13> whereis(keylist_mgr).  
<0.95.0>
14> whereis(keylist2).  
undefined
15> exit(whereis(keylist_mgr), kill).
true
16> whereis(keylist_mgr).  
undefined
17> whereis(keylist1).  
undefined
18> self().  
<0.81.0>
