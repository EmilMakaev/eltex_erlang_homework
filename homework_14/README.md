3> keylist:add(linked, "first_key", 2, "first_comment"). keylist:add(linked, "second_key", 2, "second_comment").
keylist:add(linked, "third_key", 3, "third_comment"). keylist:add(linked, "fourth_key", 4, "fourth_comment").
ok
4> keylist:add(linked, "second_key", 2, "second_comment"). keylist:add(linked, "third_key", 3, "third_comment").
keylist:add(linked, "fourth_key", 4, "fourth_comment").
ok
5> keylist:add(linked, "third_key", 3, "third_comment"). keylist:add(linked, "fourth_key", 4, "fourth_comment").
ok
6> keylist:add(linked, "fourth_key", 4, "fourth_comment").
ok
7> keylist:match(linked, {'_', '$1', 2, '$2'}).
{ok,["first_key","first_comment","second_key",
"second_comment"]}
8> keylist:match(linked, {'_', '_', 4, '$1'}).
{ok,["fourth_comment"]}
9> keylist:match_object(linked, {'_', '$1', 2, '$2'}).
{ok,[{rec_etc_state,"first_key",2,"first_comment"},
{rec_etc_state,"second_key",2,"second_comment"}]}
10> keylist:match_object(linked, {'_', '$1', 3, "third_comment"}).
{ok,[{rec_etc_state,"third_key",3,"third_comment"}]}
11> keylist:select(linked, fun({_, Key, Value, Comment}) when Value =/= 3, Comment =/= "first_comment" -> [Comment] end)
.
{ok,["fourth_comment","second_comment"]}
12> keylist:select(linked, fun({_, Key, Value, Comment}) when Value =:= 4 -> [Comment] end).
{ok,["fourth_comment"]}
13> 