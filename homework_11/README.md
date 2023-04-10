string:tokens("a-b-c-d-e", "-").\
["a","b","c","d","e"]

string:tokens("e-m-i-l", "-").\
["e","m","i","l"]

string:tokens("abcdefghi jklmn", "bf l").\
["a","cde","ghi","jk","mn"]

string:join(["a", "b", "c", "d"], "-").\
"a-b-c-d"

string:strip("  abcdefg  ").\
"abcdefg"

string:strip("   abcdefg ", right).\
"   abcdefg"

string:to_upper("AbCdEfg").\      
"ABCDEFG"

string:to_lower("AbCdEfg").\
"abcdefg"

string:to_integer("1234abc").\
{1234,"abc"}

list_to_integer("123").\
123

byte_size(<<433:16, 3:3>>).\
3

byte_size(<<1, 2, 3, 4>>).\
4

split_binary(<<1,2,3,4,5>>,3).\
{<<1,2,3>>,<<4,5>>}

erlang:binary_part(<<1,2,3,4,5,6,7,8,9,10>>, 2, 5).\
<<3,4,5,6,7>>

binary:split(<<1,255,4,0,0,0,2,3>>, [<<0,0,0>>,<<2>>]).\
[<<1,255,4>>,<<2,3>>]

binary:split(<<"a,b,c,d">>, <<$,>>, [global, trim]).\
[<<"a">>,<<"b">>,<<"c">>,<<"d">>]

binary:match(<<"abcde">>, [<<"bcde">>, <<"cd">>]).\
{1,4}

binary:match(<<"abcde">>, [<<"bcdefw">>, <<"cd">>],[]).\
{2,2}

binary:replace(<<1,2,3>>, <<1>>, <<2>>).\
<<2,2,3>>

binary_to_list(<<"a", "b", "c", "d", "e">>).\
"abcde"

list_to_binary([<<1,2,3>>, 1, [2, 3, <<4,5>>], 4 | <<6>>]).\
<<1,2,3,1,2,3,4,5,4,6>>

lists:flatten([[1], [2], 3, [4, [5, 6, [7, 8]]]]).\
[1,2,3,4,5,6,7,8]

