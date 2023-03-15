## TASK_1

1> c(protocol).
{ok,protocol}
2> DataWrongFormat = <<4:4, 6:4, 0:8, 0:3>>.
<<70,0,0:3>>
3> Data1 = <<4:4, 6:4, 0:8, 232:16, 0:16, 0:3, 0:13, 0:8, 0:8, 0:16, 0:32, 0:32, 0:32, "hello" >>.
<<70,0,0,232,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,104,
101,108,108,111>>
4> spawn(protocol, ipv4, [Data1]).
Received data <<"hello">>
<0.91.0>
5> self().
<0.81.0>
6> spawn(protocol, ipv4, [DataWrongFormat]).
<0.94.0>
=ERROR REPORT==== 15-Mar-2023::20:01:56.682299 ===
Error in process <0.94.0> with exit value:
{badarg,[{protocol,ipv4,1,[{file,"protocol.erl"},{line,32}]}]}

7> self().  
<0.81.0>
8>

% Using erlang:error because it is a (Run-time errors).
% I found this information on the official website https://www.erlang.org/doc/reference_manual/errors.html
% I do not have an exact understanding of when and where to apply (exit, throw or error).

% error процесс остался прежним
% throw меняется процесс.
% exit не меняется процесс, регистрируется вызов spawn и больше ничего не происходит.

% Использую error процесс остался прежним, но если посмотреть на официальном сайте, то там написано

<!-- https://www.erlang.org/doc/reference_manual/errors.html -->

% error(Reason). Run-time errors are exceptions of class error.
% When an exception occurs in Erlang, execution of the process that evaluated the erroneous expression
% is stopped. This is referred to as a failure, that execution or evaluation fails, or that the process fails, terminates, or exits.

-=-=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-

11> spawn(fun() -> spawn(protocol, ipv4, [Data1]) end).
Received data <<"hello">>
<0.97.0>
12>

## TASK_2

1> c(protocol).
{ok,protocol}
2> ListenerPid = spawn(protocol, ipv4_listener, []).
<0.89.0>
3> Data1 = <<4:4, 6:4, 0:8, 232:16, 0:16, 0:3, 0:13, 0:8, 0:8, 0:16, 0:32, 0:32, 0:32, "hello" >>.
<<70,0,0,232,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,104,
101,108,108,111>>
4> ListenerPid ! {ipv4, self(), Data1}.
Received data <<"hello">>
{ipv4,<0.81.0>,
<<70,0,0,232,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,104,
101,...>>}
5> flush().
Shell got {ipv4,4,6,0,232,0,0,0,0,0,0,0,0,<<0,0,0,0>>,<<"hello">>}
ok
6>

-=-=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-

1> c(protocol).
{ok,protocol}
2> Data1 = <<4:4, 6:4, 0:8, 232:16, 0:16, 0:3, 0:13, 0:8, 0:8, 0:16, 0:32, 0:32, 0:32, "hello" >>.
<<70,0,0,232,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,104,
101,108,108,111>>
3> ListenerPid = spawn(protocol, ipv4_listener, []).
<0.91.0>
4> ListenerPid ! {ipv4, Data1}.
{ipv4,<<70,0,0,232,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,104,101,108,...>>}
6> =ERROR REPORT==== 15-Mar-2023::22:17:32.184674 ===
Error in process <0.91.0> with exit value:
{badarg,[{protocol,ipv4,1,[{file,"protocol.erl"},{line,32}]}]}

6> flush().
ok
