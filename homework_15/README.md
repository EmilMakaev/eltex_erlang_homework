1> c(door).
{ok,door}
2> door:start([1, 2, 3, 4]).
Init callback was called [1,2,3,4]
{ok,<0.88.0>}
3> door:enter(1).
Received Msg {enter,1} LoopData {data,[1,2,3,4],[]}
{ok,next}
4> door:enter(2).
Received Msg {enter,2} LoopData {data,[1,2,3,4],[1]}
{ok,next}
5> door:enter(3).
Received Msg {enter,3} LoopData {data,[1,2,3,4],[2,1]}
{ok,next}
6> door:enter(4).
Received Msg {enter,4} LoopData {data,[1,2,3,4],[3,2,1]}
{ok,opened}
7> door:enter(4).
Ignore number 4
{error,already_open}
8>

=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-==-=-=-=-=-==-=-=-=-=-==-=-=-=-=-==-=-=-=-=-==-=-=-=-=-==-=-=-=-=-=
