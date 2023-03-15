-module(protocol).

-include("protocol.hrl").

-export([ipv4/1, ipv4_listener/0]).

ipv4(
    <<Version:4, IHL:4, ToS:8, TotalLength:16, Identification:16, Flags:3, FragOffset:13,
        TimeToLive:8, Protocol:8, Checksum:16, SourceAddress:32, DestinationAddress:32,
        OptionsAndPadding:((IHL - 5) * 32)/bits, RemainingData/bytes>>
) when Version =:= 4 ->
    io:format("Received data ~p ~n", [RemainingData]),
    #ipv4{
        version = Version,
        ihl = IHL,
        tos = ToS,
        total_length = TotalLength,
        identification = Identification,
        flags = Flags,
        flag_offset = FragOffset,
        time_to_live = TimeToLive,
        protocol = Protocol,
        header_checksum = Checksum,
        source_address = SourceAddress,
        destination_address = DestinationAddress,
        options = OptionsAndPadding,
        data = RemainingData
    };
ipv4(Error) ->
    % exit(foobar).
    % throw({my_exception, "Something happened"}).
    error(badarg, Error).

ipv4_listener() ->
    receive
        {ipv4, From, BinData} when is_binary(BinData) ->
            From ! ipv4(BinData);
        Error ->
            ipv4(Error)
    end.

% c(protocol).
% DataWrongFormat = <<4:4, 6:4, 0:8, 0:3>>.
% DataWrongVer = <<6:4, 6:4, 0:8, 232:16, 0:16, 0:3, 0:13, 0:8, 0:8, 0:16, 0:32, 0:32, 0:32, "hello" >>.
% Data1 = <<4:4, 6:4, 0:8, 232:16, 0:16, 0:3, 0:13, 0:8, 0:8, 0:16, 0:32, 0:32, 0:32, "hello" >>.
% Data2 = <<4:4, 6:4, 0:8, 20:16, 0:16, 0:3, 0:13, 0:8, 0:8, 0:16, 0:32, 0:32, 0:32, "hello" >>.

% protocol:ipv4(DataWrongFormat). 

% spawn(protocol, ipv4, [Data1]).
% spawn(protocol, ipv4, [DataWrongFormat]).

% =-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
% c(protocol).
% ListenerPid = spawn(protocol, ipv4_listener, []).
% Data1 = <<4:4, 6:4, 0:8, 232:16, 0:16, 0:3, 0:13, 0:8, 0:8, 0:16, 0:32, 0:32, 0:32, "hello" >>.
% ListenerPid ! {ipv4, self(), Data1}.
% flush().

% ListenerPid ! {ipv4, Data1}.
