-module(websocket_request).

-export([get/1,get_data/1,send/2]).


get(Socket) ->
    Socket.

get_data(Socket) ->
    case gen_tcp:recv(Socket, 0) of
	{ok,Data} ->
	    binary_to_list(Data);
        Other ->
	    exit(normal)
    end.

send(Data, Socket) ->
    gen_tcp:send(Socket, [0] ++ Data ++ [255]).

