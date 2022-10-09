-module(start).
-export([run/3]).
-import(gossip,[full/1, line/1]).
-import(pushsum,[fullp/1, linep/1]).

run(NumNodes, Topology, Algo) ->
    % io:fwrite("~..0B~n",[NumNodes]),
    case Algo of
        "gossip" ->
            case Topology of
                "full" -> gossip:full(NumNodes);
                "2D" -> io:format("g - 2D");
                "line" -> gossip:line(NumNodes);
                "i3D" ->  io:format("g - line")
            end;
        "pushsum" -> 
            case Topology of
                "full" -> pushsum:fullp(NumNodes);
                "2D" -> io:format("p - 2D");
                "line" -> pushsum:linep(NumNodes);
                "i3D" ->  io:format("p - line")
            end
    end.
