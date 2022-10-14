-module(start).
-export([run/3]).
-import(gossip,[full/1, line/1, twod/1, i3D/1]).
-import(pushsum,[fullp/1, linep/1, twodp/1, i3Dp/1]).

run(NumNodes, Topology, Algo) ->
    % io:fwrite("~..0B~n",[NumNodes]),
    case Algo of
        "gossip" ->
            case Topology of
                "full" -> gossip:full(NumNodes);
                "2D" -> gossip:twod(NumNodes);
                "line" -> gossip:line(NumNodes);
                "i3D" ->  gossip:i3D(NumNodes)
            end;
        "pushsum" ->
            case Topology of
                "full" -> pushsum:fullp(NumNodes);
                "2D" -> pushsum:twodp(NumNodes);
                "line" -> pushsum:linep(NumNodes);
                "i3D" ->  pushsum:i3Dp(NumNodes)
            end
    end.
