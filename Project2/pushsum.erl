-module(pushsum).
-export([create_actors/4, master/4, listeners/5, start_pushsum/1, kill_nodes/1, check_convergence/6, rumouring/2,rumouring_process/5]).
-export([fullp/1, calc_full_neighbors/4]).
-export([ linep/1, calc_line_neighbors/3]).

fullp(NumNodes) ->
    NeighborMap = calc_full_neighbors(NumNodes, maps:new(), 1, NumNodes),
    % io:format("neighbor map \n"),
    % io:format("~w", [NeighborMap]),
    %io:format("~w~n", [Neighbors]),
    %io:fwrite("~..0B~n",[lists:nth(2,Neighbors)]), % accessing 2nd element in list
    Master_PID = spawn(pushsum, create_actors, [NumNodes, [], 1, NeighborMap]), % spawn(modulename, funcname, args)
    register(master, Master_PID).

calc_full_neighbors(0, NeighborMap, _, _) -> NeighborMap;
calc_full_neighbors(NumNodes, NeighborMap, Cnt, N) ->
    IndexList = lists:filter(
        fun(X) -> X /= Cnt end,
    lists:seq(1,N)),
    NeighborMap1 = maps:put(Cnt, IndexList, NeighborMap),
    calc_full_neighbors(NumNodes-1, NeighborMap1, Cnt+1, N).

linep(NumNodes) -> 
    NeighborMap = calc_line_neighbors(NumNodes, maps:new(), 1),
    % io:format("neighbor map \n"),
    % io:format("~w", [NeighborMap]).
    Master_PID = spawn(pushsum, create_actors, [NumNodes, [], 1, NeighborMap]), % spawn(modulename, funcname, args)
    register(master, Master_PID).

calc_line_neighbors(0, NeighborMap, _) -> NeighborMap;

calc_line_neighbors(NumNodes, NeighborMap, Cnt) ->
    if
        Cnt==NumNodes ->
            NeighborMap1 = maps:put(Cnt, [Cnt-1], NeighborMap),
            calc_line_neighbors(NumNodes-1, NeighborMap1,  Cnt+1);
        Cnt==1 ->
            NeighborMap1 = maps:put(Cnt, [Cnt+1], NeighborMap),
            calc_line_neighbors(NumNodes-1, NeighborMap1,  Cnt+1);
        true ->
            NeighborMap1 = maps:put(Cnt, [Cnt-1,Cnt+1], NeighborMap),
            calc_line_neighbors(NumNodes-1, NeighborMap1,  Cnt+1)
    end.

create_actors(0, Nodes, _, NeighborMap) -> 
    ConvergedMap = maps:new(),
    T = erlang:timestamp(),
    start_pushsum(Nodes),
    master(Nodes, ConvergedMap, NeighborMap, T);

create_actors(NumNodes, Nodes, Counter, NeighborMap) ->
    PID = spawn(pushsum, listeners, [NeighborMap, 0, 1, Counter, 1]),
    register(list_to_atom(integer_to_list(Counter)), PID),
    create_actors(NumNodes-1, [{NumNodes,PID} | Nodes], Counter+1, NeighborMap).

master(Nodes, ConvergedMap, NeighborMap, T) ->
    receive
        {converge, Pid, S, W} ->
            % store converged nodes here
            ConvergedMap1 = maps:put(Pid, 1, ConvergedMap),
            % io:format("in map ~w~n", [ConvergedMap1]),
            check_convergence(Pid, ConvergedMap1, NeighborMap, Nodes, S, W), % of all nodes
            master(Nodes, ConvergedMap1, NeighborMap, T);
        converged ->
            % kill all if converged
            io:format("converged~n"),
            FTime = timer:now_diff(erlang:timestamp(), T)/1000,
            io:format("Convergence time: ~p ms\n", [FTime]),
            kill_nodes(Nodes),
            exit(self(), normal)
    end.

listeners(NeighborMap, Cnt, OldRatio, S, W) ->
    NewRatio = S/W,
    Diff = abs(OldRatio - NewRatio),
    % io:format("S W ~p ~p~n",[S, W]),
    Const = math:pow(10, -10),
    if
        Diff > Const ->
            Count = 0;
        true ->
            Count = Cnt + 1
    end,
    receive
        {Sr, Wr, Nodes} ->
            Pid = self(),
            % io:format("pid ~p~n ~w~n",[self(), Nodes]),
            if
                Count>=3 ->
                    master ! {converge, Pid, Sr+S/2, Wr+W/2};
                true ->
                    rumouring_process(Nodes, NeighborMap, Pid, S/2, W/2),
                    % io:format("~p ~p ~p ~p ~p ~p ~n",[NewRatio, OldRatio, Sr, Wr, S, W]),
                    listeners(NeighborMap, Count, NewRatio, Sr+S/2, Wr+W/2)
            end;
        die ->
            exit(self(), normal)
    end.

check_convergence(Pid, ConvergedMap, NeighborMap, Nodes, S, W) ->
    Converged = maps:keys(ConvergedMap),
    if 
        length(Nodes) == length(Converged) ->
            master ! converged;
        true -> 
            rumouring_process(Nodes, NeighborMap, Pid, S, W)
    end.

start_pushsum(Nodes) ->
    % pick a random index from nodes to start with
    RandIndex = rand:uniform(length(Nodes)),
    rumouring(RandIndex, Nodes).

rumouring_process(Nodes, NeighborMap, Pid, S, W) -> 
    Nodes1 = lists:map(
                    fun(T) ->
                        lists:nth(2,tuple_to_list(T))
                    end,
                    Nodes),
    Index = string:str(Nodes1, [Pid]),
    % io:format("Index ~p~n",[Index]),
    Indices = maps:get(Index, NeighborMap),
    RandIndex = lists:nth(rand:uniform(length(Indices)), Indices),
    % io:format("RandIndex ~p~n",[RandIndex]),
    rumouring(RandIndex, Nodes). %continue pushsuming

rumouring(RandNode, Nodes) -> 
    P =  whereis(list_to_atom(integer_to_list(RandNode))),
    if
        P /= undefined ->
            P ! {RandNode, 1, Nodes}; % S,W,Nodes
        true ->
            start_pushsum(Nodes)
    end.

kill_nodes(Nodes) -> 
    lists:foreach(fun({_, PID}) -> PID ! die end, Nodes).
