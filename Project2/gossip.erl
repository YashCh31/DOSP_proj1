-module(gossip).
-export([create_actors/4, master/4, listeners/2, start_gossip/1, kill_nodes/1, check_convergence/4, rumouring/2,rumouring_process/3]).
-export([full/1, calc_full_neighbors/4]).
-export([ twod/1, calc_twod_neighbors/4]).
-export([ line/1, calc_line_neighbors/3]).
-export([ i3D/1, calc_ithreeD_neighbors/4]).

% Function that imports number of nodes and creates actors for 2D
twod(NumNodes) ->
    Rows = trunc(math:sqrt(NumNodes)),
    %io:format("~w", [Rows]),
    NeighborMap = calc_twod_neighbors(Rows*Rows, Rows, maps:new(), 1),
    %io:format("~w", [NeighborMap]),
    Master_PID = spawn(gossip, create_actors, [NumNodes, [], 1, NeighborMap]), % spawn(modulename, funcname, args)
    register(master, Master_PID).

% Functions that return the neighbor map for 2D
calc_twod_neighbors(0, _, NeighborMap, _) -> NeighborMap;
calc_twod_neighbors(NumNodes, Nnodes, NeighborMap, Cnt) ->
    if
        Cnt == 1 ->
            NeighborMap1 = maps:put(Cnt, [Cnt+1, Cnt+Nnodes], NeighborMap),
            calc_twod_neighbors(NumNodes-1, Nnodes, NeighborMap1,  Cnt+1);
        Cnt == Nnodes ->
            NeighborMap1 = maps:put(Cnt, [Cnt-1, Cnt+Nnodes], NeighborMap),
            calc_twod_neighbors(NumNodes-1, Nnodes, NeighborMap1,  Cnt+1);
        Cnt == ((Nnodes*Nnodes)-Nnodes+1) ->
            NeighborMap1 = maps:put(Cnt, [Cnt+1, Cnt-Nnodes], NeighborMap),
            calc_twod_neighbors(NumNodes-1, Nnodes, NeighborMap1,  Cnt+1);
        Cnt == Nnodes*Nnodes ->
            NeighborMap1 = maps:put(Cnt, [Cnt-1, Cnt-Nnodes], NeighborMap),
            calc_twod_neighbors(NumNodes-1, Nnodes, NeighborMap1,  Cnt+1);
        Cnt > 1 andalso Cnt < Nnodes ->
            NeighborMap1 = maps:put(Cnt, [Cnt-1, Cnt+1, Cnt+Nnodes], NeighborMap),
            calc_twod_neighbors(NumNodes-1, Nnodes, NeighborMap1,  Cnt+1);
        ((Nnodes*Nnodes) rem Cnt) == 1 ->
            NeighborMap1 = maps:put(Cnt, [Cnt-Nnodes, Cnt+1, Cnt+Nnodes], NeighborMap),
            calc_twod_neighbors(NumNodes-1, Nnodes, NeighborMap1,  Cnt+1);
        ((Nnodes*Nnodes) rem Cnt) == 0 ->
            NeighborMap1 = maps:put(Cnt, [Cnt-Nnodes, Cnt-1, Cnt+Nnodes], NeighborMap),
            calc_twod_neighbors(NumNodes-1, Nnodes, NeighborMap1,  Cnt+1);
        Cnt > ( Nnodes*Nnodes - Nnodes + 1) andalso Cnt < Nnodes*Nnodes ->
            NeighborMap1 = maps:put(Cnt, [Cnt-Nnodes, Cnt-1, Cnt-Nnodes], NeighborMap),
            calc_twod_neighbors(NumNodes-1, Nnodes, NeighborMap1,  Cnt+1);
        true ->
            NeighborMap1 = maps:put(Cnt, [Cnt-Nnodes, Cnt-1, Cnt-Nnodes, Cnt+Nnodes], NeighborMap),
            calc_twod_neighbors(NumNodes-1, Nnodes, NeighborMap1,  Cnt+1)
    end.


% Function that imports number of nodes and creates actors for 3D
i3D(NumNodes) ->
    Rows = trunc(math:sqrt(NumNodes)),
    %io:format("~w", [Rows]),
    NeighborMap = calc_ithreeD_neighbors(Rows*Rows, Rows, maps:new(), 1),
    %io:format("~w", [NeighborMap]),
    Master_PID = spawn(gossip, create_actors, [NumNodes, [], 1, NeighborMap]), % spawn(modulename, funcname, args)
    register(master, Master_PID).

% Functions that returns the neighbor map for 3D
calc_ithreeD_neighbors(0, _, NeighborMap, _) -> NeighborMap;
calc_ithreeD_neighbors(NumNodes, Nnodes, NeighborMap, Cnt) ->
    if
        Cnt == 1 ->
            NeighborMap1 = maps:put(Cnt, [Cnt+1, Cnt+Nnodes, Cnt+Nnodes+1], NeighborMap),
            calc_ithreeD_neighbors(NumNodes-1, Nnodes, NeighborMap1,  Cnt+1);
        Cnt == Nnodes ->
            NeighborMap1 = maps:put(Cnt, [Cnt-1, Cnt+Nnodes, Cnt+Nnodes-1], NeighborMap),
            calc_ithreeD_neighbors(NumNodes-1, Nnodes, NeighborMap1,  Cnt+1);
        Cnt == ((Nnodes*Nnodes)-Nnodes+1) ->
            NeighborMap1 = maps:put(Cnt, [Cnt+1, Cnt-Nnodes, Cnt-Nnodes+1], NeighborMap),
            calc_ithreeD_neighbors(NumNodes-1, Nnodes, NeighborMap1,  Cnt+1);
        Cnt == Nnodes*Nnodes ->
            NeighborMap1 = maps:put(Cnt, [Cnt-1, Cnt-Nnodes, Cnt-Nnodes-1], NeighborMap),
            calc_ithreeD_neighbors(NumNodes-1, Nnodes, NeighborMap1,  Cnt+1);
        Cnt > 1 andalso Cnt < Nnodes ->
            NeighborMap1 = maps:put(Cnt, [Cnt-1, Cnt+1, Cnt+Nnodes, Cnt+Nnodes+1], NeighborMap),
            calc_ithreeD_neighbors(NumNodes-1, Nnodes, NeighborMap1,  Cnt+1);
        ((Nnodes*Nnodes) rem Cnt) == 1 ->
            NeighborMap1 = maps:put(Cnt, [Cnt-Nnodes, Cnt+1, Cnt+Nnodes, Cnt+Nnodes+1], NeighborMap),
            calc_ithreeD_neighbors(NumNodes-1, Nnodes, NeighborMap1,  Cnt+1);
        ((Nnodes*Nnodes) rem Cnt) == 0 ->
            NeighborMap1 = maps:put(Cnt, [Cnt-Nnodes, Cnt-1, Cnt+Nnodes, Cnt+Nnodes+1], NeighborMap),
            calc_ithreeD_neighbors(NumNodes-1, Nnodes, NeighborMap1,  Cnt+1);
        Cnt > ( Nnodes*Nnodes - Nnodes + 1) andalso Cnt < Nnodes*Nnodes ->
            NeighborMap1 = maps:put(Cnt, [Cnt-Nnodes, Cnt-1, Cnt-Nnodes, Cnt-Nnodes+1], NeighborMap),
            calc_ithreeD_neighbors(NumNodes-1, Nnodes, NeighborMap1,  Cnt+1);
        true ->
            NeighborMap1 = maps:put(Cnt, [Cnt-Nnodes, Cnt-1, Cnt-Nnodes, Cnt+Nnodes, Cnt+Nnodes+1], NeighborMap),
            calc_twod_neighbors(NumNodes-1, Nnodes, NeighborMap1,  Cnt+1)
    end.


% Function that imports number of nodes and creates actors for Full topology
full(NumNodes) ->
    NeighborMap = calc_full_neighbors(NumNodes, maps:new(), 1, NumNodes),
    % io:format("neighbor map \n"),
    %io:format("~w", [NeighborMap]),
    %io:format("~w~n", [Neighbors]),
    %io:fwrite("~..0B~n",[lists:nth(2,Neighbors)]), % accessing 2nd element in list
    Master_PID = spawn(gossip, create_actors, [NumNodes, [], 1, NeighborMap]), % spawn(modulename, funcname, args)
    register(master, Master_PID).

% Functions that returns the neighbor map for full topologie
calc_full_neighbors(0, NeighborMap, _, _) -> NeighborMap;
calc_full_neighbors(NumNodes, NeighborMap, Cnt, N) ->
    IndexList = lists:filter(
        fun(X) -> X /= Cnt end,
    lists:seq(1,N)),
    NeighborMap1 = maps:put(Cnt, IndexList, NeighborMap),
    calc_full_neighbors(NumNodes-1, NeighborMap1, Cnt+1, N).


% Function that imports number of nodes and creates actors for Line topology
line(NumNodes) ->
    NeighborMap = calc_line_neighbors(NumNodes, maps:new(), 1),
    %io:format("neighbor map \n"),
    %io:format("~w", [NeighborMap]),
    Master_PID = spawn(gossip, create_actors, [NumNodes, [], 1, NeighborMap]), % spawn(modulename, funcname, args)
    register(master, Master_PID).

% Functions that returns the neighbor map for line topologie
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

% Function used to create actors
create_actors(0, Nodes, _, NeighborMap) ->
    ConvergedMap = maps:new(),
    T = erlang:timestamp(),
    start_gossip(Nodes),
    master(Nodes, ConvergedMap, NeighborMap, T);

create_actors(NumNodes, Nodes, Counter, NeighborMap) ->
    PID = spawn(gossip, listeners, [NeighborMap, 0]),
    register(list_to_atom(integer_to_list(Counter)), PID),
    create_actors(NumNodes-1, [{NumNodes,PID} | Nodes], Counter+1, NeighborMap).

master(Nodes, ConvergedMap, NeighborMap, T) ->
    receive
        {converge, Pid} ->
            % store converged nodes here
            ConvergedMap1 = maps:put(Pid, 1, ConvergedMap),
            % io:format("in map ~w~n", [ConvergedMap1]),
            check_convergence(Pid, ConvergedMap1, NeighborMap, Nodes), % of all nodes
            master(Nodes, ConvergedMap1, NeighborMap, T);
        converged ->
            % kill all if converged
            io:format("converged~n"),
            FTime = timer:now_diff(erlang:timestamp(), T)/1000,
            io:format("Convergence time: ~p ms\n", [FTime]),
            kill_nodes(Nodes),
            exit(self(), normal)
    end.

listeners(NeighborMap, Cnt) ->
    receive
        {chinesewhisper, Nodes} ->
            Pid = self(),
            % io:format("pid ~p~n ~w~n",[self(), Nodes]),
            if
                Cnt>=5 ->
                    master ! {converge, Pid};
                true ->
                    rumouring_process(Nodes, NeighborMap, Pid),
                    listeners(NeighborMap,Cnt+1)
            end;
        die ->
            exit(self(), normal)
    end.

check_convergence(Pid, ConvergedMap, NeighborMap, Nodes) ->
    Converged = maps:keys(ConvergedMap),
    if
        length(Nodes) == length(Converged) ->
            master ! converged;
        true ->
            rumouring_process(Nodes, NeighborMap, Pid)
    end.

% Function that starts the gossip
start_gossip(Nodes) ->
    % pick a random index from nodes to start with
    RandIndex = rand:uniform(length(Nodes)),
    rumouring(RandIndex, Nodes).

% Function used to select a node from the neighbormap for gossip to travel
rumouring_process(Nodes, NeighborMap, Pid) ->
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
    rumouring(RandIndex, Nodes). %continue gossiping

rumouring(RandNode, Nodes) ->
    P =  whereis(list_to_atom(integer_to_list(RandNode))),
    if
        P /= undefined ->
            P ! {chinesewhisper, Nodes};
        true ->
            start_gossip(Nodes)
    end.

% Function used to destroy the nodes after convergence
kill_nodes(Nodes) ->
    lists:foreach(fun({_, PID}) -> PID ! die end, Nodes).
