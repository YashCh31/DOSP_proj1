-module(chord).
-export([start/2, create_actors/4, master/6, route/4,start/1,kill_nodes/1,findInFingerTable/3,finger_table_helper/4]).
-export([checkConvergence/2, checkIfReqSuccessful/3, checkIfPidExists/3]).

start(NumNodes, NumReqs) ->
    Master_PID = spawn(chord, create_actors, [NumNodes, [], NumNodes, NumReqs]), % spawn(modulename, funcname, args)
    register(master, Master_PID),
    master ! start.

create_actors(_, Nodes, 0, NumReqs) -> 
    T = erlang:timestamp(),
    % io:format("actors ~w~n", [Nodes]),
    master(Nodes, NumReqs, 0, 0, 0, T);

create_actors(NumNodes, Nodes, Counter, NumReqs) ->
    RandIndex = rand:uniform(NumNodes),
    PID = spawn(chord, route, [0, NumNodes, NumReqs, RandIndex]),
    register(list_to_atom(integer_to_list(Counter)), PID),
    create_actors(NumNodes, [{Counter,PID} | Nodes], Counter-1, NumReqs).

master(Nodes, NumReqs, HopCount, SuccessReqs, NodeCount, T) -> 
    receive
        start -> 
            start(Nodes),
            master(Nodes, NumReqs, HopCount, SuccessReqs, NodeCount, T);
        routed -> 
            % increment hop count
            % io:format("\ncurr hop count: ~p \n", [HopCount]),
            start(Nodes),
            master(Nodes, NumReqs, HopCount+1, SuccessReqs, NodeCount, T);
        converged -> 
            % check if all nodes converged
            if 
                length(Nodes) == NodeCount+1 ->
                    % return HopCount
                    % when each node contacted numReqs times
                    io:format("\nTotal hop count: ~p \n", [HopCount]),
                    io:format("\nTotal successful requests: ~p \n", [SuccessReqs]),
                    AvgHopCount = HopCount/(length(Nodes)*NumReqs),
                    io:format("\nAverage hop count: ~p \n", [AvgHopCount]),
                    FTime = timer:now_diff(erlang:timestamp(), T)/1000,
                    io:format("\nTime: ~p ms\n", [FTime]),
                    % kill all nodes here
                    timer:sleep(10000),
                    kill_nodes(Nodes),
                    exit(self(), normal);
                true ->
                    start(Nodes),
                    master(Nodes, NumReqs, HopCount+1, SuccessReqs+1, NodeCount+1, T)
            end;
            
        success -> 
            % io:format("\ncurr successful requests: ~p \n", [SuccessReqs]),
            start(Nodes),
            master(Nodes, NumReqs, HopCount, SuccessReqs+1, NodeCount, T)
    end.

% routing nodes
route(CountReqs, NumNodes, NumReqs, DestNodeIndex) ->
    checkConvergence(CountReqs, NumReqs),
    receive
        {start, Nodes} ->
            Nodes1 = lists:map(
                fun(T) ->
                    lists:nth(2,tuple_to_list(T))
                end,
                Nodes),
            SourceIndex = string:str(Nodes1, [self()]),
            FoundIndex = findInFingerTable(SourceIndex, DestNodeIndex, NumNodes),
            checkIfReqSuccessful(Nodes, FoundIndex, DestNodeIndex),
            % io:format("start source: ~p dest: ~p next: ~p \n",[SourceIndex, DestNodeIndex, FoundIndex]),
            route(CountReqs+1, NumNodes, NumReqs, DestNodeIndex);
        {find, Nodes, DestNodeIndex} ->
            Nodes2 = lists:map(
                fun(T) ->
                    lists:nth(2,tuple_to_list(T))
                end,
                Nodes),
            SourceIndex1 = string:str(Nodes2, [self()]),
            FoundIndex1 = findInFingerTable(SourceIndex1, DestNodeIndex, NumNodes),
            checkIfReqSuccessful(Nodes, FoundIndex1, DestNodeIndex),
            % io:format("find source: ~p dest: ~p next: ~p \n",[SourceIndex1, DestNodeIndex, FoundIndex1]),
            route(CountReqs+1, NumNodes, NumReqs, DestNodeIndex);
        die ->
            exit(self(), normal)
    end.

checkConvergence(CountReqs, NumReqs) -> 
    if 
        CountReqs == NumReqs ->
            master ! converged;
        true ->
            One = 1
    end.

checkIfReqSuccessful(Nodes, FoundIndex, DestNodeIndex) ->
    if 
        FoundIndex /= DestNodeIndex ->
            Pid =  whereis(list_to_atom(integer_to_list(FoundIndex))),
            checkIfPidExists(Pid, Nodes, DestNodeIndex);
        true ->
            master ! success,
            % io:format("successful request, find more sources\n"),
            FoundIndex1 = rand:uniform(length(Nodes)),
            Pid1 = whereis(list_to_atom(integer_to_list(FoundIndex1))),
            checkIfPidExists(Pid1, Nodes, DestNodeIndex)
    end.

checkIfPidExists(Pid, Nodes, DestNodeIndex) ->
    if 
        Pid /= undefined ->
            % send message to successor node
            timer:sleep(1000),
            master ! routed,
            Pid ! {find, Nodes, DestNodeIndex};
        true ->
            FoundIndex = rand:uniform(length(Nodes)), % if fails to find Pid
            Pid1 = whereis(list_to_atom(integer_to_list(FoundIndex))),
            checkIfPidExists(Pid1, Nodes, DestNodeIndex)
    end.

start(Nodes) ->
    lists:foreach(fun({_, PID}) -> PID ! {start, Nodes} end, Nodes).

kill_nodes(Nodes) -> 
    lists:foreach(fun({_, PID}) -> PID ! die end, Nodes).

findInFingerTable(Src_index, Dest_index, NumNodes) ->
    Flag = true,
    if
        Src_index < Dest_index ->
        % find src_index + 2^i until its lesser than or equal to dest_index
        % return the closest index as dest_index
            Next_index = trunc(finger_table_helper(Flag, 0, Src_index, Dest_index));

        Src_index == Dest_index ->
            Next_index = Src_index;

        Src_index > Dest_index ->
        % add NumNodes to dest_index and find src_index + 2^i until lesser than or equal to dest_index
        % return the closest index-NumNodes as dest_index
            Next_index = finger_table_helper(Flag, 0, Src_index, Dest_index+NumNodes),
            if
                Next_index - NumNodes > 0 ->
                    Next_index1 = trunc(Next_index - NumNodes);
                true ->
                    Next_index1 = trunc(Next_index)
            end
    end.

finger_table_helper(false, _, Src_index, _) -> Src_index;
finger_table_helper(true, I, Src_index, Dest_index) ->
    Temp = math:pow(2,I),
    Count = I + 1,
    %io:fwrite("~w", Src_index),
    if
        Src_index + Temp < Dest_index ->
            finger_table_helper(true, Count, Src_index + Temp, Dest_index);
        Src_index + Temp == Dest_index ->
            finger_table_helper(false, Count, Src_index + Temp, Dest_index);
        Src_index + Temp > Dest_index ->
            finger_table_helper(false, 0, Src_index, Dest_index)
    end.