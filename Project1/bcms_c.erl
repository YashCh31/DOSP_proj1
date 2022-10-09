-module(bcms_c).
-export([genBT/1, genBT/2, reg/0,connect/1, start/1, stop/0]).
-export([start_workers/4, master/2, start_mining/1]).
-import(string,[concat/2,substr/3]).

% The function that finds hashed values with leading zeros i.e. the coin
genBT(Input) -> genBT(Input, "").
genBT(0, HshStr) -> HshStr;

genBT(Input, _) ->
    receive
        start ->
            RandStr = randStr(),
            UFID = "divyajyotiukirde:",
            Str = concat(UFID, RandStr),
            HshStr = io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256, Str))]),
            HshStrZero = substr(HshStr, 1, Input),
            MaxZeros = "0000000000000000000000000000000000000000000000000000000000000000",
            InputLenZeros = substr(MaxZeros, 1, Input), %create input length string of 0s
            case (HshStrZero == InputLenZeros) of
                true ->
                    master ! {self(), Str, HshStr},
                    genBT(0, HshStr);
                false ->
                    self() ! start,
                    genBT(Input, "")
            end;
        die ->
            exit(self(), normal) % return worker number on exit; terminate worker
    end.

% The master that delegates work to workers on client received from server
master(Workers, Host) ->
    receive
        {PID, Str, HshStr} ->
            io:format("Worker ~p found the coin ", [PID]),
            io:fwrite("~s \n",[Str]),
            io:fwrite(HshStr),
            io:fwrite("\n"),
            {snode, Host} ! {PID, Str, HshStr}, %send HshStr to server node
            stop(Workers);
        stop ->
            stop(Workers);
        start ->
            start_mining(Workers),
            master(Workers, Host)
    end.

start_workers(0, Workers, _, Host) ->
    process_flag(trap_exit, true),
    master(Workers, Host);

start_workers(N, Workers, Input, Host) ->
    Worker_PID = spawn_link(bcms_c, genBT, [Input, Host]),
    start_workers(N-1, [{N,Worker_PID} | Workers], Input, Host).

% cryptographically strong random string generator
randStr() -> base64:encode(crypto:strong_rand_bytes(8)).

stop()  ->
    master ! stop.

start_mining(Workers) ->
    lists:foreach(fun({_, PID}) -> PID ! start end, Workers).

stop(Workers) ->
    lists:foreach(fun({_, PID}) -> PID ! die end, Workers).

% This function starts the workers
    % listen for jobs
    % take Input from server
    % start workers
start(Host) ->
    receive
        {start, Input} ->
            P = erlang:system_info(logical_processors_available),
            io:format("Number of processors available: ~w\n", [P]),
            Master_PID = spawn(bcms_c, start_workers, [P*10, [], Input, Host]), % spawn(modulename, funcname, args)
            register(master, Master_PID),
            master ! start,
            start(Host);
        die ->
            io:format("End processes")
            %master ! stop
    end.

% The client starts here
connect(Host) ->
    io:format("Client: ~p here ", [self()]),
    net_adm:ping(Host), % connect to host
    io:format("~w.~n", [nodes()]),
    {snode, Host} ! {self()},
    start(Host).

% Used to register the client node
reg() ->
     register(cnode, self()).
