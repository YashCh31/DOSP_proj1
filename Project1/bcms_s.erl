-module(bcms_s).
-export([genBT/1, genBT/2, reg/0, init/0, start/0, stop/0, listen/1]).
-export([start_workers/3, master/1, start_mining/1, kill_nodes/0]).
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

% The master that delegates work to workers on server
master(Workers) ->
    receive
        {PID, Str, HshStr} ->
            io:format("Worker ~p found the coin ", [PID]),
            io:fwrite("String : ~s  \n",[Str]),
            io:fwrite("Hash Value : ~s",[HshStr]),
            io:fwrite("\n"),
            stop(Workers),
            kill_nodes(); % kill node
        stop ->
            stop(Workers),
            kill_nodes();
        start ->
            start_mining(Workers),
            master(Workers)
    end.

start_workers(0, Workers, _) ->
    process_flag(trap_exit, true),
    master(Workers);

start_workers(N, Workers, Input) ->
    Worker_PID = spawn_link(bcms_s, genBT, [Input]),
    start_workers(N-1, [{N,Worker_PID} | Workers], Input).

% cryptographically strong random string generator
randStr() -> base64:encode(crypto:strong_rand_bytes(8)).

stop()  ->
    master ! stop.

start_mining(Workers) ->
    lists:foreach(fun({_, PID}) -> PID ! start end, Workers).

stop(Workers) ->
    {_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),
    U1 = Time1 * 100, % to find in microseconds
    U2 = Time2,
    io:format("Code time= ~p, Clock Time= ~p microseconds~n",[U1,U2]),
    lists:foreach(fun({_, PID}) -> PID ! die end, Workers).

% Once a client is connected, the mining procress is started
listen(Input) ->
    receive
        {From} ->
            io:fwrite("Client PID ~p connected\n", [From]),
            From ! {start, Input},   % send job to client
            %master ! start,
            listen(Input); % listen for answer
        {From, Str, HshStr} ->
            master ! {From, Str, HshStr}
    end.

start() ->
    master ! start.

kill_nodes() -> % for multiple nodes use foreach over nodes()
    lists:foreach(fun(Node) -> {cnode, Node} ! die end, nodes()).

% The server starts at init
init() ->
    P = erlang:system_info(logical_processors_available),
    io:format("Number of processors available: ~w\n", [P]),
    {ok, Input} = io:read("Enter No. of leading zeroes in hash:"),
    Master_PID = spawn(bcms_s, start_workers, [P*10, [], Input]), % spawn(modulename, funcname, args)
    register(master, Master_PID),
    listen(Input).

% register the node so that client can find it.
reg() ->
    register(snode, self()).
