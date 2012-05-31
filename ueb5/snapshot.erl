-module(snapshot).

-compile(export_all).

-record(state, {neighbours = []}).

birth() ->
    birth([]).

birth(Pids) ->
    %% spawn a new process
    Pid = spawn(snapshot, init, [#state{neighbours=Pids}]),
    io:format("Gave birth to process ~p~n", [Pid]),
    {ok, Pid}.

init(#state{neighbours=Pids}=State) ->
    %% for every neighbour send a ping message
    %% create graph
    io:format("Pinging neighbours ~p~n", [Pids]),
    ok = lists:foreach(fun ping/1, Pids),
    loop(State).

loop(#state{neighbours=Pids}=State) ->
    receive
        %% a new process pinged the process
        %% add it to the neighbours list
        {ping, Pid} ->
            Pid ! {pong},
            Pids2 = lists:append(Pids, [Pid]),
            loop(#state{neighbours=Pids2});
        %% print state of current process
        {stats, _Pid} ->
            io:format("State is: ~p~n", [State]),
            loop(State);
        {marker, _Pid} ->
            loop(State);
        _ -> loop(State)
    end.

ping(Destination) ->
    Destination ! {ping, self()}.

marker_loop(_State) ->
    ok.

test() ->
    {ok, P1} = birth(),
    {ok, P2} = birth([P1]),
    P1 ! {stats, self()},
    P2 ! {stats, self()}.
