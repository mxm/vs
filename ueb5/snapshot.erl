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
            snapshot_loop_init(State); %% received the first marker
        _ -> loop(State)
    end.


snapshot_loop_init(#state{neighbours=Pids}=State) ->
    Snapshot = State, %% record internal process state
    lists:foreach(fun send_marker/1, Pids), %% send one marker M over channel i
    %%snapshot_loop(Snapshot, [], State),
    ok.

%% use a prop list to record messages received from a process
%% if we receive a marker on a channel stop recording
%% add Messages for channel to finial snapshot ??
-record(snapshot, {processstate = state, messagerecords = []}).

snapshot_loop(#snapshot{processstate=Snapshot, messagerecords=Records}, Messages, State) ->
    receive
        {marker, Pid} ->
            %% stop recording for channel
            Records2 = [lists:get_all_values(Pid, Messages)|Records],
            Messages2 = lists:delete(Pid, Messages),
            snapshot_loop(#snapshot{processstate=Snapshot, messagerecords=Records2}, Messages2, State);
        {Message, Pid} ->
            %% record message for process
            case lists:is_defined(Pid, Messages) of
                true ->
                    snapshot_loop(Snapshot, [Message|Messages], State);
                false ->
                    snapshot_loop(Snapshot, Messages, State)
            end
    end.




%% send a ping message to a process
ping(Destination) ->
    Destination ! {ping, self()}.

%% send a marker to a process
send_marker(Destination) ->
    Destination ! {marker, self()}.

test() ->
    {ok, P1} = birth(),
    {ok, P2} = birth([P1]),
    P1 ! {stats, self()},
    P2 ! {stats, self()}.
