-module(multicast).

-compile(export_all).


test() ->
    P1 = start(),
    P2 = start(),
    P3 = start(),
    P4 = start(),
    P5 = start(),
    Group = [P1, P2, P3, P4, P5],
    P1 ! {join, Group},
    P1 ! {multicast, "This is a message"},
    Group.

start() ->
    Pid = spawn(multicast, loop, [{0, 0, []}]),
    Pid.

loop({S, R, Group}=State) ->
    receive
        {join, Members} ->
            io:format("Process ~p joined group ~p~n", [self(), Members]),
            loop({S, R, Members});
        %% start a multicast message
        {multicast, Message} ->
            io:format("Received multicast init message ~p~n", [Message]),
            io:format("Sending multicast message to group ~p~n", [Group]),
            % send message to each member of the group
            Fun = fun (Pid) ->
                    Pid ! {msg, self(), S, Message}
            end,
            lists:foreach(Fun, Group),
            %% increase seq. number
            loop({S+1, R, Group});
        {msg, Src, S2, Message} ->
            io:format("Process ~p received Message ~p~n", [self(), {Src, S2, Message}]),
            loop(State);
        X ->
            io:format("Fail...~p~n",[X]),
            loop(State)
    end.
