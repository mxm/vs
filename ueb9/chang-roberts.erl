-module('chang-roberts').
-export([init/1, loop/1, messageCounter/1, tester/1]).


%% Implementation of the Chang-Roberts ring algorithm
%%
%% For testing please send a message to the tester
%% tester ! test1  <---starts test1
%% tester ! test2  <---starts test2

%% Call this function to start with NumProcs processes
init(NumProcs) ->
	io:format("Spawning processes~n"),
	PIDs = init(NumProcs,[]),
	%% we only register once so that we can execute multiple times
	case whereis(messageCounter) of
		undefined ->
			register(messageCounter, spawn('chang-roberts', messageCounter, [0])),
			register(tester, spawn('chang-roberts', tester, [PIDs]));
		_ ->
			ok
	end.

%% Spawning of processes
init(NumProcs, PIDs) when NumProcs > 0 ->
	InitState = {_Participant = false, _Leader = no, _Successor = undefined},
	PID = spawn('chang-roberts', loop, [InitState]),
	io:format("Spawned process ~p~n",[PID]),
	init(NumProcs-1, PIDs ++ [PID]);

%% Spawning of processes done
init(0, PIDs) ->
	establishCircle(PIDs, 0).

%% Used to establish the circle
establishCircle(PIDs, ShiftPos) ->
	case ShiftPos of
		N when N >= length(PIDs) ->
			io:format("established circle.~n");
		_ ->
			Cur = ShiftPos+1,
			Suc = (ShiftPos + 1) rem length(PIDs) + 1,
			lists:nth(Cur, PIDs) ! {setSuccessor, lists:nth(Suc, PIDs)},
			establishCircle(PIDs, ShiftPos+1)
	end,
	PIDs.

%%Tester to control tests
tester(PIDs) ->
	receive
		test1 ->
			messageCounter ! reset,
			test1(PIDs);
		test2 ->
			messageCounter ! reset,
			test2(PIDs);
		true ->
			done
	end,
	tester(PIDs).

%% Testing with random starting node
test1(PIDs) ->
	random:seed(now()),
    {StartKnoten, _X} = random:uniform_s(length(PIDs),random:seed(now())),
	StartPID = lists:nth(StartKnoten, PIDs),
	io:format("Starting election with ~p~n", [StartPID]),
	StartPID ! startElection.

%% Testing with three random starting nods
test2(PIDs) ->
	test1(PIDs),
	test1(PIDs),
	test1(PIDs).

%% global message counter
messageCounter(Messages) ->
	receive
		count ->
			io:format("Number of messages: ~p~n",[Messages]),
			messageCounter(Messages+1);
		reset ->
			messageCounter(0);
		true ->
			Messages
	end.

%% main loop for the processes
loop({Participant, Leader, Successor}) ->
	%io:format("~p: State ~p~n",[self(), State]),
	NewState = 
		receive 
			{setSuccessor, PID} ->
				io:format("~p: My successor is now ~p~n",[self(),PID]),
				{Participant, Leader, _Successor = PID};
			startElection ->
				Successor ! {election, self()},
				{_Participant = true, Leader, Successor};
			{election, PID} ->
				if
					PID > self() ->
						Successor ! {election, PID},
						messageCounter ! count,
						{_Participant = true, Leader, Successor};
					PID < self() ->
						if
							Participant =:= false ->
								Successor ! {election, self()},
								messageCounter ! count;
							true ->
								ok
						end,
						{Participant,Leader,Successor};
					PID == self() ->
						Successor ! {elected, self()},
						messageCounter ! count,
						{_Participant = false, Leader, Successor}
				end;
			{elected, PID} ->
				if
					PID =/= self() ->
						Successor ! {elected, PID},
						messageCounter ! count,
						io:format("~p: Leader found (~p)~n", [self(), PID]);
					true ->
						ok
				end,
				{_Participant = false, _Leader = PID, Successor}
		end,
	loop(NewState).
