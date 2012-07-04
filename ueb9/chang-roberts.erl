-module('chang-roberts').
-export([init/1, loop/1, test/1]).

init(NumProcs) ->
	io:format("Spawning processes~n"),
	PIDs = init(NumProcs,[]),
	test(PIDs).

init(NumProcs, PIDs) when NumProcs > 0 ->
	PID = spawn('chang-roberts', loop, [{_Participant = false, _Leader = no, _Successor = undefined}]),
	io:format("Spawned process ~p~n",[PID]),
	init(NumProcs-1, PIDs ++ [PID]);

init(0, PIDs) ->
	io:format("Spawned all proccesses, will now establish the circle...~n"),
	establishCircle(PIDs, 0).

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

test(PIDs) ->
	lists:nth(1,PIDs) ! startElection.

loop({Participant, Leader, Successor}) ->
	%{Participant = false, Leader = no, Successor = undefined} = State,
	NewState = 
		receive 
			{setSuccessor, PID} ->
				io:format("~p: My successor is now ~p~n",[self(),PID]),
				{Participant, Leader, PID};
			startElection ->
				Successor ! {election, self()},
				{true, Leader, Successor};
			{election, PID} ->
				if
					PID > self() ->
						Successor ! {election, PID},
						{true, Leader, Successor};
					PID < self() ->
						if
							Participant =:= false ->
								Successor ! {election, self()};
							true ->
								'done with algo'
						end;
					PID =:= self() ->
						Successor ! {elected, self()},
						{false, Leader, Successor}
				end;
			{elected, PID} ->
				if
					PID =/= self() ->
						Successor ! {elected, PID}
				end,
				{false, _Leader = PID, Successor}
		end,
	loop(NewState).
