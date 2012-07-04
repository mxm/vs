-module('chang-roberts').
-compile(export_all).
%-export([init/1, loop/1]).

init(NumProcs) ->
	io:format("Spawning processes~n"),
	init(NumProcs,[]),
	tests().

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
			io:format("~p -> ~p~n",[lists:nth(Cur,PIDs),lists:nth(Suc,PIDs)]),
			lists:nth(Cur, PIDs) ! {setSucessor, lists:nth(Suc, PIDs)},
			establishCircle(PIDs, ShiftPos+1)
	end.

tests() ->
	ok.

loop(State) ->
	{Participant = false, Leader = no, Successor = undefined} = State,
	NewState = 
		receive 
			{setSuccessor, PID} ->
				{Participant, Leader, PID}				
		end,
	loop(NewState).
