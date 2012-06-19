-module(maekawa).
-export([init/0, initProc/1]).


init() ->
	InitState = {ProcState = released, Voted = false},

    P1 = spawn(maekawa, initProc, [InitState]),
	P2 = spawn(maekawa, initProc, [InitState]),
	P3 = spawn(maekawa, initProc, [InitState]),
	P1 ! {setGroup, [P1,P2]},
    P2 ! {setGroup, [P2,P3]},
    P3 ! {setGroup, [P3,P1]},
	P1 ! {enterCriticalSection, self()},
	P2 ! {enterCriticalSection, self()},
	'initialization done'.

%%tell a process which group it belongs to
initProc(State) ->
	receive
		{setGroup, V} ->
			io:format("initializing ~p with ~p~n",[self(), V]),
			loop(State, V);
		_ ->
			io:format("init for ~p failed~n",[self()])
	end.

loop(State, V) ->
	receive 
		{enterCriticalSection,Pid} ->
			io:format("entering critical section: ~p~n",[self()]),
			loop({wanted, false}, V);
		_ ->
			io:format("i don't understand this message!~n")
	end,	
		%{ProcState, Voted} = State,
	    %io:format("ProcState: ~p, Voted: ~p, V: ~p~n", [ProcState, Voted, V]),
		%timer:sleep(1000),
	loop(State, V).
