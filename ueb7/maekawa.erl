-module(maekawa).
-export([init/0, initProc/0]).

%% feel free to adjust the init function

init() ->
	%%6 processes and 3 groups
	P1 = spawn(maekawa, initProc, []),
	P2 = spawn(maekawa, initProc, []),
	P3 = spawn(maekawa, initProc, []),
	P4 = spawn(maekawa, initProc, []),
	P5 = spawn(maekawa, initProc, []),
	P6 = spawn(maekawa, initProc, []),
	G1 = [P1,P2,P3],
	G2 = [P2,P4,P5],
	G3 = [P5,P6,P1],
	P1 ! {setGroup, G1},
    P2 ! {setGroup, G2},
    P3 ! {setGroup, G1},
	P4 ! {setGroup, G2},
	P5 ! {setGroup, G3},
	P6 ! {setGroup, G3},
	P1 ! enterCriticalSection,
	P3 ! enterCriticalSection,
	P6 ! enterCriticalSection,
	'initialization done'.

%%tell a process which group it belongs to
initProc() ->
	InitState = {_State = released, _Voted = false, _Requests = [], _Replies = 0},
	receive
		{setGroup, V} ->
			io:format("initializing ~p with ~p~n",[self(), V]),
			loop(InitState, V);
		_ ->
			io:format("init for ~p failed~n",[self()])
	end.

%%main loop
loop(State, V) ->
	{ProcState, Voted, Requests, Replies} = State,
	NewState =
		case ProcState of
			held ->
				%%we can now safely enter the critical sections
				criticalSection(),
				[GroupMember ! release || GroupMember <- V],
				{_ProcState = released, Voted, Requests, Replies};
			wanted ->
				if 
					Replies =:= length(V) ->
						{_ProcState = held, Voted, Requests, 0};
					true ->
						processMessages(State, V)
				end;
			released ->
				processMessages(State, V)	
		end,
	%io:format("~p: state is ~p~n",[self(), NewState]),
	loop(NewState, V).

%%message processing
processMessages(State, V) ->
	{ProcState, Voted, Requests, Replies} = State,
		receive 
			enterCriticalSection ->
				io:format("~p: trying to enter critical section~n",[self()]),
				[GroupMember ! {request, self()} || GroupMember <- V],
				io:format("~p: sent out requests to group ~n", [self()]),
				{_State = wanted, _Voted = false, Requests, Replies};
			{request, Pid} ->
				case State of
					{_ProcState = held, _Voted, _Requests, _Replies} ->
						io:format("~p: queuing request from ~p~n",[self(), Pid]),
						{ProcState, Voted, Requests ++ [Pid], Replies};
					{_ProcState, _Voted = true, _Requests, _Replies} ->
						io:format("~p: queuing request from ~p~n",[self(), Pid]),
						{ProcState, Voted, Requests ++ [Pid], Replies};
					_ ->
						io:format("~p: Sending reply to ~p~n", [self(), Pid]),
						Pid ! reply,
						{ProcState, _Voted = true, Requests, Replies}
				end;
			release ->
				case Requests of
					[] ->
						{ProcState, _Voted = false, [], Replies};
					[H | T] ->
						io:format("~p: Sending reply to ~p~n", [self(), H]),
						H ! reply,
						{ProcState, _Voted = true, T, Replies}
				end;
			reply ->
				io:format("~p: Received reply number ~p~n", [self(), Replies+1]),
				{ProcState, Voted, Requests, Replies + 1};
			_ ->
				io:format("~p: WARNING i don't understand this message: ~~n",[self()]),
				State
		end.

%%the critical section
criticalSection() ->
	io:format("~p: I'm in the critical section. Hope noone's around...~n", [self()]),
	timer:sleep(1000),
	io:format("~p: I'm leaving the critical section~n", [self()]).
