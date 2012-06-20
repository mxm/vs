-module(maekawa).
-export([init/0, initProc/0]).


init() ->
    P1 = spawn(maekawa, initProc, []),
	P2 = spawn(maekawa, initProc, []),
	P3 = spawn(maekawa, initProc, []),
	P1 ! {setGroup, [P1,P2]},
    P2 ! {setGroup, [P2,P3]},
    P3 ! {setGroup, [P3,P1]},
	P1 ! enterCriticalSection,
	P2 ! enterCriticalSection,
	'initialization done'.

%%tell a process which group it belongs to
initProc() ->
	InitState = {_State = released, _Voted = false, _Requests = []},
	receive
		{setGroup, V} ->
			io:format("initializing ~p with ~p~n",[self(), V]),
			loop(InitState, V);
		_ ->
			io:format("init for ~p failed~n",[self()])
	end.

%%main loop
loop(State, V) ->
	{ProcState, Voted, Requests} = State,
	NewState =
		case ProcState of
			held ->
				%%we can now safely enter the critical sections
				criticalSection(),
				[GroupMember ! release || GroupMember <- V],
				{_ProcState = released, Voted, Requests};
			_ ->	
			receive 
				enterCriticalSection ->
					%% state := wanted (slide 43) is not necessary!?
					io:format("~p: entering critical section~n",[self()]),
					[GroupMember ! {request, self()} || GroupMember <- V],
					io:format("~p: sent out requests to everbody. waiting~n", [self()]),
					%loop({_ProcState = wanted, Voted, Requests}, V),
					self() ! reply,
					awaitReplies(length(V)),
					{_State = held, _Voted = false, Requests};
				{request, Pid} ->
					case State of
						{_ProcState = held, _Voted = true} ->
							io:format("~p: queuing request from ~p~n",[self(), Pid]),
							{ProcState, Voted, lists:append(Requests, [Pid])};
						_ ->
							io:format("~p: Sending reply to ~p~n", [self(), Pid]),
							Pid ! reply,
							{ProcState, _Voted = true, Requests}
					end;
				release ->
					case Requests of
						[] ->
							{ProcState, _Voted = false, []};
						[H|T] ->
							H ! reply,
							{ProcState, _Voted = true, T}
					end;		
				U ->
					io:format("~p: i don't understand this message: ~p~n",[self(), U]),
					{ProcState, Voted, Requests}
			end
		end,
	loop(NewState, V).

awaitReplies(N) ->
	awaitReplies(0, N).

awaitReplies(Replies, N) ->
	case Replies of
		N ->
			io:format("~p: Received ~p of ~p replies.~n",[self(), Replies, N]);
		_ ->
			receive 
				reply ->
					io:format("~p: Received reply ~p of ~p~n",[self(), Replies+1, N]),
					awaitReplies(Replies+1, N);
				U ->
					io:format("~p Not a valid reply: ~p~n", [self(), U]),
					awaitReplies(Replies, N)
			end
	end.

criticalSection() ->
	io:format("~p: I'm in the critical section. Hope noone's around...~n", [self()]),
	timer:sleep(1000),
	io:format("~p: I'm leaving the critical section~n", [self()]).
