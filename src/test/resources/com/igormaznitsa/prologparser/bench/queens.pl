% 9-queens program

:- ensure_loaded(harness).

benchmark(Data, Out) :-
	queen(Data, Out).

data([1,2,3,4,5,6,7,8,9]).

% #if	defined(CONSTRAINT_PROPAGATION)

queen(Data, Out) :-
	queen_2(Data, [], Out).

queen_2([], _, []).
queen_2([H|T], History, [Q|M]) :-
	qdelete(Q, H, T, L1),
	nodiag(History, Q, 1),
	queen_2(L1, [Q|History], M).

% #elif	defined(COROUTINING)
% 
% queen(Data, Out) :-
% 	safe(Out),
% 	qperm(Data, Out).
% 
% #else
% 
% queen(Data, Out) :-
% 	qperm(Data, Out),
% 	safe(Out).
% 
% #endif

qperm([], []).
qperm([X|Y], [U|V]) :-
	qdelete(U, X, Y, Z),
	qperm(Z, V).

qdelete(A, A, L, L).
qdelete(X, A, [H|T], [A|R]) :-
	qdelete(X, H, T, R).

safe([]).
safe([N|L]) :-
	nodiag(L, N, 1),
	safe(L).

nodiag([], _, _).
nodiag([N|L], B, D) :-
	D =\= N - B,
	D =\= B - N,
	D1 is D + 1,
	nodiag(L, B, D1).
