
:- ensure_loaded(harness).

data(triple(18, 12, 6)).

benchmark(triple(X, Y, Z), Out) :-
	call(tak(X, Y, Z, Out)).

tak(X,Y,Z,A) :-
	call(X =< Y), !,
	call(Z = A).
tak(X,Y,Z,A) :-
	% X > Y,
	call(X1 is X - 1),
	call(tak(X1,Y,Z,A1)),
	call(Y1 is Y - 1),
	call(tak(Y1,Z,X,A2)),
	call(Z1 is Z - 1),
	call(tak(Z1,X,Y,A3)),
	call(tak(A1,A2,A3,A)).

