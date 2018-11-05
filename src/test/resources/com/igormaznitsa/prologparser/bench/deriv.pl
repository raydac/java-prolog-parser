
:- ensure_loaded(harness).

data(_Data).

benchmark(_Data, quad(E1, E2, E3, E4)) :-
	ops8(E1), divide10(E2), log10(E3), times10(E4).

ops8(E) :-
	d((x + 1) * ((^(x, 2) + 2) * (^(x, 3) + 3)), x, E).

divide10(E) :-
	d(((((((((x / x) / x) / x) / x) / x) / x) / x) / x) / x, x, E).

log10(E) :-
	d(log(log(log(log(log(log(log(log(log(log(x)))))))))), x, E).

times10(E) :-
	d(((((((((x * x) * x) * x) * x) * x) * x) * x) * x) * x, x, E).

d(U + V, X, DU + DV) :-
	!,
	d(U, X, DU),
	d(V, X, DV).
d(U - V, X, DU - DV) :-
	!,
	d(U, X, DU),
	d(V, X, DV).
d(U * V, X, DU * V + U * DV) :-
	!,
	d(U, X, DU),
	d(V, X, DV).
d(U / V, X, (DU * V - U * DV) / ^(V, 2)) :-
	!,
	d(U, X, DU),
	d(V, X, DV).
d(^(U, N), X, DU * N * ^(U, N1)) :-
	!,
	N1 is N - 1,
	d(U, X, DU).
d(-U, X, -DU) :-
	!,
	d(U, X, DU).
d(exp(U), X, exp(U) * DU) :-
	!,
	d(U, X, DU).
d(log(U), X, DU / U) :-
	!,
	d(U, X, DU).
d(X, X, 1) :-
	!.
d(_, _, 0).
