%	pri2

:- ensure_loaded(harness).

benchmark(Data, Out) :-
	primes(Data, Out).

data(98).

primes(Limit, Ps) :-
	integers(2, Limit, Is),
	sift(Is, Ps).

integers(Low, High, [Low | Rest]) :- 
	Low =< High, !,
	M is Low + 1,
	integers(M, High, Rest).
integers(_,_,[]).

sift([], []).
sift([I | Is], [I | Ps]) :-
	remove(I, Is, New),
	sift(New, Ps).

remove(_P,[],[]).
remove(P,[I | Is], Nis0) :-
	I mod P =\= 0, !,
	Nis0 = [I | Nis],
	remove(P,Is,Nis).
remove(P,[_I | Is], Nis) :-
	remove(P,Is,Nis).
