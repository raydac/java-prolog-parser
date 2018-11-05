% A generic benchmark harness

runtime_entry(start) :-
	bench(1000).

bench(Count) :-
	cputime(T0),
	dodummy(Count),
	cputime(T1),
	dobench(Count),
	cputime(T2),
	Time is (T2-T1)-(T1-T0),
	format('~|~t~d~8+ iterations taking ~|~t~d~6+ msec~n', [Count,Time]).

dobench(Count) :-
	data(Data),
	repeat(Count),
	(benchmark(Data, _Result) -> true),
	fail.
dobench(_).

dodummy(Count) :-
	data(List),
	repeat(Count),
	dummy(List,_),
	fail.
dodummy(_).

dummy(_,_).

repeat(_N).
repeat(N) :-
	N > 1,
	N1 is N - 1,
	repeat(N1).

cputime(T) :-
	statistics(runtime, [T, _]).

