% generated: 9 November 1989
% option(s): 
%
%   mu
%
%   derived from Douglas R. Hofstadter, "Godel, Escher, Bach," pages 33-35.
%
%   prove "mu-math" theorem muiiu

:- ensure_loaded(harness).

benchmark(Data, Out) :-
	length(Data, Len),
	theorem(Data, Len, Out).

data([m,u,i,i,u]).

theorem([m,i], _, [[a|[m,i]]]).
theorem(R, Depth, [[N|R]|P]) :-
    Depth > 0,
    D is Depth-1,
    theorem(S, D, P),
    rule(N, S, R).

rule(1, S, R) :- rule1(S, R).
rule(2, S, R) :- rule2(S, R).
rule(3, S, R) :- rule3(S, R).
rule(4, S, R) :- rule4(S, R).

rule1([i], [i,u]).
rule1([H|X], [H|Y]) :-
    rule1(X, Y).

rule2([m|X], [m|Y]) :- 
    concatenate(X, X, Y).

rule3([i,i,i|X], [u|X]).
rule3([H|X], [H|Y]) :-
    rule3(X, Y).

rule4([u,u|X], X).
rule4([H|X], [H|Y]) :-
    rule4(X, Y).

concatenate([], X, X).
concatenate([A|B], X, [A|B1]) :-
    concatenate(B, X, B1).
