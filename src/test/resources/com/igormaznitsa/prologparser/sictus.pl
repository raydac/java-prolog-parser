cycle(State) :- transform(State, State1), cycle(State1).

concatenate([], L, L).
concatenate([X|L1], L2, [X|L3]) :- concatenate(L1, L2, L3).

member(X, [X|_]).
member(X, [_|L]) :- member(X, L).

reverse(L, L1) :- reverse_concatenate(L, [], L1).

reverse_concatenate([], L, L).
reverse_concatenate([X|L1], L2, L3) :-
        reverse_concatenate(L1, [X|L2], L3).

descendant(X, Y) :- offspring(X, Y).
descendant(X, Z) :- offspring(X, Y), descendant(Y, Z).

offspring(abraham, ishmael).
offspring(abraham, isaac).
offspring(isaac, esau).
offspring(isaac, jacob).

get_assoc(Key, t(K,V,L,R), Val) :-
        compare(Rel, Key, K),
        get_assoc(Rel, Key, V, L, R, Val).

get_assoc(=, _, Val, _, _, Val).
get_assoc(<, Key, _, Tree, _, Val) :-
        get_assoc(Key, Tree, Val).
get_assoc(>, Key, _, _, Tree, Val) :-
        get_assoc(Key, Tree, Val).

put_assoc(Key, t, Val, Tree) :- !, Tree = t(Key,Val,t,t).
put_assoc(Key, t(K,V,L,R), Val, New) :-
        compare(Rel, Key, K),
        put_assoc(Rel, Key, K, V, L, R, Val, New).

put_assoc(=, Key, _, _, L, R, Val, t(Key,Val,L,R)).
put_assoc(<, Key, K, V, L, R, Val, t(K,V,Tree,R)) :-
        put_assoc(Key, L, Val, Tree).
put_assoc(>, Key, K, V, L, R, Val, t(K,V,L,Tree)) :-
        put_assoc(Key, R, Val, Tree).

:- op(300, xfy, **). /* binds tighter than * */

d(X, X, D) :- atomic(X), !, D = 1.
d(C, X, D) :- atomic(C), !, D = 0.
d(U+V, X, DU+DV) :- d(U, X, DU), d(V, X, DV).
d(U-V, X, DU-DV) :- d(U, X, DU), d(V, X, DV).
d(U*V, X, DU*V+U*DV) :- d(U, X, DU), d(V, X, DV).
d(U**N, X, N*U**N1*DU) :- integer(N), N1 is N-1, d(U, X, DU).
d(-U, X, -DU) :- d(U, X, DU).

variables(X, [X|L0], L) :- var(X), !, L = L0.
variables(T, L0, L) :-
%       nonvar(T),
        functor(T, _, A),
        variables(0, A, T, L0, L).

variables(A, A, _, L0, L) :- !, L = L0.
variables(A0, A, T, L0, L) :-
%       A0<A,
        A1 is A0+1,
        arg(A1, T, X),
        variables(X, L0, L1),
        variables(A1, A, T, L1, L).

:- op(1150, fx, [wait]).

:- multifile (user:term_expansion/2).
user:term_expansion((:- wait(F/N)), (:- block(Head))) :-
        functor(Head, F, N),
        wb_args(N, Head).

wb_args(1, Head) :- !, arg(1, Head, '-').
wb_args(N, Head) :-
%       N>1,
        arg(N, Head, ?),
        N1 is N-1,
        wb_args(N1, Head).

my_clause( (grandparent(X, Z) :- parent(X, Y), parent(Y, Z)) ).

my_clause( (parent(john, mary) :- true) ).

execute((P,Q)) :- !, execute(P), execute(Q).
execute(P) :- predicate_property(P, built_in), !, P.
execute(P) :- my_clause((P :- Q)), execute(Q).

:- op(900, xfx, '=>').
:- op(800, xfy, '&').
:- op(300, xfx, ':').

sentence(P) --> noun_phrase(X, P1, P), verb_phrase(X, P1).

noun_phrase(X, P1, P) -->
        determiner(X, P2, P1, P), noun(X, P3), rel_clause(X, P3, P2).
noun_phrase(X, P, P) --> name(X).

verb_phrase(X, P) --> trans_verb(X, Y, P1), noun_phrase(Y, P1, P).
verb_phrase(X, P) --> intrans_verb(X, P).

rel_clause(X, P1, P1&P2) --> [that], verb_phrase(X, P2).
rel_clause(_, P, P) --> [].

determiner(X, P1, P2, all(X):(P1=>P2)) --> [every].
determiner(X, P1, P2, exists(X):(P1&P2)) --> [a].

noun(X, man(X)) --> [man].
noun(X, woman(X)) --> [woman].

name(john) --> [john].

trans_verb(X, Y, loves(X,Y)) --> [loves].
intrans_verb(X, lives(X)) --> [lives].

foreign_resource(count, [allocate_counters,init_counters,
                         incr_counter,get_counter]).

foreign(allocate_counters, c, allocate_counters).
foreign(init_counters, c, init_counters).
foreign(incr_counter, c, incr_counter([-integer])).
foreign(get_counter, c, get_counter(+integer, [-integer])).

:- load_foreign_resource(count).
:- allocate_counters.

count(Goal) :-
        statistics(walltime, [Start,_]),
        count_solutions(Goal),
        statistics(walltime, [End,_]),
        display_counts(N),
        Time is End - Start,
        format('Total ~d solutions in ~3d seconds.~n', [N,Time]).

count_solutions(Goal) :-
        init_counters,
        Goal,
        incr_counter(_),
        fail.
count_solutions(_).

display_counts(N) :-
        get_counter(-1, N),
        muse_num_workers(M),
        display_worker_counts(0, M).

display_worker_counts(M, M) :- !.
display_worker_counts(W, M) :-
        get_counter(W, C),
        (   C==0 -> true ;
            format('Solutions by worker ~d: ~d ~n', [W,C])
        ),
        W1 is W+1, display_worker_counts(W1, M).

