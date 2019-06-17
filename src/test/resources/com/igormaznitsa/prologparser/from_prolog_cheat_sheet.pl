 % --- SUPPORT PREDICATES ---

   not(P) :- call(P), !, fail.
   not(P).

   % --- SET OPERATIONS ---
   % ---    all assume that given lists have no duplicates:
   % ---    use make_set to ensure this if necessary

   % --- is X a member of given set?

   member(X,[X|_]).
   member(X,[_|Y]) :- member(X,Y).

   % --- More efficient to define non_member separately instead of
   % --- as not(member(X,L))

   non_member(X,[Y|T]) :- X \== Y, non_member(X,T).
   non_member(X,[]).

   % --- Remove duplicates from list

   make_set([],[]).
   make_set(X,Y) :- setof(Z,member(Z,X),Y).

   % --- append two lists (duplicates ARE preserved)
   % --- note why append(X,L,[X|L]). can cause trouble

   append([],X,X).
   append([X|Y],Z,[X|W]) :- append(Y,Z,W).

   % --- make a new set with all members except A

   delete(A,[A|X],X).
   delete(A,[B|X],[B|Z]) :- delete(A,X,Z).

   % --- is X a subset of Y ?

   subset([A|X],Y) :- member(A,Y), subset(X,Y).
   subset([],Y).

   % --- set intersection

   intersect([],Y,[]).
   intersect([X|R],Y,[X|Z]) :- member(X,Y),!,intersect(R,Y,Z).
   intersect([X|R],Y,Z) :- non_member(X,Y),!,intersect(R,Y,Z).

   % --- set difference

   difference([],Y,[]).
   difference([X|R],Y,Z) :- member(X,Y),!,difference(R,Y,Z).
   difference([X|R],Y,[X|Z]) :- difference(R,Y,Z).

   % --- set union

   union([],X,X).
   union([X|R],Y,Z) :- member(X,Y), !, union(R,Y,Z).
   union([X|R],Y,[X|Z]) :- union(R,Y,Z).

   % --- make a new set Y with new element A and old set X

   insert(A,X,Y) :- union([A],X,Y).

   % --- get the nth member of list

   nth([F|R],1,F).
   nth([F|R],N,M) :- N > 1, N1 is N-1, nth(R,N1,M).

   % --- are two sets equal?

   equal(X,X).
   equal(X,Y) :- difference(X,Y,[]), difference(Y,X,[]).

   % --- flatten list of lists into list

   flatten([],[]).
   flatten([[]|L],L).
   flatten([X|L1],[X|L2]) :- atomic(X), flatten(L1,L2).
   flatten([X|L1],L4) :- flatten(X,L2),
                         flatten(L1,L3),
                         append(L2,L3,L4).