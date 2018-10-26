start(L, N, T) :- 	
			thread_create(ID, firstResp(L, N)), 
			secondResp(L, N, T).
			
secondResp(L, 0, T):- !. 
secondResp([H|Tail], N, T) :-
			occurr_count(T,H,Count), 
			C is N-1, 
			secondResp(Tail,C, T).
firstResp(L, 0) :- !. 
firstResp([H|Tail], N) :-
			plain(H,L_plain), 
			bubble(L_plain,L_ord), 
			C is N-1, 
			firstResp(Tail,C).

plain(L1,L2) :- plain(L1,[],L2).

plain([],ACC,ACC). 
plain([H|REST],ACC,L2) :- 	
			H = [_|_], 
			plain(H,ACC,ACC1), 
			plain(REST,ACC1,L2).
plain([H|REST],ACC,L2) :- 
			append(ACC,[H],ACC1), 
			plain(REST,ACC1,L2).
plain(X,ACC,L2) :- 
			append(ACC,[X],L2).
			
bubble(L1,L2) :- bubble(L1,0,L2).

bubble(L1,0,L2) :- 
			sweep(L1,0,L2).
bubble(L1,0,L2) :- 
			sweep(L1,1,LTMP), bubble(LTMP,0,L2).
			
sweep([X|[]],0,[X|[]]). 
sweep([X,Y|REST1],CHANGED,[X|REST2]) :-
			X =< Y, sweep([Y|REST1],CHANGED,REST2). 
sweep([X,Y|REST1],1,[Y|REST2]) :-
			X > Y, sweep([X|REST1],_,REST2). 
			
occurr_count(T,L,N) :- 
			occurr_count(T,L,0,N).
occurr_count(_,[],ACC,ACC). 
occurr_count(T,[T|REST],ACC,N) :-
			ACC1 is ACC+1, occurr_count(T,REST,ACC1,N). 
occurr_count(T,[_|REST],ACC,N) :- 
			occurr_count(T,REST,ACC,N).