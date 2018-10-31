/**
 * Prolog code for the tic-tac-toe game.
 *
 * Min-max search via negation.
 *
 * Warranty & Liability
 * To the extent permitted by applicable law and unless explicitly
 * otherwise agreed upon, XLOG Technologies GmbH makes no warranties
 * regarding the provided information. XLOG Technologies GmbH assumes
 * no liability that any problems might be solved with the information
 * provided by XLOG Technologies GmbH.
 *
 * Rights & License
 * All industrial property rights regarding the information - copyright
 * and patent rights in particular - are the sole property of XLOG
 * Technologies GmbH. If the company was not the originator of some
 * excerpts, XLOG Technologies GmbH has at least obtained the right to
 * reproduce, change and translate the information.
 *
 * Reproduction is restricted to the whole unaltered document. Reproduction
 * of the information is only allowed for non-commercial uses. Selling,
 * giving away or letting of the execution of the library is prohibited.
 * The library can be distributed as part of your applications and libraries
 * for execution provided this comment remains unchanged.
 *
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

% move(+Board, +Player, -Board)
move([[-,B,C],[D,E,F],[G,H,I]], P, [[P,B,C],[D,E,F],[G,H,I]]).
move([[A,-,C],[D,E,F],[G,H,I]], P, [[A,P,C],[D,E,F],[G,H,I]]).
move([[A,B,-],[D,E,F],[G,H,I]], P, [[A,B,P],[D,E,F],[G,H,I]]).
move([[A,B,C],[-,E,F],[G,H,I]], P, [[A,B,C],[P,E,F],[G,H,I]]).
move([[A,B,C],[D,-,F],[G,H,I]], P, [[A,B,C],[D,P,F],[G,H,I]]).
move([[A,B,C],[D,E,-],[G,H,I]], P, [[A,B,C],[D,E,P],[G,H,I]]).
move([[A,B,C],[D,E,F],[-,H,I]], P, [[A,B,C],[D,E,F],[P,H,I]]).
move([[A,B,C],[D,E,F],[G,-,I]], P, [[A,B,C],[D,E,F],[G,P,I]]).
move([[A,B,C],[D,E,F],[G,H,-]], P, [[A,B,C],[D,E,F],[G,H,P]]).

% init(+Board)
init([[-,-,-],[-,-,-],[-,-,-]]).

% win(+Board, +Player)
win([[P,P,P],[_,_,_],[_,_,_]], P).
win([[_,_,_],[P,P,P],[_,_,_]], P).
win([[_,_,_],[_,_,_],[P,P,P]], P).
win([[P,_,_],[P,_,_],[P,_,_]], P).
win([[_,P,_],[_,P,_],[_,P,_]], P).
win([[_,_,P],[_,_,P],[_,_,P]], P).
win([[P,_,_],[_,P,_],[_,_,P]], P).
win([[_,_,P],[_,P,_],[P,_,_]], P).

% other(+Player, -Player).
other(o, x).
other(x, o).

% tie(+Board, +Player)
tie(X, P) :-
   \+ move(X, P, _).

% best(+Board, +Player, -Board)
best(X, P, Y) :-
   move(X, P, Y),
   (  win(Y, P) -> true
   ;  other(P, Q),
      \+ tie(Y, Q),
      \+ best(Y, Q, _)).

% tictac
tictac :-
   init(X),
   best(X, x, _).
