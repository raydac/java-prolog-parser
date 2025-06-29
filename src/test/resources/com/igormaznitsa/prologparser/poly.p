/**
 * Prolog code for the polynominal reduction benchmark.
 *
 * This benchmark can be found in:
 * Haygood, R. (1989): A Prolog Benchmark Suite for Aquarius,
 * Computer Science Division, University of California
 * Berkely, April 30, 1989
 *
 * It has its root in some Lisp code by R.P. Gabriel.
 *
 * We used a brushed up version where polynomials are better
 * normalized. Therefore we find some additional predicates
 * such as make_poly/3 and make_term/4.
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

/*****************************************************************/
/* Normal Test Cases                                             */
/*****************************************************************/

% poly
poly :-
   poly_add(1, poly(x,[term(1,-1)]), X1),
   poly_add(X1, poly(y,[term(1,1)]), X2),
   poly_add(X2, poly(z,[term(1,-1)]), X3),
   poly_exp(10, X3, _).

/*****************************************************************/
/* Reduced Test Cases                                            */
/*****************************************************************/

% rpoly
rpoly :-
   poly_add(1, poly(x,[term(1,-1)]), X1),
   poly_add(X1, poly(y,[term(1,1)]), X2),
   poly_exp(10, X2, _).

/*****************************************************************/
/* The Simplifier                                                */
/*****************************************************************/

% make_poly(+Sum, +Var, -Expr)
make_poly([], _, 0) :- !.
make_poly(Terms, Var, poly(Var,Terms)).

% poly_add(+Expr, +Expr, -Expr)
poly_add(poly(Var,Terms1), poly(Var,Terms2), Res) :- !,
   term_add(Terms1, Terms2, Terms),
   make_poly(Terms, Var, Res).
poly_add(poly(Var1,Terms1), poly(Var2,Terms2), poly(Var1,Terms)) :-
   Var1 @< Var2, !,
   add_to_order_zero_term(Terms1, poly(Var2,Terms2), Terms).
poly_add(Poly, poly(Var,Terms2), poly(Var,Terms)) :- !,
   add_to_order_zero_term(Terms2, Poly, Terms).
poly_add(poly(Var,Terms1), C, poly(Var,Terms)) :- !,
   add_to_order_zero_term(Terms1, C, Terms).
poly_add(C1, C2, C) :-
   C is C1+C2.

% make_term(+Expr, +Integer, +Sum, -Sum)
make_term(0, _, Terms, Terms) :- !.
make_term(C, E, Terms, [term(E,C)|Terms]).

% term_add(+Sum, +Sum, -Sum)
term_add([], X, X) :- !.
term_add(X, [], X) :- !.
term_add([term(E,C1)|Terms1], [term(E,C2)|Terms2], Res) :- !,
   poly_add(C1, C2, C),
   term_add(Terms1, Terms2, Terms),
   make_term(C, E, Terms, Res).
term_add([term(E1,C1)|Terms1], [term(E2,C2)|Terms2], [term(E1,C1)|Terms]) :-
   E1 < E2, !,
   term_add(Terms1, [term(E2,C2)|Terms2], Terms).
term_add(Terms1, [term(E2,C2)|Terms2], [term(E2,C2)|Terms]) :-
   term_add(Terms1, Terms2, Terms).

% add_to_order_zero_term(+Sum, +Expr, -Sum)
add_to_order_zero_term([term(0,C1)|Terms], C2, [term(0,C)|Terms]) :- !,
   poly_add(C1, C2, C).
add_to_order_zero_term(Terms, C, [term(0,C)|Terms]).

% poly_mul(+Expr, +Expr, -Expr)
poly_mul(poly(Var,Terms1), poly(Var,Terms2), poly(Var,Terms)) :- !,
   term_mul(Terms1, Terms2, Terms).
poly_mul(poly(Var1,Terms1), poly(Var2,Terms2), poly(Var1,Terms)) :-
   Var1 @< Var2, !,
   mul_through(Terms1, poly(Var2,Terms2), Terms).
poly_mul(P, poly(Var,Terms2), Res) :- !,
   mul_through(Terms2, P, Terms),
   make_poly(Terms, Var, Res).
poly_mul(poly(Var,Terms1), C, Res) :- !,
   mul_through(Terms1, C, Terms),
   make_poly(Terms, Var, Res).
poly_mul(C1, C2, C) :-
   C is C1*C2.

% term_mul(+Sum, +Sum, -Sum)
term_mul([], _, []) :- !.
term_mul(_, [], []) :- !.
term_mul([Term|Terms1], Terms2, Terms) :-
   single_term_mul(Terms2, Term, PartA),
   term_mul(Terms1, Terms2, PartB),
   term_add(PartA, PartB, Terms).

% single_term_mul(+Sum, +Summand, -Sum)
single_term_mul([], _, []).
single_term_mul([term(E1,C1)|Terms1], term(E2,C2), [term(E,C)|Terms]) :-
   E is E1+E2,
   poly_mul(C1, C2, C),
   single_term_mul(Terms1, term(E2,C2), Terms).

% mul_through(+Sum, +Expr, -Sum)
mul_through([], _, []).
mul_through([term(E,Term)|Terms], Poly, Res) :-
   poly_mul(Term, Poly, NewTerm),
   mul_through(Terms, Poly, NewTerms),
   make_term(NewTerm, E, NewTerms, Res).

% poly_expr(+Integer, +Expr, -Expr)
poly_exp(0, _, 1) :- !.
poly_exp(N, Poly, Result) :-
   N rem 2 =:= 0, !,
   M is N//2,
   poly_exp(M, Poly, Part),
   poly_mul(Part, Part, Result).
poly_exp(N, Poly, Result) :-
   M is N-1,
   poly_exp(M, Poly, Part),
   poly_mul(Poly, Part, Result).
