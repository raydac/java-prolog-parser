/**
 * Prolog code for the DCG calculator.
 *
 * DCGs are a simple case of Colmerauers Metamorphosis grammars
 * an were presented to the community in the 1980 paper
 * Definite Clause Grammars for Language Analysis, by F.C.N.
 * Pereira and D.H.D. Warren.
 *
 * The DCG calculator is usually considered Prolog homework.
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

% calc
calc :-
   phrase(expr(_), "-12+34*56+78").
calc :-
   phrase(expr(_), "(-12+34)*(56+78)").

/*****************************************************************/
/* Reduced Test Cases                                            */
/*****************************************************************/

% rcalc
rcalc :-
   phrase(expr(_), "(-12+34)*56+78").

/*****************************************************************/
/* The Calculator                                                */
/*****************************************************************/

% expr(-Integer)
expr(Z) --> "-", !,
   term(X),
   Y is -X,
   expr_rest(Y, Z).
expr(Y) -->
   term(X),
   expr_rest(X, Y).

% expr_rest(+Integer, -Integer)
expr_rest(X, T) --> "+", !,
   term(Y),
   Z is X+Y,
   expr_rest(Z, T).
expr_rest(X, T) --> "-", !,
   term(Y),
   Z is X-Y,
   expr_rest(Z, T).
expr_rest(X, X) --> [].

% term(-Integer)
term(Y) -->
   factor(X),
   term_rest(X, Y).

% term_rest(+Integer, -Integer)
term_rest(X, T) --> "*", !,
   factor(Y),
   Z is X*Y,
   factor_rest(Z, T).
term_rest(X, T) --> "/", !,
   factor(Y),
   Z is X/Y,
   factor_rest(Z, T).
term_rest(X, X) --> [].

% factor(-Integer)
factor(X) --> "(", !,
   expr(X), ")".
factor(Y) -->
   digit(X),
   factor_rest(X, Y).

% factor_rest(+Integer, -Integer)
factor_rest(X, T) -->
   digit(Y), !,
   Z is X*10+Y,
   factor_rest(Z, T).
factor_rest(X, X) --> [].

% digit(-Integer)
digit(Y) -->
   [X],
   48 =< X,
    X =< 57,
    Y is X-48.
