/**
 * Prolog code for the arithmetic compare theory test cases.
 *
 * Source of test cases are the following standards:
 *   - Prolog General Core ISO/IUEC 13211-1
 *   - Draft Technical Corrigendum 2, WG17, Ulrich Neumerkel
 *     <a href="http://www.complang.tuwien.ac.at/ulrich/iso-prolog/dtc2">www.complang.tuwien.ac.at/ulrich/iso-prolog/dtc2</a>
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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 *
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- use_package(library(jekdev/reference/testing)).

:- multifile runner:ref/4.
:- discontiguous runner:ref/4.

:- multifile runner:case/4.
:- discontiguous runner:case/4.

/****************************************************************/
/* Arithmetic Comparisons                                       */
/****************************************************************/

/* X =:= Y */

runner:ref(=:=, 2, arithmetic_eqless, 'ISO 8.7.1.4').
runner:case(=:=, 2, arithmetic_eqless, 'ISO 8.7.1.4, ISO 1') :-
   \+ 0 =:= 1.
runner:case(=:=, 2, arithmetic_eqless, 'ISO 8.7.1.4, ISO 7') :-
   1.0 =:= 1.
runner:case(=:=, 2, arithmetic_eqless, 'ISO 8.7.1.4, ISO 14') :-
   3*2 =:= 7-1.
runner:case(=:=, 2, arithmetic_eqless, 'ISO 8.7.1.4, XLOG 1') :-
   catch(7 =:= -foobar, error(E,_), true),
   E == type_error(evaluable,foobar/0).

/* X =\= Y */

runner:ref(=\=, 2, arithmetic_eqless, 'ISO 8.7.1.4').
runner:case(=\=, 2, arithmetic_eqless, 'ISO 8.7.1.4, ISO 2') :-
   0 =\= 1.
runner:case(=\=, 2, arithmetic_eqless, 'ISO 8.7.1.4, ISO 8') :-
   \+ 1.0 =\= 1.
runner:case(=\=, 2, arithmetic_eqless, 'ISO 8.7.1.4, ISO 15') :-
   \+ 3*2 =\= 7-1.
runner:case(=\=, 2, arithmetic_eqless, 'ISO 8.7.1.4, XLOG 2') :-
   catch(7 =\= abs(_), error(E,_), true),
   E == instantiation_error.

/* X < Y */

runner:ref(<, 2, arithmetic_eqless, 'ISO 8.7.1.4').
runner:case(<, 2, arithmetic_eqless, 'ISO 8.7.1.4, ISO 3') :-
   0 < 1.
runner:case(<, 2, arithmetic_eqless, 'ISO 8.7.1.4, ISO 10') :-
   \+ 1.0 < 1.
runner:case(<, 2, arithmetic_eqless, 'ISO 8.7.1.4, ISO 16') :-
   \+ 3*2 < 7-1.
runner:case(<, 2, arithmetic_eqless, 'ISO 8.7.1.4, XLOG 3') :-
   catch(sign(foobar) < 7, error(E,_), true),
   E == type_error(evaluable,foobar/0).

/* X > Y */

runner:ref(>, 2, arithmetic_eqless, 'ISO 8.7.1.4').
runner:case(>, 2, arithmetic_eqless, 'ISO 8.7.1.4, ISO 4') :-
   \+ 0 > 1.
runner:case(>, 2, arithmetic_eqless, 'ISO 8.7.1.4, ISO 11') :-
   \+ 1.0 > 1.
runner:case(>, 2, arithmetic_eqless, 'ISO 8.7.1.4, ISO 17') :-
   \+ 3*2 > 7-1.
runner:case(>, 2, arithmetic_eqless, 'ISO 8.7.1.4, ISO 23') :-
   catch(_ > 5, error(E,_), true),
   E == instantiation_error.

/* X =< Y */

runner:ref(=<, 2, arithmetic_eqless, 'ISO 8.7.1.4').
runner:case(=<, 2, arithmetic_eqless, 'ISO 8.7.1.4, ISO 6') :-
   0 =< 1.
runner:case(=<, 2, arithmetic_eqless, 'ISO 8.7.1.4, ISO 13') :-
   1.0 =< 1.
runner:case(=<, 2, arithmetic_eqless, 'ISO 8.7.1.4, ISO 19') :-
   3*2 =< 7-1.
runner:case(=<, 2, arithmetic_eqless, 'ISO 8.7.1.4, XLOG 5') :-
   catch(foobar+77 =< 7, error(E,_), true),
   E == type_error(evaluable,foobar/0).

/* X >= Y */

runner:ref(>=, 2, arithmetic_eqless, 'ISO 8.7.1.4').
runner:case(>=, 2, arithmetic_eqless, 'ISO 8.7.1.4, ISO 5') :-
   \+ 0 >= 1.
runner:case(>=, 2, arithmetic_eqless, 'ISO 8.7.1.4, ISO 12') :-
   1.0 >= 1.
runner:case(>=, 2, arithmetic_eqless, 'ISO 8.7.1.4, ISO 18') :-
   3*2 >= 7-1.
runner:case(>=, 2, arithmetic_eqless, 'ISO 8.7.1.4, XLOG 6') :-
   catch(77-_ >= 7, error(E,_), true),
   E == instantiation_error.

/* min(X, Y) */

runner:ref(min, -3, arithmetic_eqless, 'Corr.2 9.3.9').
runner:case(min, -3, arithmetic_eqless, 'Corr.2 9.3.9, ISO 1') :-
   2 is min(2,3).
runner:case(min, -3, arithmetic_eqless, 'Corr.2 9.3.9, ISO 2') :-
   2.0 is min(2,3.0).
runner:case(min, -3, arithmetic_eqless, 'Corr.2 9.3.9, ISO 3') :-
   2.0 is min(3,2.0).
runner:case(min, -3, arithmetic_eqless, 'Corr.2 9.3.9, XLOG 1') :-
   catch(3.0 is min(foobar,3.0), error(E,_), true),
   E == type_error(evaluable,foobar/0).

/* max(X, Y) */

runner:ref(max, -3, arithmetic_eqless, 'Corr.2 9.3.8').
runner:case(max, -3, arithmetic_eqless, 'Corr.2 9.3.8, ISO 1') :-
   3 is max(2,3).
runner:case(max, -3, arithmetic_eqless, 'Corr.2 9.3.8, ISO 2') :-
   3.0 is max(2.0,3).
runner:case(max, -3, arithmetic_eqless, 'Corr.2 9.3.8, ISO 3') :-
   3.0 is max(3.0,2).
runner:case(max, -3, arithmetic_eqless, 'Corr.2 9.3.8, XLOG 1') :-
   catch(3.0 is max(2.0,_), error(E,_), true),
   E == instantiation_error.

/* epsilon */

runner:ref(epsilon, -1, arithmetic_eqless, 'N208 9.7.3').
runner:case(epsilon, -1, arithmetic_eqless, 'N208 9.7.3, XLOG 1') :-
   \+ 1+epsilon =:= 1.
runner:case(epsilon, -1, arithmetic_eqless, 'N208 9.7.3, XLOG 2') :-
   1+epsilon/2 =:= 1.
