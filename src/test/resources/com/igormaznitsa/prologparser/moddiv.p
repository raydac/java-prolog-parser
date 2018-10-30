/**
 * Prolog code for the arithmetic round theory test cases.
 *
 * Source of test cases are the following standards:
 *   - Prolog General Core ISO/IUEC 13211-1
 *   - Draft Technical Corrigendum 1, WG17, Ulrich Neumerkel
 *     <a href="http://www.complang.tuwien.ac.at/ulrich/iso-prolog/dtc1">www.complang.tuwien.ac.at/ulrich/iso-prolog/dtc1</a>
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
/* Rounding Operations                                           */
/****************************************************************/

/* truncate(X)  */

runner:ref(truncate, -2, arithmetic_moddiv, 'ISO 9.1.7').
runner:case(truncate, -2, arithmetic_moddiv, 'ISO 9.1.7, ISO 8') :-
   0.0 is truncate(-0.5).
runner:case(truncate, -2, arithmetic_moddiv, 'ISO 9.1.7, XLOG 1') :-
   7.0 is truncate(7.6).
runner:case(truncate, -2, arithmetic_moddiv, 'ISO 9.1.7, ISO 9') :-
   catch(_ is truncate(foobar), error(E,_), true),
   E == type_error(evaluable,foobar/0).

/* floor(X) */

runner:ref(floor, -2, arithmetic_moddiv, 'ISO 9.1.7').
runner:case(floor, -2, arithmetic_moddiv, 'ISO 9.1.7, ISO 1') :-
   7.0 is floor(7.4).
runner:case(floor, -2, arithmetic_moddiv, 'ISO 9.1.7, ISO 2') :-
   -1.0 is floor(-0.4).
% runner:case(floor, -2, arithmetic_moddiv, 'ISO 9.1.7, ISO 24') :- current_prolog_flag(max_integer, M),
%   catch(_ is floor(M*2.0), error(E, _), true), E==evaluation_error(int_overflow).

/* ceiling(X) */

runner:ref(ceiling, -2, arithmetic_moddiv, 'ISO 9.1.7').
runner:case(ceiling, -2, arithmetic_moddiv, 'ISO 9.1.7, ISO 7') :-
   0.0 is ceiling(-0.5).
runner:case(ceiling, -2, arithmetic_moddiv, 'ISO 9.1.7, XLOG 2') :-
   8.0 is ceiling(7.6).

/* round(X) */

runner:ref(round, -2, arithmetic_moddiv, 'ISO 9.1.7').
runner:case(round, -2, arithmetic_moddiv, 'ISO 9.1.7, ISO 3') :-
   8.0 is round(7.5).
runner:case(round, -2, arithmetic_moddiv, 'ISO 9.1.7, ISO 4') :-
   8.0 is round(7.6).
runner:case(round, -2, arithmetic_moddiv, 'ISO 9.1.7, ISO 5') :-
   -1.0 is round(-0.6).
runner:case(round, -2, arithmetic_moddiv, 'ISO 9.1.7, ISO 6') :-
   catch(_ is round(_), error(E,_), true),
   E == instantiation_error.

/* X // Y */

runner:ref(//, -3, arithmetic_moddiv, 'ISO 9.1.7, Corrigendum 1').
runner:case(//, -3, arithmetic_moddiv, 'ISO 9.1.7, ISO 21') :-
   0 is 7//35.
runner:case(//, -3, arithmetic_moddiv, 'ISO 9.1.7, ISO 23') :-
   10 is 140//(3+11).
runner:case(//, -3, arithmetic_moddiv, 'ISO 9.1.7, XLOG 3') :-
   -2 is -5//2.
runner:case(//, -3, arithmetic_moddiv, 'ISO 9.1.7, XLOG 4') :-
   catch(_ is 7//0, error(E,_), true),
   E == evaluation_error(zero_divisor).
/* extra */
runner:case(//, -3, arithmetic_moddiv, 'ISO 9.1.7, XLOG 4') :-
   -2 is -7.5//3.5.

/* X rem Y */

runner:ref(rem, -3, arithmetic_moddiv, 'ISO 9.1.7').
runner:case(rem, -3, arithmetic_moddiv, 'ISO 9.1.7, XLOG 1') :-
   1 is 7 rem 3.
runner:case(rem, -3, arithmetic_moddiv, 'ISO 9.1.7, XLOG 2') :-
   0 is 0 rem(3+11).
runner:case(rem, -3, arithmetic_moddiv, 'ISO 9.1.7, XLOG 3') :-
   1 is 7 rem-2.
runner:case(rem, -3, arithmetic_moddiv, 'ISO 9.1.7, XLOG 4') :-
   -1 is -5 rem 2.
runner:case(rem, -3, arithmetic_moddiv, 'ISO 9.1.7, XLOG 5') :-
   catch(_ is 77 rem _, error(E,_), true),
   E == instantiation_error.
runner:case(rem, -3, arithmetic_moddiv, 'ISO 9.1.7, XLOG 6') :-
   catch(_ is 7 rem 0, error(E,_), true),
   E == evaluation_error(zero_divisor).
/* extra */
runner:case(rem, -3, arithmetic_moddiv, 'ISO 9.1.7, XLOG 7') :-
   -0.5 is -7.5 rem 3.5.

/* X div Y,  */

runner:ref(div, -3, arithmetic_moddiv, 'Corr.2 9.1.3').
runner:case(div, -3, arithmetic_moddiv, 'Corr.2 9.1.3, XLOG 1') :-
   0 is 7 div 35.
runner:case(div, -3, arithmetic_moddiv, 'Corr.2 9.1.3, XLOG 2') :-
   10 is 140 div(3+11).
runner:case(div, -3, arithmetic_moddiv, 'Corr.2 9.1.3, XLOG 3') :-
   -3 is -5 div 2.
runner:case(div, -3, arithmetic_moddiv, 'Corr.2 9.1.3, XLOG 4') :-
   catch(_ is foobar div 77, error(E,_), true),
   E == type_error(evaluable,foobar/0).
/* extra */
runner:case(div, -3, arithmetic_moddiv, 'Corr.2 9.1.3, XLOG 5') :-
   -3 is -7.5 div 3.5.

/* X mod Y */

runner:ref(mod, -3, arithmetic_moddiv, 'ISO 9.1.7').
runner:case(mod, -3, arithmetic_moddiv, 'ISO 9.1.7, ISO 30') :-
   1 is 7 mod 3.
runner:case(mod, -3, arithmetic_moddiv, 'ISO 9.1.7, ISO 31') :-
   0 is 0 mod(3+11).
runner:case(mod, -3, arithmetic_moddiv, 'ISO 9.1.7, ISO 32') :-
   -1 is 7 mod-2.
runner:case(mod, -3, arithmetic_moddiv, 'ISO 9.1.7, ISO 33') :-
   catch(_ is 77 mod _, error(E,_), true),
   E == instantiation_error.
runner:case(mod, -3, arithmetic_moddiv, 'ISO 9.1.7, ISO 34') :-
   catch(_ is foobar mod 77, error(E,_), true),
   E == type_error(evaluable,foobar/0).
runner:case(mod, -3, arithmetic_moddiv, 'ISO 9.1.7, XLOG 1') :-
   1.5 is 7.5 mod 2.
runner:case(mod, -3, arithmetic_moddiv, 'ISO 9.1.7, ISO 36') :-
   catch(_ is 7 mod 0, error(E,_), true),
   E == evaluation_error(zero_divisor).
/* extra */
runner:case(mod, -3, arithmetic_moddiv, 'ISO 9.1.7, XLOG 2') :-
   3.0 is -7.5 mod 3.5.
