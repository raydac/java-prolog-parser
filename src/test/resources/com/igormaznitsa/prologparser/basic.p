/**
 * Prolog code for the arithmetic basic theory test cases.
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
/* Basic Operations                                             */
/****************************************************************/

/* X is Y */

runner:ref(is, 2, arithmetic_basic, 'ISO 8.6.1.4').
runner:case(is, 2, arithmetic_basic, 'ISO 8.6.1.4, ISO 1') :-
   Result is 3+11.0,
   Result == 14.0.
runner:case(is, 2, arithmetic_basic, 'ISO 8.6.1.4, ISO 2') :-
   X = 1+2,
   Y is X*3,
   X == 1+2,
   Y == 9.
runner:case(is, 2, arithmetic_basic, 'ISO 8.6.1.4, ISO 3') :-
   3 is 3.
runner:case(is, 2, arithmetic_basic, 'ISO 8.6.1.4, ISO 4') :-
   \+ 3 is 3.0.
runner:case(is, 2, arithmetic_basic, 'ISO 8.6.1.4, ISO 5') :-
   \+ foobar is 77.
runner:case(is, 2, arithmetic_basic, 'ISO 8.6.1.4, ISO 6') :-
   catch(77 is _, error(E,_), true),
   E == instantiation_error.

/* - X */

runner:ref(-, -2, arithmetic_basic, 'ISO 9.1.7').
runner:case(-, -2, arithmetic_basic, 'ISO 9.1.7, ISO 6') :-
   -7 is -7.
runner:case(-, -2, arithmetic_basic, 'ISO 9.1.7, ISO 7') :-
   8 is - (3-11).
runner:case(-, -2, arithmetic_basic, 'ISO 9.1.7, ISO 8') :-
   7.8 is - (3.2-11).
runner:case(-, -2, arithmetic_basic, 'ISO 9.1.7, ISO 9') :-
   catch(_ is -_, error(E,_), true),
   E == instantiation_error.
runner:case(-, -2, arithmetic_basic, 'ISO 9.1.7, ISO 10') :-
   catch(_ is -foobar, error(E,_), true),
   E == type_error(evaluable,foobar/0).

/* abs(X) */

runner:ref(abs, -2, arithmetic_basic, 'ISO 9.1.7').
runner:case(abs, -2, arithmetic_basic, 'ISO 9.1.7, ISO 52') :-
   7 is abs(7).
runner:case(abs, -2, arithmetic_basic, 'ISO 9.1.7, ISO 53') :-
   8 is abs(3-11).
runner:case(abs, -2, arithmetic_basic, 'ISO 9.1.7, ISO 54') :-
   7.8 is abs(3.2-11.0).
runner:case(abs, -2, arithmetic_basic, 'ISO 9.1.7, ISO 55') :-
   catch(_ is abs(_), error(E,_), true),
   E == instantiation_error.

/* sign(X) */

runner:ref(sign, -2, arithmetic_basic, 'ISO 9.1.4').
runner:case(sign, -2, arithmetic_basic, 'ISO 9.1.4, XLOG 1') :-
   1 is sign(7).
runner:case(sign, -2, arithmetic_basic, 'ISO 9.1.4, XLOG 2') :-
   -1 is sign(3-11).
runner:case(sign, -2, arithmetic_basic, 'ISO 9.1.4, XLOG 3') :-
   -1.0 is sign(3.2-11).
runner:case(sign, -2, arithmetic_basic, 'ISO 9.1.4, XLOG 5') :-
   catch(_ is sign(foobar), error(E,_), true),
   E == type_error(evaluable,foobar/0).

/* float(X) */

runner:ref(float, -2, arithmetic_basic, 'ISO 9.1.7').
runner:case(float, -2, arithmetic_basic, 'ISO 9.1.7, ISO 47') :-
   7.0 is float(7).
runner:case(float, -2, arithmetic_basic, 'ISO 9.1.7, ISO 48') :-
   7.3 is float(7.3).
runner:case(float, -2, arithmetic_basic, 'ISO 9.1.7, ISO 49') :-
   1.0 is float(5//3).

/* X + Y */

runner:ref(+, -3, arithmetic_basic, 'ISO 9.1.7').
runner:case(+, -3, arithmetic_basic, 'ISO 9.1.7, ISO 1') :-
   42 is 7+35.
runner:case(+, -3, arithmetic_basic, 'ISO 9.1.7, ISO 2') :-
   14 is 0+(3+11).
runner:case(+, -3, arithmetic_basic, 'ISO 9.1.7, ISO 3') :-
   14.2 is 0+(3.2+11).
runner:case(+, -3, arithmetic_basic, 'ISO 9.1.7, ISO 5') :-
   catch(_ is foobar+77, error(E,_), true),
   E == type_error(evaluable,foobar/0).
% runner:case(+, -3, arithmetic_basic, iso) :- current_prolog_flag(max_integer, M),
%    catch(_ is M+1, error(E, _), true), E==evaluation_error(int_overflow).
% runner:case(+, -3, arithmetic_basic, iso) :- current_prolog_flag(max_integer, M),
%    catch(_ is (M+1)-1, error(E, _), true), E==evaluation_error(int_overflow).

/* X - Y */

runner:ref(-, -3, arithmetic_basic, 'ISO 9.1.7').
runner:case(-, -3, arithmetic_basic, 'ISO 9.1.7, ISO 11') :-
   -28 is 7-35.
runner:case(-, -3, arithmetic_basic, 'ISO 9.1.7, ISO 12') :-
   6 is 20-(3+11).
runner:case(-, -3, arithmetic_basic, 'ISO 9.1.7, ISO 13') :-
   -14.2 is 0-(3.2+11).
runner:case(-, -3, arithmetic_basic, 'ISO 9.1.7, ISO 14') :-
   catch(_ is 77-_, error(E,_), true),
   E == instantiation_error.
% runner:case(-, -3, arithmetic_basic, iso) :- current_prolog_flag(max_integer, M),
%    catch(_ is (-1)-M, error(E, _), true), E==evaluation_error(int_overflow).

/* X * Y */

runner:ref(*, -3, arithmetic_basic, 'ISO 9.1.7').
runner:case(*, -3, arithmetic_basic, 'ISO 9.1.7, ISO 16') :-
   245 is 7*35.
runner:case(*, -3, arithmetic_basic, 'ISO 9.1.7, ISO 17') :-
   0 is 0*(3+11).
runner:case(*, -3, arithmetic_basic, 'ISO 9.1.7, ISO 18') :-
   21.299999999999997 is 1.5*(3.2+11).
runner:case(*, -3, arithmetic_basic, 'ISO 9.1.7, ISO 19') :-
   catch(_ is 77*_, error(E,_), true),
   E == instantiation_error.
% runner:case(*, -3, arithmetic_basic, iso) :- current_prolog_flag(max_integer, M),
%    catch(_ is M*2, error(E, _), true), E==evaluation_error(int_overflow).

/* X / Y */

runner:ref(/, -3, arithmetic_basic, 'ISO 9.1.7, Corrigendum 1').
runner:case(/, -3, arithmetic_basic, 'ISO 9.1.7, ISO 22') :-
   0.2 is 7.0/35.
runner:case(/, -3, arithmetic_basic, 'ISO 9.1.7, ISO 24') :-
   1.4200000000000002 is 20.164/(3.2+11).
runner:case(/, -3, arithmetic_basic, 'ISO 9.1.7, ISO 25') :-
   -2.3333333333333335 is 7/ -3.
runner:case(/, -3, arithmetic_basic, 'ISO 9.1.7, ISO 26') :-
   -2.3333333333333335 is -7/3.
runner:case(/, -3, arithmetic_basic, 'ISO 9.1.7, ISO 28') :-
   catch(_ is foobar/77, error(E,_), true),
   E == type_error(evaluable,foobar/0).
runner:case(/, -3, arithmetic_basic, 'ISO 9.1.7, ISO 29') :-
   catch(_ is 3/0, error(E,_), true),
   E == evaluation_error(zero_divisor).

/* X^Y */

runner:ref(^, -3, arithmetic_basic, 'Corr.2 9.3.10.4').
runner:case(^, -3, arithmetic_basic, 'Corr.2 9.3.10.4, ISO 1') :-
   1 is 0^0.
runner:case(^, -3, arithmetic_basic, 'Corr.2 9.3.10.4, XLOG 1') :-
   catch(_ is 3^1.0, error(E,_), true),
   E = type_error(integer,_).
runner:case(^, -3, arithmetic_basic, 'Corr.2 9.3.10.4, ISO 3') :-
   -27 is -3^3.
runner:case(^, -3, arithmetic_basic, 'Corr.2 9.3.10.4, ISO 4') :-
   7625597484987 is 3^27.
runner:case(^, -3, arithmetic_basic, 'Corr.2 9.3.10.4, ISO 5') :-
   7625597484987 is 3^3^3.
runner:case(^, -3, arithmetic_basic, 'Corr.2 9.3.10.4, XLOG 2') :-
   catch(_ is 1^ -1, error(E,_), true),
   E == representation_error(not_less_than_zero).
runner:case(^, -3, arithmetic_basic, 'Corr.2 9.3.10.4, XLOG 3') :-
   catch(_ is 2^ -1.5, error(E,_), true),
   E = type_error(integer,_).
runner:case(^, -3, arithmetic_basic, 'Corr.2 9.3.10.4, XLOG 4') :-
   9.0 is 3.0^2.
runner:case(^, -3, arithmetic_basic, 'Corr.2 9.3.10.4, XLOG 5') :-
   -27.0 is -3.0^3.
