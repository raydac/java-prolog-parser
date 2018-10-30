/**
 * Prolog code for the arithmetic trigo theory test cases.
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
/* Trigonometric Operations                                     */
/****************************************************************/

/* X ** Y  */

runner:ref(**, -3, arithmetic_sincos, 'ISO 9.3.1.4').
runner:case(**, -3, arithmetic_sincos, 'ISO 9.3.1.4, ISO 1') :-
   125.0 is 5**3.
runner:case(**, -3, arithmetic_sincos, 'ISO 9.3.1.4, ISO 2') :-
   -125.0 is -5**3.
runner:case(**, -3, arithmetic_sincos, 'ISO 9.3.1.4, ISO 3') :-
   0.2 is 5** -1.
runner:case(**, -3, arithmetic_sincos, 'ISO 9.3.1.4, ISO 4') :-
   catch(_ is 77**_, error(E,_), true),
   E == instantiation_error.
runner:case(**, -3, arithmetic_sincos, 'ISO 9.3.1.4, ISO 6') :-
   125.0 is 5**3.0.
runner:case(**, -3, arithmetic_sincos, 'ISO 9.3.1.4, ISO 7') :-
   1.0 is 0.0**0.

/* sin(X) */

runner:ref(sin, -2, arithmetic_sincos, 'ISO 9.3.2.4').
runner:case(sin, -2, arithmetic_sincos, 'ISO 9.3.2.4, ISO 1') :-
   0.0 is sin(0.0).
runner:case(sin, -2, arithmetic_sincos, 'ISO 9.3.2.4, ISO 3') :-
   0.0 is sin(0).
runner:case(sin, -2, arithmetic_sincos, 'ISO 9.3.2.4, ISO 4') :-
   catch(_ is sin(foobar), error(E,_), true),
   E == type_error(evaluable,foobar/0).
runner:case(sin, -2, arithmetic_sincos, 'ISO 9.3.2.4, ISO 5') :-
   1.0 is sin(pi/2.0).
runner:case(sin, -2, arithmetic_sincos, 'ISO 9.3.2.4, XLOG 1') :-
   0.7457052121767203 is sin(2.3).

/* cos(X) */

runner:ref(cos, -2, arithmetic_sincos, 'ISO 9.3.3.4').
runner:case(cos, -2, arithmetic_sincos, 'ISO 9.3.3.4, ISO 1') :-
   1.0 is cos(0.0).
runner:case(cos, -2, arithmetic_sincos, 'ISO 9.3.3.4, ISO 2') :-
   catch(_ is cos(_), error(E,_), true),
   E == instantiation_error.
runner:case(cos, -2, arithmetic_sincos, 'ISO 9.3.3.4, ISO 3') :-
   1.0 is cos(0).
runner:case(cos, -2, arithmetic_sincos, 'ISO 9.3.3.4, ISO 5') :-
   6.123233995736766e-17 is cos(pi/2.0).
runner:case(cos, -2, arithmetic_sincos, 'ISO 9.3.3.4, XLOG 1') :-
   -0.666276021279824 is cos(2.3).

/* tan(X) */

runner:ref(tan, -2, arithmetic_sincos, 'Corr.2 9.3.14.4').
runner:case(tan, -2, arithmetic_sincos, 'Corr.2 9.3.14.4, XLOG 1') :-
   X is tan(pi/6),
   1/3 =:= X*X.
runner:case(tan, -2, arithmetic_sincos, 'Corr.2 9.3.14.4, XLOG 2') :-
   0.9999999999999999 is tan(pi/4).
runner:case(tan, -2, arithmetic_sincos, 'Corr.2 9.3.14.4, XLOG 3') :-
   catch(_ is tan(foobar), error(E,_), true),
   E == type_error(evaluable,foobar/0).
runner:case(tan, -2, arithmetic_sincos, 'Corr.2 9.3.14.4, XLOG 4') :-
   -1.1192136417341325 is tan(2.3).

/* asin(X) */

runner:ref(asin, -2, arithmetic_sincos, 'Corr.2 9.3.11.4').
runner:case(asin, -2, arithmetic_sincos, 'Corr.2 9.3.11.4, XLOG 1') :-
   0.0 is asin(0).
runner:case(asin, -2, arithmetic_sincos, 'Corr.2 9.3.11.4, XLOG 2') :-
   pi =:= asin(sqrt(3/4))*3.
runner:case(asin, -2, arithmetic_sincos, 'Corr.2 9.3.11.4, XLOG 3') :-
   pi/2 =:= asin(1).
runner:case(asin, -2, arithmetic_sincos, 'Corr.2 9.3.11.4, XLOG 4') :-
   catch(_ is asin(_), error(E,_), true),
   E == instantiation_error.
runner:case(asin, -2, arithmetic_sincos, 'Corr.2 9.3.11.4, ISO 3') :-
   catch(_ is asin(2), error(E,_), true),
   E == evaluation_error(undefined).

/* acos(X) */

runner:ref(acos, -2, arithmetic_sincos, 'Corr.2 9.3.12.4').
runner:case(acos, -2, arithmetic_sincos, 'Corr.2 9.3.12.4, XLOG 1') :-
   pi/2 =:= acos(0).
runner:case(acos, -2, arithmetic_sincos, 'Corr.2 9.3.12.4, XLOG 2') :-
   3.1415926535897936 is acos(sqrt(3/4))*6.
runner:case(acos, -2, arithmetic_sincos, 'Corr.2 9.3.12.4, XLOG 3') :-
   0.0 is acos(1).
runner:case(acos, -2, arithmetic_sincos, 'Corr.2 9.3.12.4, XLOG 4') :-
   catch(_ is acos(foobar), error(E,_), true),
   E == type_error(evaluable,foobar/0).
runner:case(acos, -2, arithmetic_sincos, 'Corr.2 9.3.12.4, ISO 3') :-
   catch(_ is acos(1.5), error(E,_), true),
   E == evaluation_error(undefined).

/* atan(X) */

runner:ref(atan, -2, arithmetic_sincos, 'ISO 9.3.4.4').
runner:case(atan, -2, arithmetic_sincos, 'ISO 9.3.4.4, ISO 1') :-
   0.0 is atan(0.0).
runner:case(atan, -2, arithmetic_sincos, 'ISO 9.3.4.4, ISO 2') :-
   pi =:= atan(1.0)*4.
runner:case(atan, -2, arithmetic_sincos, 'ISO 9.3.4.4, ISO 3') :-
   catch(_ is atan(_), error(E,_), true),
   E == instantiation_error.
runner:case(atan, -2, arithmetic_sincos, 'ISO 9.3.4.4, ISO 4') :-
   0.0 is atan(0).

/* exp(X) */

runner:ref(exp, -2, arithmetic_sincos, 'ISO 9.3.5.4').
runner:case(exp, -2, arithmetic_sincos, 'ISO 9.3.5.4, ISO 1') :-
   1.0 is exp(0.0).
runner:case(exp, -2, arithmetic_sincos, 'ISO 9.3.5.4, ISO 2') :-
   2.7182818284590455 is exp(1.0)
;  2.718281828459045 is exp(1.0).
runner:case(exp, -2, arithmetic_sincos, 'ISO 9.3.5.4, ISO 4') :-
   1.0 is exp(0).
runner:case(exp, -2, arithmetic_sincos, 'ISO 9.3.5.4, ISO 5') :-
   catch(_ is exp(foobar), error(E,_), true),
   E == type_error(evaluable,foobar/0).

/* log(X) */

runner:ref(log, -2, arithmetic_sincos, 'ISO 9.3.6.4').
runner:case(log, -2, arithmetic_sincos, 'ISO 9.3.6.4, ISO 1') :-
   0.0 is log(1.0).
runner:case(log, -2, arithmetic_sincos, 'ISO 9.3.6.4, ISO 2') :-
   1.0 is log(e).
runner:case(log, -2, arithmetic_sincos, 'ISO 9.3.6.4, ISO 3') :-
   catch(_ is log(_), error(E,_), true),
   E == instantiation_error.
runner:case(log, -2, arithmetic_sincos, 'ISO 9.3.6.4, ISO 4') :-
   catch(_ is log(0), error(E,_), true),
   nonvar(E),
   E = evaluation_error(_).
runner:case(log, -2, arithmetic_sincos, 'ISO 9.3.6.4, ISO 6') :-
   catch(_ is log(0.0), error(E,_), true),
   nonvar(E),
   E = evaluation_error(_).
runner:case(log, -2, arithmetic_sincos, 'ISO 9.3.6.4, ISO 7') :-
   catch(_ is log(-1), error(E,_), true),
   nonvar(E),
   E = evaluation_error(undefined).

/* sqrt(X) */

runner:ref(sqrt, -2, arithmetic_sincos, 'ISO 9.3.7.4').
runner:case(sqrt, -2, arithmetic_sincos, 'ISO 9.3.7.4, ISO 1') :-
   0.0 is sqrt(0.0).
runner:case(sqrt, -2, arithmetic_sincos, 'ISO 9.3.7.4, ISO 2') :-
   1.0 is sqrt(1).
runner:case(sqrt, -2, arithmetic_sincos, 'ISO 9.3.7.4, ISO 3') :-
   1.1 is sqrt(1.21).
runner:case(sqrt, -2, arithmetic_sincos, 'ISO 9.3.7.4, ISO 5') :-
   catch(_ is sqrt(-1.0), error(E,_), true),
   E == evaluation_error(undefined).
runner:case(sqrt, -2, arithmetic_sincos, 'ISO 9.3.7.4, ISO 6') :-
   catch(_ is sqrt(foobar), error(E,_), true),
   E == type_error(evaluable,foobar/0).

/* pi */

runner:ref(pi, -1, arithmetic_sincos, 'Corr.2 9.3.15.4').
runner:case(pi, -1, arithmetic_sincos, 'Corr.2 9.3.15.4, ISO 1') :-
   3.141592653589793 is pi.

/* atan2 */

runner:ref(atan2, -3, arithmetic_sincos, 'Corr.2 9.3.13').
runner:case(atan2, -3, arithmetic_sincos, 'Corr.2 9.3.13, ISO 1') :-
   0.0 is atan2(1,0)-pi/2.
runner:case(atan2, -3, arithmetic_sincos, 'Corr.2 9.3.13, ISO 2') :-
   0.0 is atan2(0,-1)-pi.
runner:case(atan2, -3, arithmetic_sincos, 'Corr.2 9.3.13, ISO 3') :-
   catch(_ is atan2(0,0), error(E,_), true),
   E == evaluation_error(undefined).
