/**
 * Prolog code for the arithmetic bitwise theory test cases.
 *
 * Source of test cases are the following standards:
 *   - Prolog General Core ISO/IUEC 13211-1
 *   - Draft Technical Corrigendum 1, WG17, Ulrich Neumerkel
 *     <a href="http://www.complang.tuwien.ac.at/ulrich/iso-prolog/dtc1">www.complang.tuwien.ac.at/ulrich/iso-prolog/dtc1</a>
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
/* Bitwise Operations                                           */
/****************************************************************/

/* \ X */

runner:ref(\, -2, arithmetic_bitwise, 'ISO 9.4.5.4').
runner:case(\, -2, arithmetic_bitwise, 'ISO 9.4.5.4, ISO 2') :-
   10 is \ \10.
runner:case(\, -2, arithmetic_bitwise, 'ISO 9.4.5.4, ISO 3') :-
   -11 is \10.
runner:case(\, -2, arithmetic_bitwise, 'ISO 9.4.5.4, ISO 4') :-
   catch(_ is \_, error(E,_), true),
   E == instantiation_error.
runner:case(\, -2, arithmetic_bitwise, 'ISO 9.4.5.4, ISO 5') :-
   catch(_ is \2.5, error(E,_), true),
   E == type_error(integer,2.5).

/* X /\ Y */

runner:ref(/\, -3, arithmetic_bitwise, 'ISO 9.4.3.4, Corrigendum 1').
runner:case(/\, -3, arithmetic_bitwise, 'ISO 9.4.3.4, ISO 2') :-
   8 is 10/\12.
runner:case(/\, -3, arithmetic_bitwise, 'ISO 9.4.3.4, ISO 3') :-
   125 is 17*256+125/\255.
runner:case(/\, -3, arithmetic_bitwise, 'ISO 9.4.3.4, ISO 4') :-
   4 is -10/\12.
runner:case(/\, -3, arithmetic_bitwise, 'ISO 9.4.3.4, ISO 6') :-
   catch(_ is foobar/\2, error(E,_), true),
   E == type_error(evaluable,foobar/0).

/* X \/ Y */

runner:ref(\/, -3, arithmetic_bitwise, 'ISO 9.4.4.4, Corrigendum 1').
runner:case(\/, -3, arithmetic_bitwise, 'ISO 9.4.4.4, ISO 2') :-
   14 is 10\/12.
runner:case(\/, -3, arithmetic_bitwise, 'ISO 9.4.4.4, ISO 3') :-
   255 is 125\/255.
runner:case(\/, -3, arithmetic_bitwise, 'ISO 9.4.4.4, ISO 4') :-
   -2 is -10\/12.
runner:case(\/, -3, arithmetic_bitwise, 'ISO 9.4.4.4, ISO 5') :-
   catch(_ is 77\/_, error(E,_), true),
   E == instantiation_error.

/* X << Y */

runner:ref(<<, -3, arithmetic_bitwise, 'ISO 9.4.2.4, Corrigendum 1').
runner:case(<<, -3, arithmetic_bitwise, 'ISO 9.4.2.4, ISO 1') :-
   64 is 16<<2.
runner:case(<<, -3, arithmetic_bitwise, 'ISO 9.4.2.4, ISO 2') :-
   76 is 19<<2.
runner:case(<<, -3, arithmetic_bitwise, 'ISO 9.4.2.4, ISO 3') :-
   -64 is -16<<2.
runner:case(<<, -3, arithmetic_bitwise, 'ISO 9.4.2.4, ISO 5') :-
   catch(_ is foobar<<2, error(E,_), true),
   E == type_error(evaluable,foobar/0).

/* X >> Y */

runner:ref(>>, -3, arithmetic_bitwise, 'ISO 9.4.1.4, Corrigendum 1').
runner:case(>>, -3, arithmetic_bitwise, 'ISO 9.4.1.4, ISO 1') :-
   4 is 16>>2.
runner:case(>>, -3, arithmetic_bitwise, 'ISO 9.4.1.4, ISO 2') :-
   4 is 19>>2.
runner:case(>>, -3, arithmetic_bitwise, 'ISO 9.4.1.4, ISO 3') :-
   -4 is -16>>2.
runner:case(>>, -3, arithmetic_bitwise, 'ISO 9.4.1.4, ISO 4') :-
   catch(_ is 77>>_, error(E,_), true),
   E == instantiation_error.

/* xor(X,Y) */

runner:ref(xor, -3, arithmetic_bitwise, 'Corr.2 9.4.6.4').
runner:case(xor, -3, arithmetic_bitwise, 'Corr.2 9.4.6.4, ISO 1') :-
   6 is 10 xor 12.
runner:case(xor, -3, arithmetic_bitwise, 'Corr.2 9.4.6.4, ISO 2') :-
   130 is 125 xor 255.
runner:case(xor, -3, arithmetic_bitwise, 'Corr.2 9.4.6.4, ISO 3') :-
   -6 is -10 xor 12.
