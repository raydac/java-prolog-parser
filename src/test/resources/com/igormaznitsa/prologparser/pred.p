/**
 * Prolog code for the control pred theory test cases.
 *
 * Source of test cases are the following standards:
 *   - Prolog General Core ISO/IUEC 13211-1
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
/* Predicate Definitions                                        */
/****************************************************************/

/* current_predicate(P) */

runner:ref(current_predicate, 1, control_pred, 'ISO 8.8.2.4').

:- multifile dog/0.
:- dynamic dog/0.
dog.

runner:case(current_predicate, 1, control_pred, 'ISO 8.8.2.4, ISO 1') :-
   current_predicate(dog/0).
runner:case(current_predicate, 1, control_pred, 'ISO 8.8.2.4, ISO 2') :-
   \+ current_predicate(current_predicate/1).

elk(X) :-
   moose(X).

runner:case(current_predicate, 1, control_pred, 'ISO 8.8.2.4, ISO 3') :-
   current_predicate(elk/A),
   A == 1.
runner:case(current_predicate, 1, control_pred, 'ISO 8.8.2.4, ISO 4') :-
   \+ current_predicate(ulk/_).

:- multifile insect/1.
:- dynamic insect/1.
insect(ant).
insect(bee).

runner:case(current_predicate, 1, control_pred, 'ISO 8.8.2.4, ISO 5a') :-
   current_predicate(N/1),
   (  N == elk
   ;  N == insect).
runner:case(current_predicate, 1, control_pred, 'ISO 8.8.2.4, ISO 5b') :-
   current_predicate(N/1),
   (  N == elk
   ;  N == insect),
   current_predicate(M/1),
   M \== N,
   (  M == elk
   ;  M == insect).
runner:case(current_predicate, 1, control_pred, 'ISO 8.8.2.4, ISO 6') :-
   catch(current_predicate(4), error(E,_), true),
   E == type_error(predicate_indicator,4).

/* predicate_property(P, Q) */

