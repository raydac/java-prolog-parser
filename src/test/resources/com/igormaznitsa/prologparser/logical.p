/**
 * Prolog code for the control logical theory test cases.
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

:- use_module(library(system/charsio)).

/****************************************************************/
/* Logical Predicates                                           */
/****************************************************************/

/* A, B */

runner:ref(',', 2, control_logical, 'ISO 7.8.5.4').
runner:case(',', 2, control_logical, 'ISO 7.8.5.4, ISO 1') :-
   \+ (  X = 1,
         var(X)).
runner:case(',', 2, control_logical, 'ISO 7.8.5.4, ISO 2') :-
   var(X),
   X = 1,
   X == 1.
runner:case(',', 2, control_logical, 'ISO 7.8.5.4, ISO 3') :-
   X = true,
   call(X).

/* A; B */

runner:ref(;, 2, control_logical, 'ISO 7.8.6.4').
runner:case(;, 2, control_logical, 'ISO 7.8.6.4, ISO 1') :- true; fail.
runner:case(;, 2, control_logical, 'ISO 7.8.6.4, ISO 2') :-
   \+ (  !, fail; true).
runner:case(;, 2, control_logical, 'ISO 7.8.6.4, ISO 3') :- !
;  call(3).
runner:case(;, 2, control_logical, 'ISO 7.8.6.4, ISO 4a') :-
   (  X = 1, !
   ;  X = 2),
   X == 1.
runner:case(;, 2, control_logical, 'ISO 7.8.6.4, ISO 4b') :-
   findall(X, (  X = 1, !
              ;  X = 2), [_]).
runner:case(;, 2, control_logical, 'ISO 7.8.6.4, ISO 5a') :-
   (  X = 1
   ;  X = 2),
   (  true; !),
   X == 1.
runner:case(;, 2, control_logical, 'ISO 7.8.6.4, ISO 5b') :-
   findall(X, (  (  X = 1
                 ;  X = 2),
                 (  true; !)), [_,X|_]),
   X == 1.
runner:case(;, 2, control_logical, 'ISO 7.8.6.4, ISO 5c') :-
   findall(X, (  (  X = 1
                 ;  X = 2),
                 (  true; !)), [_,_]).

/* A -> B */

runner:ref(->, 2, control_logical, 'ISO 7.8.7.4').
runner:case(->, 2, control_logical, 'ISO 7.8.7.4, ISO 1') :- true -> true.
runner:case(->, 2, control_logical, 'ISO 7.8.7.4, ISO 2') :-
   \+ (  true -> fail).
runner:case(->, 2, control_logical, 'ISO 7.8.7.4, ISO 3') :-
   \+ (  fail -> true).
runner:case(->, 2, control_logical, 'ISO 7.8.7.4, ISO 4a') :-
   (  true
   -> X = 1),
   X == 1.
runner:case(->, 2, control_logical, 'ISO 7.8.7.4, ISO 4b') :-
   findall(X, (  true
              -> X = 1), [_]).
runner:case(->, 2, control_logical, 'ISO 7.8.7.4, ISO 5a') :-
   (  (  X = 1
      ;  X = 2) -> true),
   X == 1.
runner:case(->, 2, control_logical, 'ISO 7.8.7.4, ISO 5b') :-
   findall(X, (  (  X = 1
                 ;  X = 2) -> true), [_]).
runner:case(->, 2, control_logical, 'ISO 7.8.7.4, ISO 6a') :-
   (  true
   -> (  X = 1
      ;  X = 2)),
   X == 1.
runner:case(->, 2, control_logical, 'ISO 7.8.7.4, ISO 6b') :-
   findall(X, (  true
              -> (  X = 1
                 ;  X = 2)), [_,X|_]),
   X == 2.
runner:case(->, 2, control_logical, 'ISO 7.8.7.4, ISO 6c') :-
   findall(X, (  true
              -> (  X = 1
                 ;  X = 2)), [_,_]).

/* A -> B ; C */

runner:ref(if_then_else, 3, control_logical, 'ISO 7.8.8.4, Corrigendum 1').
runner:case(if_then_else, 3, control_logical, 'ISO 7.8.8.4, ISO 1') :-
   true -> true; fail.
runner:case(if_then_else, 3, control_logical, 'ISO 7.8.8.4, ISO 2') :-
   fail -> true; true.
runner:case(if_then_else, 3, control_logical, 'ISO 7.8.8.4, ISO 3') :-
   \+ (  true -> fail; fail).
runner:case(if_then_else, 3, control_logical, 'ISO 7.8.8.4, ISO 4') :-
   \+ (  fail -> true; fail).
runner:case(if_then_else, 3, control_logical, 'ISO 7.8.8.4, ISO 5') :-
   (  true
   -> X = 1
   ;  X = 2),
   X == 1.
runner:case(if_then_else, 3, control_logical, 'ISO 7.8.8.4, ISO 6') :-
   (  fail
   -> X = 1
   ;  X = 2),
   X == 2.
runner:case(if_then_else, 3, control_logical, 'ISO 7.8.8.4, ISO 7a') :-
   (  true
   -> (  X = 1
      ;  X = 2); true),
   X == 1.
runner:case(if_then_else, 3, control_logical, 'ISO 7.8.8.4, ISO 7b') :-
   findall(X, (  true
              -> (  X = 1
                 ;  X = 2); true), [_,X|_]),
   X == 2.
runner:case(if_then_else, 3, control_logical, 'ISO 7.8.8.4, ISO 7c') :-
   findall(X, (  true
              -> (  X = 1
                 ;  X = 2); true), [_,_]).
runner:case(if_then_else, 3, control_logical, 'ISO 7.8.8.4, ISO 8a') :-
   (  (  X = 1
      ;  X = 2) -> true; true),
   X == 1.
runner:case(if_then_else, 3, control_logical, 'ISO 7.8.8.4, ISO 8b') :-
   findall(X, (  (  X = 1
                 ;  X = 2) -> true; true), [_]).
runner:case(if_then_else, 3, control_logical, 'ISO 7.8.8.4, ISO 9') :-
   (  ! -> fail), true; true.

/* \+ A */

runner:ref(\+, 1, control_logical, 'ISO 8.15.1.4').
runner:case(\+, 1, control_logical, 'ISO 8.15.1.4, ISO 1') :-
   \+ \+ true.
runner:case(\+, 1, control_logical, 'ISO 8.15.1.4, ISO 2') :-
   \+ \+ !.
runner:case(\+, 1, control_logical, 'ISO 8.15.1.4, ISO 3') :-
   \+ (  !, fail).
runner:case(\+, 1, control_logical, 'ISO 8.15.1.4, ISO 4') :-
   \+ 4 = 5.
runner:case(\+, 1, control_logical, 'ISO 8.15.1.4, ISO 5') :-
   catch(\+ 3, error(E,_), true),
   E == type_error(callable,3).
runner:case(\+, 1, control_logical, 'ISO 8.15.1.4, ISO 6') :-
   catch(\+ _, error(E,_), true),
   E == instantiation_error.
% runner:case(\+, 1, control_logical, 'ISO 8.15.1.4, ISO 7') :- \+ (X = f(X)).

/* repeat */

runner:ref(repeat, 0, control_logical, 'ISO 8.15.3.4').
% runner:case(repeat, 0, control_logical, 'ISO 8.15.3.4, ISO 1') :- repeat, write('hello '), fail.
runner:case(repeat, 0, control_logical, 'ISO 8.15.3.4, ISO 2') :-
   \+ (  repeat, !, fail).

/* once(A) */

runner:ref(once, 1, control_logical, 'ISO 8.15.2.4').
runner:case(once, 1, control_logical, 'ISO 8.15.2.4, ISO 1') :-
   once(!).
runner:case(once, 1, control_logical, 'ISO 8.15.2.4, ISO 2a') :-
   once(!),
   (  X = 1
   ;  X = 2),
   X == 1.
runner:case(once, 1, control_logical, 'ISO 8.15.2.4, ISO 2b') :-
   findall(X, (  once(!),
                 (  X = 1
                 ;  X = 2)), [_,X|_]),
   X == 2.
runner:case(once, 1, control_logical, 'ISO 8.15.2.4, ISO 2c') :-
   findall(X, (  once(!),
                 (  X = 1
                 ;  X = 2)), [_,_]).
runner:case(once, 1, control_logical, 'ISO 8.15.2.4, ISO 3') :-
   once(repeat).
runner:case(once, 1, control_logical, 'ISO 8.15.2.4, ISO 4') :-
   \+ once(fail).
% runner:case(once, 1, control_logical, 'ISO 8.15.2.4, ISO 5') :- once(X = f(X)).

/* halt */

/* call(A) */

runner:ref(call, 1, control_logical, 'ISO 7.8.3.4, Corr.2 7.8.3.4').

b(X) :-
   Y = (write(X),X),
   call(Y).

a(1).
a(2).

runner:case(call, 1, control_logical, 'ISO 7.8.3.4, ISO 1') :-
   call(!).
runner:case(call, 1, control_logical, 'ISO 7.8.3.4, ISO 2') :-
   \+ call(fail).
runner:case(call, 1, control_logical, 'ISO 7.8.3.4, ISO 3') :-
   \+ call((  fail, _)).
runner:case(call, 1, control_logical, 'ISO 7.8.3.4, ISO 4') :-
   \+ call((  fail,
              call(1))).
runner:case(call, 1, control_logical, 'ISO 7.8.3.4, ISO 5a') :-
   with_output_to(atom(A), catch(b(_), error(_,_), true)), !,
   A = '_B'.
runner:case(call, 1, control_logical, 'ISO 7.8.3.4, ISO 5b') :-
   with_output_to(atom(_), catch(b(_), error(E,_), true)),
   nonvar(E),
   E = instantiation_error.
runner:case(call, 1, control_logical, 'ISO 7.8.3.4, ISO 6a') :-
   with_output_to(atom(A), catch(b(3), error(_,_), true)), !,
   A = ''.
runner:case(call, 1, control_logical, 'ISO 7.8.3.4, ISO 6b') :-
   with_output_to(atom(_), catch(b(3), error(E,_), true)),
   nonvar(E),
   E = type_error(callable,_).
runner:case(call, 1, control_logical, 'ISO 7.8.3.4, ISO 7a') :-
   Z = !,
   call((  Z = !,
           a(X), Z)),
   X == 1,
   Z == !.
runner:case(call, 1, control_logical, 'ISO 7.8.3.4, ISO 7b') :-
   findall(Z-X, (  Z = !,
                   call((  Z = !,
                           a(X), Z))), [_]).
runner:case(call, 1, control_logical, 'ISO 7.8.3.4, ISO 8a') :-
   call((  Z = !,
           a(X), Z)),
   X == 1,
   Z == !.
runner:case(call, 1, control_logical, 'ISO 7.8.3.4, ISO 8b') :-
   findall(Z-X, call((  Z = !,
                        a(X), Z)), [_,Z-X|_]),
   X == 2,
   Z == !.
runner:case(call, 1, control_logical, 'ISO 7.8.3.4, ISO 8c') :-
   findall(Z-X, call((  Z = !,
                        a(X), Z)), [_,_]).
runner:case(call, 1, control_logical, 'ISO 7.8.3.4, ISO 9a') :-
   with_output_to(atom(A), catch(call((  write(3), _)), error(_,_), true)), !,
   A = '3'.
runner:case(call, 1, control_logical, 'ISO 7.8.3.4, ISO 9b') :-
   with_output_to(atom(_), catch(call((  write(3), _)), error(E,_), true)),
   nonvar(E),
   E = instantiation_error.
runner:case(call, 1, control_logical, 'ISO 7.8.3.4, ISO 10a') :-
   with_output_to(atom(A), catch(call((  write(3),
                                         call(1))), error(_,_), true)), !,
   A = '3'.
runner:case(call, 1, control_logical, 'ISO 7.8.3.4, ISO 10b') :-
   with_output_to(atom(_), catch(call((  write(3),
                                         call(1))), error(E,_), true)),
   nonvar(E),
   E = type_error(callable,_).
runner:case(call, 1, control_logical, 'ISO 7.8.3.4, ISO 11') :-
   catch(call(_), error(E,_), true),
   nonvar(E),
   E = instantiation_error.
runner:case(call, 1, control_logical, 'ISO 7.8.3.4, ISO 12') :-
   catch(call(1), error(E,_), true),
   nonvar(E),
   E = type_error(callable,_).
runner:case(call, 1, control_logical, 'ISO 7.8.3.4, ISO 13') :-
   catch(call((  fail, 1)), error(E,_), true),
   nonvar(E),
   E = type_error(callable,_).
runner:case(call, 1, control_logical, 'ISO 7.8.3.4, ISO 14') :-
   with_output_to(atom(_), catch(call((  write(3), 1)), error(E,_), true)),
   nonvar(E),
   E = type_error(callable,_).
runner:case(call, 1, control_logical, 'ISO 7.8.3.4, ISO 15') :-
   catch(call((  1; true)), error(E,_), true),
   nonvar(E),
   E = type_error(callable,_).
