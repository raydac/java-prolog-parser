/**
 * Prolog code for the control kernel theory test cases.
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
/* Kernel Predicates                                            */
/****************************************************************/

/* fail */

runner:ref(fail, 0, control_kernel, 'ISO 7.8.2.4').
runner:case(fail, 0, control_kernel, 'ISO 7.8.2.4, ISO 1') :-
   \+ fail.

/* true */

runner:ref(true, 0, control_kernel, 'ISO 7.8.1.4').
runner:case(true, 0, control_kernel, 'ISO 7.8.1.4, ISO 1') :- true.

/* ! */

runner:ref(!, 0, control_kernel, 'ISO 7.8.4.4').
runner:case(!, 0, control_kernel, 'ISO 7.8.4.4, ISO 1') :- !.
runner:case(!, 0, control_kernel, 'ISO 7.8.4.4, ISO 2') :-
   \+ (  !, fail; true).
runner:case(!, 0, control_kernel, 'ISO 7.8.4.4, ISO 3') :-
   call(!), fail; true.

:- meta_predicate twice(0).
twice(!) :-
   write('C ').
twice(true) :-
   write('Moss ').

runner:case(!, 0, control_kernel, 'ISO 7.8.4.4, ISO 4a') :-
   with_output_to(atom(X), (  twice(_), !,
                              write('Forwards '))), !,
   X == 'C Forwards '.
runner:case(!, 0, control_kernel, 'ISO 7.8.4.4, ISO 4b') :-
   \+ with_output_to(atom(_), (  twice(_), !,
                                 write('Forwards '), fail)).
runner:case(!, 0, control_kernel, 'ISO 7.8.4.4, ISO 5a') :-
   with_output_to(atom(X), (  (  !
                              ;  write('No ')),
                              write('Cut disjunction '))), !,
   X == 'Cut disjunction '.
runner:case(!, 0, control_kernel, 'ISO 7.8.4.4, ISO 5b') :-
   \+ with_output_to(atom(_), (  (  !
                                 ;  write('No ')),
                                 write('Cut disjunction '), fail)).
runner:case(!, 0, control_kernel, 'ISO 7.8.4.4, ISO 6a') :-
   with_output_to(atom(X), (  twice(_),
                              (  write('No '); !),
                              write('Cut '))), !,
   X == 'C No Cut '.
runner:case(!, 0, control_kernel, 'ISO 7.8.4.4, ISO 6b') :-
   findall(X, with_output_to(atom(X), (  twice(_),
                                         (  write('No '); !),
                                         write('Cut '))), [_,X|_]),
   X == 'Cut '.
runner:case(!, 0, control_kernel, 'ISO 7.8.4.4, ISO 6c') :-
   findall(X, with_output_to(atom(X), (  twice(_),
                                         (  write('No '); !),
                                         write('Cut '))), [_,_]).
runner:case(!, 0, control_kernel, 'ISO 7.8.4.4, ISO 7a') :-
   with_output_to(atom(X), \+ (  twice(_),
                                 (  !, fail
                                 ;  write('No ')))), !,
   X == 'C '.
runner:case(!, 0, control_kernel, 'ISO 7.8.4.4, ISO 7b') :-
   \+ (  twice(_),
         (  !, fail
         ;  write('No '))).
runner:case(!, 0, control_kernel, 'ISO 7.8.4.4, ISO 8a') :-
   with_output_to(atom(Y), (  twice(X),
                              call(X),
                              write('Forwards '))), !,
   Y == 'C Forwards '.
runner:case(!, 0, control_kernel, 'ISO 7.8.4.4, ISO 8b') :-
   findall(Y, with_output_to(atom(Y), (  twice(X),
                                         call(X),
                                         write('Forwards '))), [_,Y|_]),
   Y == 'Moss Forwards '.
runner:case(!, 0, control_kernel, 'ISO 7.8.4.4, ISO 8c') :-
   findall(Y, with_output_to(atom(Y), (  twice(X),
                                         call(X),
                                         write('Forwards '))), [_,_]).

:- meta_predicate goal(0).
goal((  twice(_), !)).
goal(write('Three ')).

runner:case(!, 0, control_kernel, 'ISO 7.8.4.4, ISO 9a') :-
   with_output_to(atom(Y), (  goal(X),
                              call(X),
                              write('Forwards '))), !,
   Y == 'C Forwards '.
runner:case(!, 0, control_kernel, 'ISO 7.8.4.4, ISO 9b') :-
   findall(Y, with_output_to(atom(Y), (  goal(X),
                                         call(X),
                                         write('Forwards '))), [_,Y|_]),
   Y == 'Three Forwards '.
runner:case(!, 0, control_kernel, 'ISO 7.8.4.4, ISO 9c') :-
   findall(Y, with_output_to(atom(Y), (  goal(X),
                                         call(X),
                                         write('Forwards '))), [_,_]).
runner:case(!, 0, control_kernel, 'ISO 7.8.4.4, ISO 10a') :-
   with_output_to(atom(Y), (  twice(_),
                              \+ \+ !,
                              write('Forwards '))), !,
   Y == 'C Forwards '.
runner:case(!, 0, control_kernel, 'ISO 7.8.4.4, ISO 10b') :-
   findall(Y, with_output_to(atom(Y), (  twice(_),
                                         \+ \+ !,
                                         write('Forwards '))), [_,Y|_]),
   Y == 'Moss Forwards '.
runner:case(!, 0, control_kernel, 'ISO 7.8.4.4, ISO 10c') :-
   findall(Y, with_output_to(atom(Y), (  twice(_),
                                         \+ \+ !,
                                         write('Forwards '))), [_,_]).
runner:case(!, 0, control_kernel, 'ISO 7.8.4.4, ISO 11a') :-
   with_output_to(atom(Y), (  twice(_),
                              once(!),
                              write('Forwards '))), !,
   Y == 'C Forwards '.
runner:case(!, 0, control_kernel, 'ISO 7.8.4.4, ISO 11b') :-
   findall(Y, with_output_to(atom(Y), (  twice(_),
                                         once(!),
                                         write('Forwards '))), [_,Y|_]),
   Y == 'Moss Forwards '.
runner:case(!, 0, control_kernel, 'ISO 7.8.4.4, ISO 11c') :-
   findall(Y, with_output_to(atom(Y), (  twice(_),
                                         once(!),
                                         write('Forwards '))), [_,_]).
runner:case(!, 0, control_kernel, 'ISO 7.8.4.4, ISO 12a') :-
   with_output_to(atom(Y), (  twice(_),
                              call(!),
                              write('Forwards '))), !,
   Y == 'C Forwards '.
runner:case(!, 0, control_kernel, 'ISO 7.8.4.4, ISO 12b') :-
   findall(Y, with_output_to(atom(Y), (  twice(_),
                                         call(!),
                                         write('Forwards '))), [_,Y|_]),
   Y == 'Moss Forwards '.
runner:case(!, 0, control_kernel, 'ISO 7.8.4.4, ISO 12c') :-
   findall(Y, with_output_to(atom(Y), (  twice(_),
                                         call(!),
                                         write('Forwards '))), [_,_]).

/* sys_local_cut */

/* halt(N) */

/* catch(A, E, B) */

runner:ref(catch, 3, control_kernel, 'ISO 7.8.9.4').

runner:case(catch, 3, control_kernel, 'ISO 7.8.9.4, XLOG 1') :-
   catch(current_prolog_flag(sys_mask, on), _, true).
runner:case(catch, 3, control_kernel, 'ISO 7.8.9.4, XLOG 2') :-
   catch(throw(x), _, current_prolog_flag(sys_mask, off)).

:- multifile foo/1.
:- dynamic foo/1.
foo(X) :-
   Y is X*2,
   throw(test(Y)).

runner:case(catch, 3, control_kernel, 'ISO 7.8.9.4, ISO 1') :-
   catch(foo(5), test(Y), true),
   Y == 10.

bar(X) :-
   X = Y,
   throw(Y).

runner:case(catch, 3, control_kernel, 'ISO 7.8.9.4, ISO 2') :-
   catch(bar(3), Z, true),
   Z == 3.
runner:case(catch, 3, control_kernel, 'ISO 7.8.9.4, ISO 3') :-
   catch(true, _, 3).
runner:case(catch, 3, control_kernel, 'ISO 7.8.9.4, ISO 4a') :-
   with_output_to(atom(X), catch(true, _, write(demoen))), !,
   X == ''.
runner:case(catch, 3, control_kernel, 'ISO 7.8.9.4, ISO 4b') :-
   catch((  catch(true, _, write(demoen)),
            throw(bla)), E, true),
   E == bla.

car(X) :-
   X = 1,
   throw(X).

runner:case(catch, 3, control_kernel, 'ISO 7.8.9.4, ISO 5a') :-
   catch(car(_), Y, true),
   Y == 1.
runner:case(catch, 3, control_kernel, 'ISO 7.8.9.4, ISO 5b') :-
   catch(car(X), _, true),
   var(X).
runner:case(catch, 3, control_kernel, 'ISO 7.8.9.4, ISO 6') :-
   \+ catch(number_codes(_, "1a0"), error(syntax_error(_),_), fail).

coo(X) :-
   throw(X).
g :-
   catch(p, _, write(h2)),
   coo(c).
p.
p :-
   throw(b).

runner:case(catch, 3, control_kernel, 'ISO 7.8.9.4, ISO 7a') :-
   with_output_to(atom(X), catch(g, _, write(h1))), !,
   X == h1.
runner:case(catch, 3, control_kernel, 'ISO 7.8.9.4, ISO 7b') :-
   with_output_to(atom(_), catch(g, C, write(h1))), !,
   C == c.
runner:case(catch, 3, control_kernel, 'ISO 7.8.9.4, ISO 8') :-
   catch(coo(_), error(E,_), true),
   E == instantiation_error.

/* throw(E), ISO 7.8.10.4 */

% throw/1 is covered by catch/3, see above.

/* set_prolog_flag(F,V) */

/* current_prolog_flag(F,V) */