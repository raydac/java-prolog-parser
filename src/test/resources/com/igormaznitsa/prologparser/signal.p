/**
 * Prolog code for the control signal theory test cases.
 *
 * Source of test cases are the following standard:
 *   - Draft Proposal for setup_call_cleanup/3, WG17, Ulrich Neumerkel
 *     <a href="http://www.complang.tuwien.ac.at/ulrich/iso-prolog/N215">www.complang.tuwien.ac.at/ulrich/iso-prolog/N215</a>
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
/* Signal Handling                                              */
/****************************************************************/

/* setup_call_cleanup(S, G, C) */

runner:ref(setup_call_cleanup, 3, control_signal, 'WG17 N215').

nondet.
nondet :- fail.

% a), d) interrupts
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, XLOG 1a') :-
   setup_call_cleanup(current_prolog_flag(sys_mask, off), true, true).
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, XLOG 1b') :-
   setup_call_cleanup(true, current_prolog_flag(sys_mask, on), true).
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, XLOG 1c') :-
   setup_call_cleanup(true, nondet, current_prolog_flag(sys_mask, off)), !.
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, XLOG 1d') :-
   setup_call_cleanup(true, true, current_prolog_flag(sys_mask, off)).
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, XLOG 1e') :-
   setup_call_cleanup(true, (  X = 1
                            ;  X = 2,
                               current_prolog_flag(sys_mask, on)), true),
   X = 2.

% a)
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, ISO 1') :-
   \+ setup_call_cleanup(fail, true, true).
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, ISO 2') :-
   catch(setup_call_cleanup(throw(x), true, true), E, true),
   E == x.
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, XLOG 2a') :-
   setup_call_cleanup((  X = 1
                      ;  X = 2), true, true),
   X == 1.
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, XLOG 2b') :-
   \+ (  findall(X, setup_call_cleanup((  X = 1
                                       ;  X = 2), true, true), [_,X|_]),
         X == 2).
% will fail, clean-up pre-validation, not yet implemented
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, ISO 3') :-
   catch(setup_call_cleanup(true, throw(x), _), error(E,_), true),
   E == instantiation_error.

% b)
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, XLOG 3a') :-
   setup_call_cleanup(true, (  X = 1
                            ;  X = 2), true),
   X == 1.
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, XLOG 3b') :-
   findall(X, setup_call_cleanup(true, (  X = 1
                                       ;  X = 2), true), [_,X|_]),
   X == 2.
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, XLOG 4') :-
   \+ setup_call_cleanup(true, fail, true).
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, XLOG 5') :-
   catch(setup_call_cleanup(true, throw(x), true), E, true),
   E == x.
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, XLOG 6') :-
   setup_call_cleanup(true, fail, true); true.
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, XLOG 7') :-
   setup_call_cleanup(X = 1, (  Y = X; true), true),
   Y == 1.
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, XLOG 8') :-
   setup_call_cleanup(X = 1, Y = X, true),
   Y == 1.
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, XLOG 9') :-
   setup_call_cleanup(true, !, true), fail; true.

% c)
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, ISO 13a') :-
   \+ (  setup_call_cleanup(true, nondet, true), fail).
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, ISO 13b') :-
   setup_call_cleanup(true, nondet, true), !.
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, ISO 13c') :-
   catch((  setup_call_cleanup(true, nondet, true),
            throw(x)), E, true),
   E == x.
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, ISO 13d') :-
   catch((  setup_call_cleanup(true, nondet, throw(x)), fail), E, true),
   E == x.
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, ISO 8a') :-
   with_output_to(atom(Y), (  setup_call_cleanup(X = abc, nondet, write(X)), fail; true)), !,
   Y == abc.
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, ISO 8b') :-
   with_output_to(atom(Y), (  setup_call_cleanup(X = abc, nondet, write(X)), !)), !,
   Y == abc.
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, ISO 8c') :-
   with_output_to(atom(Y), catch((  setup_call_cleanup(X = abc, nondet, write(X)),
                                    throw(x)), _, true)), !,
   Y == abc.
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, ISO 13e') :-
   with_output_to(atom(Y), setup_call_cleanup(true, nondet, write(abc))), !,
   Y == ''.
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, ISO 10') :-
   with_output_to(atom(Y), setup_call_cleanup(true, true, write(abc))), !,
   Y == abc.

% d)
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, ISO 4') :-
   \+ (  setup_call_cleanup(true, nondet, (  true
                                          ;  throw(x))), fail).
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, XLOG 10') :-
   setup_call_cleanup(true, nondet, X = 1), !,
   X == 1.
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, XLOG 11') :-
   setup_call_cleanup(true, nondet, X = 1),
   var(X).
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, XLOG 12') :-
   setup_call_cleanup(true, true, X = 1),
   X == 1.

runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, ISO 15') :-
   findall(X, with_output_to(atom(X), catch(setup_call_cleanup(S = 1, (  G = 2
                                                                      ;  G = 3,
                                                                         throw(x)),
                                               write(S+G)), _, true)), [_,X|_]),
   X \== '1 + 3'.
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, ISO 21') :-
   with_output_to(atom(X), catch((  setup_call_cleanup(S = 1, (  G = 2
                                                              ;  G = 3),
                                       write(S+G+B)),
                                    B = 4,
                                    throw(x)), _, true)), !,
   X \== '1 + 2 + 4'.
% will fail, clean-up exception accumulation, not part of ISO proposal
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, ISO 25') :-
   catch(setup_call_cleanup(true, throw(x), throw(y)), E, true),
   E == x.
% will fail, clean-up exception accumulation, not part of ISO proposal
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, ISO 26') :-
   catch((  setup_call_cleanup(true, nondet, throw(x)),
            throw(y)), E, true),
   E == y.
% will fail, clean-up exception accumulation, not part of ISO proposal
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, ISO 27') :-
   catch(setup_call_cleanup(true, throw(x), setup_call_cleanup(true, fail, throw(y))), E, true),
   E == x.
% will fail, clean-up determinism, not part of ISO proposal
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, ISO 9') :-
   setup_call_cleanup(true, true, fail).
% will fail, clean-up determinism, not part of ISO proposal
runner:case(setup_call_cleanup, 3, control_signal, 'WG17 N215, XLOG 13') :-
   setup_call_cleanup(true, nondet, fail), !.

/* call_cleanup(G, C) */

runner:ref(call_cleanup, 2, control_signal, 'WG17 N215b').

% a), d) interrupts
runner:case(call_cleanup, 2, control_signal, 'WG17 N215b, XLOG 1b') :-
   call_cleanup(current_prolog_flag(sys_mask, on), true).
runner:case(call_cleanup, 2, control_signal, 'WG17 N215b, XLOG 1c') :-
   call_cleanup(nondet, current_prolog_flag(sys_mask, off)), !.
runner:case(call_cleanup, 2, control_signal, 'WG17 N215b, XLOG 1d') :-
   call_cleanup(true, current_prolog_flag(sys_mask, off)).
runner:case(call_cleanup, 2, control_signal, 'WG17 N215b, XLOG 1e') :-
   call_cleanup((  X = 1
                ;  X = 2,
                   current_prolog_flag(sys_mask, on)), true),
   X = 2.

% a)
% will fail, clean-up pre-validation, not yet implemented
runner:case(call_cleanup, 2, control_signal, 'WG17 N215b, ISO 3') :-
   catch(call_cleanup(throw(x), _), error(E,_), true),
   E == instantiation_error.

% b)
runner:case(call_cleanup, 2, control_signal, 'WG17 N215b, XLOG 3a') :-
   call_cleanup((  X = 1
                ;  X = 2), true),
   X == 1.
runner:case(call_cleanup, 2, control_signal, 'WG17 N215b, XLOG 3b') :-
   findall(X, call_cleanup((  X = 1
                           ;  X = 2), true), [_,X|_]),
   X == 2.
runner:case(call_cleanup, 2, control_signal, 'WG17 N215b, XLOG 4') :-
   \+ call_cleanup(fail, true).
runner:case(call_cleanup, 2, control_signal, 'WG17 N215b, XLOG 5') :-
   catch(call_cleanup(throw(x), true), E, true),
   E == x.
runner:case(call_cleanup, 2, control_signal, 'WG17 N215b, XLOG 6') :-
   call_cleanup(fail, true); true.
runner:case(call_cleanup, 2, control_signal, 'WG17 N215b, XLOG 9') :-
   call_cleanup(!, true), fail; true.

% c)
runner:case(call_cleanup, 2, control_signal, 'WG17 N215b, ISO 13a') :-
   \+ (  call_cleanup(nondet, true), fail).
runner:case(call_cleanup, 2, control_signal, 'WG17 N215b, ISO 13b') :-
   call_cleanup(nondet, true), !.
runner:case(call_cleanup, 2, control_signal, 'WG17 N215b, ISO 13c') :-
   catch((  call_cleanup(nondet, true),
            throw(x)), E, true),
   E == x.
runner:case(call_cleanup, 2, control_signal, 'WG17 N215b, ISO 13d') :-
   catch((  call_cleanup(nondet, throw(x)), fail), E, true),
   E == x.
runner:case(call_cleanup, 2, control_signal, 'WG17 N215b, ISO 13e') :-
   with_output_to(atom(Y), call_cleanup(nondet, write(abc))), !,
   Y == ''.
runner:case(call_cleanup, 2, control_signal, 'WG17 N215b, ISO 10') :-
   with_output_to(atom(Y), call_cleanup(true, write(abc))), !,
   Y == abc.

% d)
runner:case(call_cleanup, 2, control_signal, 'WG17 N215b, ISO 4') :-
   \+ (  call_cleanup(nondet, (  true
                              ;  throw(x))), fail).
runner:case(call_cleanup, 2, control_signal, 'WG17 N215b, XLOG 10') :-
   call_cleanup(nondet, X = 1), !,
   X == 1.
runner:case(call_cleanup, 2, control_signal, 'WG17 N215b, XLOG 11') :-
   call_cleanup(nondet, X = 1),
   var(X).
runner:case(call_cleanup, 2, control_signal, 'WG17 N215b, XLOG 12') :-
   call_cleanup(true, X = 1),
   X == 1.

% will fail, clean-up exception accumulation, not part of ISO proposal
runner:case(call_cleanup, 2, control_signal, 'WG17 N215b, ISO 25') :-
   catch(call_cleanup(throw(x), throw(y)), E, true),
   E == x.
% will fail, clean-up exception accumulation, not part of ISO proposal
runner:case(call_cleanup, 2, control_signal, 'WG17 N215b, ISO 26') :-
   catch((  call_cleanup(nondet, throw(x)),
            throw(y)), E, true),
   E == y.
% will fail, clean-up exception accumulation, not part of ISO proposal
runner:case(call_cleanup, 2, control_signal, 'WG17 N215b, ISO 27') :-
   catch(call_cleanup(throw(x), setup_call_cleanup(true, fail, throw(y))), E, true),
   E == x.
% will fail, clean-up determinism, not part of ISO proposal
runner:case(call_cleanup, 2, control_signal, 'WG17 N215b, ISO 9') :-
   call_cleanup(true, fail).
% will fail, clean-up determinism, not part of ISO proposal
runner:case(call_cleanup, 2, control_signal, 'WG17 N215b, XLOG 13') :-
   call_cleanup(nondet, fail), !.
