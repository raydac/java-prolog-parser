%to run : schedule(number_of_weeks, Schedule)
%for example : schedule(2,Schedule) means genarate a schedule for 2 weeks

% +++++++****+++++++ PLEASE TAKE CARE THAT IT MAY TAKE TIME TO GENERATE VERY COMPLICATED SCHEDULES ++++++++****+++++++
% +++++++****+++++++                **            SO PLEASE WAIT :)          **                    ++++++++****+++++++


% **************schedule structure illustration ***************
%assuming schedule is in the form [[[[[list of events in Sat]],[list of slots in Sun],[Mon],[Tue],[Wed],[Thu]],[list of days in week 2],[week 3]...]
% +++++++****+++++++ PLEASE TAKE CARE THAT IT MAY TAKE TIME TO GENERATE VERY COMPLICATED SCHEDULES ++++++++****+++++++
% +++++++****+++++++                **            SO PLEASE WAIT :)          **                    ++++++++****+++++++
% For Example :
%  [[[[[]], [[]], [[]], [[]], [[]]], [[[]], [[]], [[]], [[]], [[]]], [[[]], [[]], [[slotNum(3), event(group6MET, csen604, project1, evaluation)]], [[]], [[]]], [[[]], [[]], [[]], [[slotNum(4), event(group4MET, csen402, quiz1, quiz)]], [[]]], [[[]], [[]], [[]], [[]], [[slotNum(5), event(group4MET, csen403, project1, evaluation)]]], [[[]], [[]], [[]], [[]], [[]]]], [[[[]], [[]], [[]], [[]], [[]]], [[[]], [[]], [[]], [[]], [[]]], [[[]], [[]], [[slotNum(3), event(group6MET, csen603, quiz1, quiz)]], [[]], [[]]], [[[]], [[]], [[]], [[slotNum(4), event(group4MET, csen401, milestone1, evaluation)]], [[]]], [[[]], [[]], [[]], [[]], [[]]], [[[]], [[]], [[]], [[]], [[]]]]]
% means the Following:
/*
====================================================
=======================week 1=======================
====================================================
_______________________________saturday_______________________________
__[[[]], [[]], [[]], [[]], [[]]]__
_______________________________sunday_______________________________
__[[[]], [[]], [[]], [[]], [[]]]__
_______________________________monday_______________________________
__[[[]], [[]], [[slotNum(3), event(group6MET, csen604, project1, evaluation)]], [[]], [[]]]__
_______________________________tuesday_______________________________
__[[[]], [[]], [[]], [[slotNum(4), event(group4MET, csen402, quiz1, quiz)]], [[]]]__
_______________________________wednesday_______________________________
__[[[]], [[]], [[]], [[]], [[slotNum(5), event(group4MET, csen403, project1, evaluation)]]]__
_______________________________thursday_______________________________
__[[[]], [[]], [[]], [[]], [[]]]__
====================================================
=======================week 2=======================
====================================================
_______________________________saturday_______________________________
__[[[]], [[]], [[]], [[]], [[]]]__
_______________________________sunday_______________________________
__[[[]], [[]], [[]], [[]], [[]]]__
_______________________________monday_______________________________
__[[[]], [[]], [[slotNum(3), event(group6MET, csen603, quiz1, quiz)]], [[]], [[]]]__
_______________________________tuesday_______________________________
__[[[]], [[]], [[]], [[slotNum(4), event(group4MET, csen401, milestone1, evaluation)]], [[]]]__
_______________________________wednesday_______________________________
__[[[]], [[]], [[]], [[]], [[]]]__
_______________________________thursday_______________________________
__[[[]], [[]], [[]], [[]], [[]]]__
*/



event_in_course(csen403, labquiz1, assignment).
event_in_course(csen403, labquiz2, assignment).
event_in_course(csen403, project1, evaluation).
event_in_course(csen403, project2, evaluation).
event_in_course(csen403, quiz1, quiz).
event_in_course(csen403, quiz2, quiz).
event_in_course(csen403, quiz3, quiz).

event_in_course(csen401, quiz1, quiz).
event_in_course(csen401, quiz2, quiz).
event_in_course(csen401, quiz3, quiz).
event_in_course(csen401, milestone1, evaluation).
event_in_course(csen401, milestone2, evaluation).
event_in_course(csen401, milestone3, evaluation).

event_in_course(csen402, quiz1, quiz).
event_in_course(csen402, quiz2, quiz).
event_in_course(csen402, quiz3, quiz).

event_in_course(math401, quiz1, quiz).
event_in_course(math401, quiz2, quiz).
event_in_course(math401, quiz3, quiz).

event_in_course(elct401, quiz1, quiz).
event_in_course(elct401, quiz2, quiz).
event_in_course(elct401, quiz3, quiz).
event_in_course(elct401, assignment1, assignment).
event_in_course(elct401, assignment2, assignment).


event_in_course(csen601, quiz1, quiz).
event_in_course(csen601, quiz2, quiz).
event_in_course(csen601, quiz3, quiz).
event_in_course(csen601, project, evaluation).
event_in_course(csen603, quiz1, quiz).
event_in_course(csen603, quiz2, quiz).
event_in_course(csen603, quiz3, quiz).

event_in_course(csen602, quiz1, quiz).
event_in_course(csen602, quiz2, quiz).
event_in_course(csen602, quiz3, quiz).

event_in_course(csen604, quiz1, quiz).
event_in_course(csen604, quiz2, quiz).
event_in_course(csen604, quiz3, quiz).
event_in_course(csen604, project1, evaluation).
event_in_course(csen604, project2, evaluation).

holiday(1,saturday).
holiday(3,monday).
holiday(5,tuesday).
holiday(10,sunday).

studying(csen403, group4MET).
studying(csen401, group4MET).
studying(csen402, group4MET).

studying(csen601, group6MET).
studying(csen602, group6MET).
studying(csen603, group6MET).
studying(csen604, group6MET).

studying(elct401,group4EMS).
studying(math401,group4EMS).

should_precede(math401,quiz1,quiz2).
should_precede(math401,quiz2,quiz3).

should_precede(elct401,assignment1,assignment2).
should_precede(elct401,quiz1,quiz2).
should_precede(elct401,quiz2,quiz3).


should_precede(csen401,milestone1,milestone2).
should_precede(csen401,milestone2,milestone3).
should_precede(csen401,quiz1,quiz2).
should_precede(csen401,quiz2,quiz3).

should_precede(csen402,quiz1,quiz2).
should_precede(csen402,quiz2,quiz3).

should_precede(csen403,labquiz1,labquiz2).
should_precede(csen403,project1,project2).
should_precede(csen403,quiz1,quiz2).
should_precede(csen403,quiz2,quiz3).


should_precede(csen601,quiz1,quiz2).
should_precede(csen601,quiz2,quiz3).

should_precede(csen602,quiz1,quiz2).
should_precede(csen602,quiz2,quiz3).

should_precede(csen603,quiz1,quiz2).
should_precede(csen603,quiz2,quiz3).

should_precede(csen604,project1,project2).
should_precede(csen604,quiz1,quiz2).
should_precede(csen604,quiz2,quiz3).

%quizslot(group4EMS, sunday,5).
quizslot(group4MET, saturday, 1).
quizslot(group4MET, tuesday, 4).
%quizslot(group4MET, wednesday, 5).
%quizslot(group6MET, sunday, 5).
quizslot(group6MET, monday, 3).
%quizslot(group6MET, thursday, 3).

%======================================================================
%======================================================================
%************************* Start of the codes *************************
%======================================================================
%======================================================================


/*The predicate precede(G,Schedule) should succeed only
if all of the should_precede facts of the courses
studied by the group G are satisfied.*/

precede(G,Schedule):-
   flatten(Schedule,S),
   collect(G,S,E),  %now i have a list E that contains all group G events in the schedule
    setof(C,studying(C,G),Cs),
    !,precedehlp(G,E,Cs).

%precedehlp(G,E,Cs) succeed only if all of the should_precede facts of courses Cs studied by group G are satisfied
precedehlp(_,_,[]).
precedehlp(G,E,[C|Cs]):-
    collectCR(G,C,E,CourseE), %now i have a list CourseE that contains all course C events in the schedule
    %precedehlp2(CourseE),
    findall(event(G,C,N,Type),(event_in_course(C,N,Type),(should_precede(C,N,_);should_precede(C,_,N))),CO1),
    findall(event(G,C,N,Type),event_in_course(C,N,Type),CO2), %this step is done to handle events that have no precedence restrictions
    append(CO1,CO2,CO),
    remove_dups(CO,CorrectOrde),
    intersection(CorrectOrde,CourseE,CorrectOrder),
    %Now I have the correct order of the events and a comparison will be made with the order of the events in the schedule
    get_allEvents_ofCertainType_from_a_list(CourseE,QuizesInSchedule,quiz),
    get_allEvents_ofCertainType_from_a_list(CorrectOrder,QuizesCorrectOrder,quiz),
    !,QuizesInSchedule==QuizesCorrectOrder,

    get_allEvents_ofCertainType_from_a_list(CourseE,AssignmentsInSchedule,assignment),
    get_allEvents_ofCertainType_from_a_list(CorrectOrder,AssignmentsCorrectOrder,assignment),
    !,AssignmentsInSchedule==AssignmentsCorrectOrder,

    get_allEvents_ofCertainType_from_a_list(CourseE,EvaluationsInSchedule,evaluation),
    get_allEvents_ofCertainType_from_a_list(CorrectOrder,EvaluationsCorrectOrder,evaluation),
    !,EvaluationsInSchedule==EvaluationsCorrectOrder,
    precedehlp(G,E,Cs).


%get_allEvents_ofCertainType_from_a_list(L1,L2,T)... outputs L2 which contains all the elements in L1 of type T
get_allEvents_ofCertainType_from_a_list([],[],_).
get_allEvents_ofCertainType_from_a_list([event(G,C,N,Type)|T],[event(G,C,N,Type)|T2],Type):-
    get_allEvents_ofCertainType_from_a_list(T,T2,Type).

get_allEvents_ofCertainType_from_a_list([_|T],T2,Type):-
    get_allEvents_ofCertainType_from_a_list(T,T2,Type).


%collectCR(G,C,Events,CourseEvents) .. CourseEvents list contains all events for course C studied by group G form group events list Events
collectCR(_,_,[],[]).
collectCR(G,C,[event(G,C,N,Type)|T],[event(G,C,N,Type)|T2]):-
        collectCR(G,C,T,T2).
collectCR(G,C,[event(_,X,_,_)|T],T2):-
    X\=C,collectCR(G,C,T,T2).

%collect(G,S,Events).. Events list contains all events of group G in schedule S
collect(_,[],[]).
collect(G,[slotNum(_),event(G,C,N,Type)|T],[event(G,C,N,Type)|T2]):-
        collect(G,T,T2).
collect(G,[slotNum(_),event(X,_,_,_)|T],T2):-
    X\=G,collect(G,T,T2).


/*valid_slot_schedule(G,Slot) is true only if the group G does
not have more than one event in Slot.*/

valid_slots_schedule(_,[]).
valid_slots_schedule(G,[W|T]):-
    !,valid_slots_schedulehlpW(G,W),!,
    valid_slots_schedule(G,T).

valid_slots_schedulehlpW(_,[]).		%enters the days of each week
valid_slots_schedulehlpW(G,[D|T]):-
     !,valid_slots_schedulehlpD(G,D),!,
    valid_slots_schedulehlpW(G,T).

valid_slots_schedulehlpD(_,[]).
valid_slots_schedulehlpD(G,[S|T]):-    %enters the slots of each day
    flatten(S,Sflatten),
    countOccuranceOfaGroup(Sflatten,G,N),
    !,N=<1, %if number of events of group G in slot S is more than 1 then the predicate should fail
    valid_slots_schedulehlpD(G,T).

%gets the number of events for group G in a slot
countOccuranceOfaGroup([],_,0).
countOccuranceOfaGroup([_,event(G,_,_,_)|T],G,Y):- countOccuranceOfaGroup(T,G,Z), Y is 1+Z.
countOccuranceOfaGroup([_,event(G1,_,_,_)|T],G,Z):- G1\=G,countOccuranceOfaGroup(T,G,Z).


/*available_timings(G,L) should only be true if L is
the list of timings in which the group G could have
an event scheduled.*/
available_timings(G,L):-
    bagof(timing(Day,SlotNum),quizslot(G,Day,SlotNum),L).


/*group_events(G,Events) is true if Events is the list of events that
should be scheduled for the group G.*/
group_events(G,Events):-
    setof(event(G,Course,Name,Type),(studying(Course,G),event_in_course(Course,Name,Type)),Events).

/*no_consec_quizzes(G,Schedule) should succeed only if the group G does not have two quizzes for the
same course in two consecitive weeks*/
no_consec_quizzes(_,[]).
no_consec_quizzes(G,[W1,W2|T]):-
    flatten(W1,W1flatten),
    flatten(W2,W2flatten),
    append(W1flatten,W2flatten,MergedWeeks),
    findall(C,studying(C,G),Courses),%Courses studied by group G
    make_sure_this_week_contains_AtMost_1_quiz_for_this_course(G,MergedWeeks,Courses),
    no_consec_quizzes(G,[W2|T]).


no_consec_quizzes(_,[W|[]]):-
    findall(C,studying(C,G),Courses),
    make_sure_this_week_contains_AtMost_1_quiz_for_this_course(G,W,Courses).

make_sure_this_week_contains_AtMost_1_quiz_for_this_course(_,_,[]).
make_sure_this_week_contains_AtMost_1_quiz_for_this_course(G,W,[C|Cs]):-
	countOccuranceOfquiz(W,G,C,N),
	!,N < 2,make_sure_this_week_contains_AtMost_1_quiz_for_this_course(G,W,Cs).

%gets the number of quizes for a course in group G in a list
countOccuranceOfquiz([],_,_,0).
countOccuranceOfquiz([event(G,C,_,quiz)|T],G,C,Y):- countOccuranceOfquiz(T,G,C,Z), Y is 1+Z.
countOccuranceOfquiz([_|T],G,C,Z):- countOccuranceOfquiz(T,G,C,Z).


/*
no_same_day_quiz(G,Schedule) is only true if group G does not have two quizzes scheduled on the same
day in Schedule. */
no_same_day_quiz(_,[]).
no_same_day_quiz(G,[W|T]):- % access each week in the schedule
    !,no_same_day_quizWKhlp(G,W),!,
    no_same_day_quiz(G,T).

%no_same_day_quizhlp(G,Day) checks in Day list if at most 1 quiz is in a day for a each course.
no_same_day_quizWKhlp(_,[]).
no_same_day_quizWKhlp(G,[D|T]):-  %gets the days of the week
    flatten(D,D1),
    getQuizesNumberInAday(D1,G,N),
    !,N=<1,
    no_same_day_quizWKhlp(G,T).

%getQuizesNumberInAday(Events,G,N).. N is the number of quizzes in events of the day (Events) for group G
getQuizesNumberInAday([],_,0).
getQuizesNumberInAday([_,event(G,_,_,quiz)|T],G,Y):- getQuizesNumberInAday(T,G,Z), Y is 1+Z.
getQuizesNumberInAday([_,event(G1,_,_,X)|T],G,Z):- (G1\=X;X\=quiz),getQuizesNumberInAday(T,G,Z).

/*
no_same_day_assignment(G,Schedule) is only true if group G does not have two assignments scheduled on the same
day in Schedule. */
no_same_day_assignment(_,[]).
no_same_day_assignment(G,[W|T]):- % access each week in the schedule
    !,no_same_day_assignmentWKhlp(G,W),!,
    no_same_day_assignment(G,T).

%no_same_day_assignmenthlp(G,Day) checks in Day list if at most 1 assignment is in a day for a each course.
no_same_day_assignmentWKhlp(_,[]).
no_same_day_assignmentWKhlp(G,[D|T]):-  %gets the days of the week
    flatten(D,D1),
    getAssignmentsNumberInAday(D1,G,N),
    !,N=<1,
    no_same_day_assignmentWKhlp(G,T).

%getAssignmentsNumberInAday(Events,G,N).. N is the number of Assignments in events of the day (Events) for group G
getAssignmentsNumberInAday([],_,0).
getAssignmentsNumberInAday([_,event(G,_,_,assignment)|T],G,Y):- getAssignmentsNumberInAday(T,G,Z), Y is 1+Z.
getAssignmentsNumberInAday([_,event(G1,_,_,X)|T],G,Z):- (G1\=X;X\=assignment),getAssignmentsNumberInAday(T,G,Z).






%no_holidays(G,Schedule) should succeed only if Schedule has no events scheduled in any of the available holidays.
no_holidays(_,[]).
no_holidays(G,Schedule):-
    no_holidayshlp(G,Schedule,1),!.

no_holidayshlp(_,[],_).
no_holidayshlp(G,[W|T],C):- %if this week has holidays then enter the days of these week
 % C is the week counter
    holiday(C,_),
    !,no_holidayshlpDayhlp(G,W,C,1),
    C1 is C+1,
    no_holidayshlp(G,T,C1).

no_holidayshlp(G,[_|T],C):- %lw el week dah mafish fih holiday hay5osh 3al week el ba3do
    C1 is C+1,
    no_holidayshlp(G,T,C1).

%this predicate is made so that if one of the 2 predicates (no_holidayshlpDayhlp)
%of no_holidayshlpDay is true it continues
%if not it fails
no_holidayshlpDayhlp(G,W,C,1):-
    no_holidayshlpDay(G,W,C,1).


no_holidayshlpDay(_,[],_,_).
no_holidayshlpDay(G,[D|T],C,DayCounter):-
    holiday(C,DAY),
    day_to_number(DAY,DayCounter),
    flatten(D,DFlatten),
    !,DFlatten == [], %ensures that the day number DayCounter has no events
    D1 is DayCounter+1,
    no_holidayshlpDay(G,T,C,D1).

no_holidayshlpDay(G,[_|T],C,DayCounter):-
    holiday(C,DAY),
    day_to_number(DAY,N),
    !,N\=DayCounter,
    D1 is DayCounter+1,
    no_holidayshlpDay(G,T,C,D1).

%assuming saturday is 1 and thursday is 6
day_to_number(saturday,1).
day_to_number(sunday,2).
day_to_number(monday,3).
day_to_number(tuesday,4).
day_to_number(wednesday,5).
day_to_number(thursday,6).




%get_all_groups_in_aList(L) .. L is a list for all groups that should be inserted in the schedule
get_all_groups_in_aList(L):-
    findall(G,quizslot(G,_,_),L1),
    remove_dups(L1,L),!.

%remove_dups(L1,L2) .. L2 is the same as L1 but without duplicates
remove_dups([],[]).
remove_dups([First | Rest], NewRest) :-
	member(First, Rest),
	remove_dups(Rest, NewRest).
remove_dups([First | Rest], [First | NewRest]) :-
	not(member(First, Rest)),
	remove_dups(Rest, NewRest).

%========================================================================================
%==================================== MAIN PREDICATE ====================================
%========================================================================================

/*
 schedule(Week_Number,Schedule) should success if Schedule is the schedule of all of the available events satisfying all the constraints.
Week_Number is the number of available weeks for the semester.
*/
schedule(Week_Number, Schedule):-
    scheduleform(S,Week_Number),!,
    get_all_groups_in_aList(Gs),!,
    schedulehlp_loop_over_groups(S,Gs,Schedule).

%schedulehlp_loop_over_groups(ScheduleForm,Groups,FinalSchedule)
%puts events of each group in Groups list in scheduleForm and outputs it in the FinalSchedule
schedulehlp_loop_over_groups(S,[],S):-
    make_free_slots_emptyLists(S),
    !,printgeneratedScheduleForTesting(S,1),!.

schedulehlp_loop_over_groups(Schedule,[G|Gs],FinalSchedule):-
    group_events(G,E),!,
    random_permutation(E,RandomPermutedEvents),!,
    get_time(T1),
    permute_and_reverse(RandomPermutedEvents,PermutedEventsInPut),%permutes the events
    randomly_generating_schedule(T1,PermutedEventsInPut,PermutedEventsOutPut),
    check(Schedule,G,PermutedEventsOutPut), % insert permuted events in the schedule and then it checks the constraints (no_same_day_quiz ,....) if the constraints fail it tries another events' permutation
    make_free_slots_uninitiatedElements(Schedule,PSchedule), %it changes empty slots [] to unistantiated elements _G756 ...
    schedulehlp_loop_over_groups(PSchedule,Gs,FinalSchedule).


randomly_generating_schedule(T1,PermutedEventsInPut,PermutedEventsOutPut):-
    get_time(T2),
    Diff is T2 - T1,
    Diff >= 0.000001,
    random_permutation(PermutedEventsInPut,PermutedEventsOutPut).

randomly_generating_schedule(_,PermutedEventsInPut,PermutedEventsOutPut):-
    PermutedEventsInPut=PermutedEventsOutPut.

% insert permuted events in the schedule and then it checks the constraints (no_same_day_quiz ,....) if the constraints
check(Schedule,G,PermutedEventsOutPut):-
    schedulehlp_loop_over_groupEVENTS(G,PermutedEventsOutPut,Schedule,1),!,
    make_free_slots_emptyLists(Schedule),!,%it changes unistantiated elements (_G756) to empty slots []...
    checkConstraintsForEachGroup(Schedule,G).


permute_and_reverse(Org,PR):-
    permute(Org,P),
    rev(P,PR).

permute([], []).
permute([X|Rest], L) :-
    permute(Rest, L1),
    select(X, L, L1).

rev(L,R):-  accRev(L,[],R).
accRev([H|T],A,R):-  accRev(T,[H|A],R).
   accRev([],A,A).

checkConstraintsForEachGroup(Schedule,G):-
   	no_holidays(G,Schedule),!,
    precede(G,Schedule),!,
    valid_slots_schedule(G,Schedule),!,
    no_consec_quizzes(G,Schedule),!,
    no_same_day_quiz(G,Schedule),!,
    no_same_day_assignment(G,Schedule),!.

% make a new list List that contains X elements .. it's length is N
inserthlp(X, N, List)  :-
    length(List, N),
    maplist(=(X), List).

%it changes unistantiated elements (_G756) to empty slots []
make_free_slots_emptyLists([]).
make_free_slots_emptyLists([W|T]):- %loop over the weeks of the schedule
    make_free_slots_emptyListsInEachWeek(W), %access each week
    make_free_slots_emptyLists(T).

make_free_slots_emptyListsInEachWeek([]).
make_free_slots_emptyListsInEachWeek([D|Ds]):- %loops over the days of a week
    make_free_slots_emptyListsInEachDay(D,1), %access each day
	make_free_slots_emptyListsInEachWeek(Ds).

make_free_slots_emptyListsInEachDay([],_).
make_free_slots_emptyListsInEachDay([S|Ss],SC):- %loop over the slots of a day
    make_free_slots_emptyListsInEachSlothlp(S,SC), %access each slot
    SC1 is SC+1, % SC is needed to get the number of the slot and put it in slotNUM(...)
    make_free_slots_emptyListsInEachDay(Ss,SC1).

make_free_slots_emptyListsInEachSlothlp([],_).
make_free_slots_emptyListsInEachSlothlp([E|Es],SC):- %loop over events in a slot
    make_free_slots_emptyListsInEachSlot(E,SC),
    make_free_slots_emptyListsInEachSlothlp(Es,SC).

make_free_slots_emptyListsInEachSlot([],_).
make_free_slots_emptyListsInEachSlot([[],[]],_).
make_free_slots_emptyListsInEachSlot([slotNum(SC),event(_,_,_,_)],SC) .



%it changes empty slots [] to unistantiated elements _G756 ...
make_free_slots_uninitiatedElements([],_).
make_free_slots_uninitiatedElements([W|T],[W1|T1]):-%loop over the weeks of the schedule
    make_free_slots_uninitiatedElementsInEachWeek(W,W1),%access each week
    make_free_slots_uninitiatedElements(T,T1).

make_free_slots_uninitiatedElementsInEachWeek([],_).
make_free_slots_uninitiatedElementsInEachWeek([D|Ds],[D1|Ds1]):-%loops over the days of a week
    make_free_slots_uninitiatedElementsInEachDay(D,D1),%access each day
	make_free_slots_uninitiatedElementsInEachWeek(Ds,Ds1).

make_free_slots_uninitiatedElementsInEachDay([],_).
make_free_slots_uninitiatedElementsInEachDay([S|Ss],[S1|Ss1]):-%loop over the slots of a day
    make_free_slots_uninitiatedElementsInEachSlothlp(S,S1),%access each slot
    make_free_slots_uninitiatedElementsInEachDay(Ss,Ss1).

make_free_slots_uninitiatedElementsInEachSlothlp([],_).
make_free_slots_uninitiatedElementsInEachSlothlp([E|Es],[E1|Es1]):-%loop over events in a slot
	make_free_slots_uninitiatedElementsInEachSlot(E,E1),
    make_free_slots_uninitiatedElementsInEachSlothlp(Es,Es1).

make_free_slots_uninitiatedElementsInEachSlot([],_).
make_free_slots_uninitiatedElementsInEachSlot([[],[]],_).
make_free_slots_uninitiatedElementsInEachSlot([slotNum(N),event(X,Y,Z,W)],[slotNum(N),event(X,Y,Z,W)]) .


% printgeneratedScheduleForTesting(Schedule,WeekCounter) .. prints the schedule on the console to have a friendly user interface
printgeneratedScheduleForTesting([],_).
printgeneratedScheduleForTesting([W|T],WC):-
    write("==================================================== \n"),
    write("======================="),
    write("week "),
    write(WC),
    write("======================="),
   	write("\n"),
    write("==================================================== \n"),
    printing_days(W,1),
    write("\n"),
    WC1 is WC+1,printgeneratedScheduleForTesting(T,WC1) .

printing_days([],_).
printing_days([D|Ds],DC):-
	day_to_number(DayName,DC),
    write("_______________________________"),
    write(DayName),
    write("_______________________________"),
    write("\n"),
    write("\n __"),
    write(D),
    write("__ \n"),
    DC1 is DC+1,
    printing_days(Ds,DC1).

%============================================================================================================
%=============================== INSERTING THE EVENTS IN THE SCHEDULE =======================================
%============================================================================================================

schedulehlp_loop_over_groupEVENTS(_,[],_,_).
schedulehlp_loop_over_groupEVENTS(_,_,[],_).
schedulehlp_loop_over_groupEVENTS(G,Events,[W|T],WeekCounter):-
    available_timings(G,AvailableTimes),%list of available timings for group G
    findall(timing(D,_),holiday(WeekCounter,D),Holidays), %gets a list of holidays
    subtract(AvailableTimes,Holidays,AvailableTimesConsideringHolidays), %remove the holidays from the available timings
    put_events_in_available_timings(Events,AvailableTimesConsideringHolidays,W),%puts events for group G in the schedule
    length(AvailableTimesConsideringHolidays,NumOfEventsUsedThisWeek),  % gets number of events used in this week
    get_rest_of_events_to_be_scheduled_in_the_nextWEEK(Events,NumOfEventsUsedThisWeek,RestOfEvents), % gets a list of remaining events
    WeekCounter1 is WeekCounter+1,
    schedulehlp_loop_over_groupEVENTS(G,RestOfEvents,T,WeekCounter1).


% gets a list of remaining events
get_rest_of_events_to_be_scheduled_in_the_nextWEEK(RestOfEvents,0,RestOfEvents).
get_rest_of_events_to_be_scheduled_in_the_nextWEEK([],_,[]).
get_rest_of_events_to_be_scheduled_in_the_nextWEEK([_|Es],NumOfEventsUsedThisWeek,RestOfEvents):-
    N1 is NumOfEventsUsedThisWeek-1,
    get_rest_of_events_to_be_scheduled_in_the_nextWEEK(Es,N1,RestOfEvents).


%loops over events and available timings and put each event in a different available timne
put_events_in_available_timings(_,[],_).
put_events_in_available_timings([],_,_).
put_events_in_available_timings([E|Es],[AT|ATs],W):-
    get_available_slot_in_aDay(AT,W,E),
    put_events_in_available_timings(Es,ATs,W).


get_available_slot_in_aDay(timing(DayName,SlotNUM),Week,Event):-
    loop_over_days_to_get_the_wanted_day(DayName,Week,DayFromSchedule,1),
    loop_over_slots_to_get_the_wanted_slot(DayFromSchedule,SlotNUM,1,Event).

loop_over_slots_to_get_the_wanted_slot([],_,_,_).
%if this is the right slot enter its events and put the new event
loop_over_slots_to_get_the_wanted_slot([S|_],SlotNUM,SlotCounter,Event):-
    SlotNUM == SlotCounter,
    loop_over_eventsInASlotAndPutTheNewEvent(S,SlotNUM,Event).


loop_over_slots_to_get_the_wanted_slot([_|Ss],SlotNUM,SlotCounter,Event):-
    SlotNUM\=SlotCounter,
    SC is SlotCounter +1,
    loop_over_slots_to_get_the_wanted_slot(Ss,SlotNUM,SC,Event).

%if the cell doesn't contain event of another group then insert the new Event
loop_over_eventsInASlotAndPutTheNewEvent([[_,E]|_],_,Event):-
    %E\=event(_,_,_,_),
    append([],Event,E).
%if there is already an event then check the next cell
loop_over_eventsInASlotAndPutTheNewEvent([[_,event(_,_,_,_)]|Es],SlotNUM,Event):-
    loop_over_eventsInASlotAndPutTheNewEvent(Es,SlotNUM,Event).

%%s




loop_over_days_to_get_the_wanted_day(_,[],_,_).
loop_over_days_to_get_the_wanted_day(DayName,[_|Ds],DayFromSchedule,DayCounter):-
    day_to_number(DayName,NumOfDay),
    NumOfDay\=DayCounter,
    DC is DayCounter+1,
    loop_over_days_to_get_the_wanted_day(DayName,Ds,DayFromSchedule,DC).

loop_over_days_to_get_the_wanted_day(DayName,[D|_],D,DayCounter):-
    day_to_number(DayName,NumOfDay),
    NumOfDay==DayCounter.


%forms the schedule shape
scheduleform([],0).
scheduleform([W|T],C):-
    weekform(W,1),
    C1 is C-1,
    scheduleform(T,C1).

weekform([],7).
weekform([D|T],C):-
    dayform(D,1),
    C1 is C+1,
    weekform(T,C1).

dayform([],6).
dayform([S|T],C):-
    S = [_|_], %will be later [slotNUM(..),event(...),......]
    C1 is C+1,
    dayform(T,C1).