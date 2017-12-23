%% swipl -l check-higher-order.prolog

:- discontiguous reachable_fact/1.
(:- 'style_check'('-'('singleton'))).

object(a).
object(b).
object(c).

reachable_fact(location(a)).
reachable_fact(location(b)).
reachable_fact(connected(a,b)).

%%%% actions

reachable_op(drive(A,B)):-
    reachable_fact(location(A)),
    reachable_fact(location(B)),
    reachable_fact(connected(A,B)).

%% free variable in head
%% --- causes C to remain a variable _2011
%% --- this can be suppressed by adding object(C)

reachable_op(drive2(A,B,C)):-
    reachable_fact(location(A)),
    reachable_fact(location(B)),
    reachable_fact(connected(A,B)).

reachable_op(drive2_2(A,B,C)):-
    reachable_fact(location(A)),
    reachable_fact(location(B)),
    reachable_fact(connected(A,B)),
    object(C).

%% For simplicity, we can also add all objects.
reachable_op(drive2_3(A,B,C)):-
    reachable_fact(location(A)),
    reachable_fact(location(B)),
    reachable_fact(connected(A,B)),
    object(A)
    object(B)
    object(C).

%% free variable in body
%% --- cause the same variable C to be matched twice (once for C=a, next for C=b)
%% In actions this DOES NOT HAPPEN

reachable_op(drive3(A,B)):-
    reachable_fact(location(A)),
    reachable_fact(location(B)),
    reachable_fact(location(C)),
    reachable_fact(connected(A,B)).


%%%% axioms

reachable_fact(connected_bidi(A,B)):-
    reachable_fact(location(A)),
    reachable_fact(location(B)),
    reachable_fact(connected(A,B)).

%% free variable in head
%% --- causes C to remain a variable _2011
%% --- this can be suppressed by adding object(C)

reachable_fact(connected_bidi2(A,B,C)):-
    reachable_fact(location(A)),
    reachable_fact(location(B)),
    reachable_fact(connected(A,B)).

reachable_fact(connected_bidi2_2(A,B,C)):-
    reachable_fact(location(A)),
    reachable_fact(location(B)),
    reachable_fact(connected(A,B)),
    object(C).

%% free variable in body
%% --- cause the same variable C to be matched twice (once for C=a, next for C=b)
%% In axioms this HAPPENS
%% C is existentially quantified

reachable_fact(connected_bidi3(A,B)):-
    reachable_fact(location(A)),
    reachable_fact(location(B)),
    reachable_fact(location(C)),
    reachable_fact(connected(A,B)).

%% We don't care which value C takes, and since we only care about reachability,
%% we can cut it

reachable_fact(connected_bidi3_2(A,B)):-
    reachable_fact(location(A)),
    reachable_fact(location(B)),
    reachable_fact(location(C)),
    reachable_fact(connected(A,B)),
    !.

main:-
    forall(reachable_fact(X),writeln(X)),
    nl,
    forall(reachable_op(X),writeln(X)).
    
:- initialization(main).
