% -*- mode : prolog -*-

%% for yap?
%% :- use_module(library(pl/tabling)).
%% for swipl?
%% :- use_module(library(tabling)).
%% :- set_prolog_flag(redefined,off).
:-table(fib/2).

fib(0, 1) :- !.
fib(1, 1) :- !.
fib(N, F) :-
        N > 1,
        N1 is N-1,
        N2 is N-2,
        fib(N1, F1),
        fib(N2, F2),
        F is F1+F2.

main :- time(fib(30, X)), write(X), halt.

%% usage: ../../cl-prolog/bprolog/BProlog/bp -i fib.pl -g main
