
%%%%%%%%% Two Room Prolog Planner %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% University of Central Florida
%%% CAP4630 - Spring 2018
%%%
%%% by Gaelen Dignan and Nicholas Deleuze
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module( planner,
	   [
	       plan/4,change_state/3,conditions_met/2,member_state/2,
	       move/3,go/2,test/0,test2/0
	   ]).

:- [utils].

plan(State, Goal, _, Moves) :-	equal_set(State, Goal),
				write('moves are'), nl,
				reverse_print_stack(Moves).
plan(State, Goal, Been_list, Moves) :-
				move(Name, Preconditions, Actions),
				conditions_met(Preconditions, State),
				change_state(State, Actions, Child_state),
				not(member_state(Child_state, Been_list)),
				stack(Child_state, Been_list, New_been_list),
				stack(Name, Moves, New_moves),
			plan(Child_state, Goal, New_been_list, New_moves),!.

change_state(S, [], S).
change_state(S, [add(P)|T], S_new) :-	change_state(S, T, S2),
					add_to_set(P, S2, S_new), !.
change_state(S, [del(P)|T], S_new) :-	change_state(S, T, S2),
					remove_from_set(P, S2, S_new), !.
conditions_met(P, S) :- subset(P, S).

member_state(S, [H|_]) :-	equal_set(S, H).
member_state(S, [_|T]) :-	member_state(S, T).

/* move types */

move(pickup(X), [handempty, cranein1, clear(X), on1(X, Y)],
		[del(handempty), del(clear(X)), del(on1(X, Y)),
				 add(clear(Y)),	add(holding(X))]).

move(pickup(X), [handempty, cranein1, clear(X), ontable1(X)],
		[del(handempty), del(clear(X)), del(ontable1(X)),
				 add(holding(X))]).

move(pickup(X), [handempty, cranein2, clear(X), on2(X, Y)],
		[del(handempty), del(clear(X)), del(on2(X, Y)),
				 add(clear(Y)),	add(holding(X))]).

move(pickup(X), [handempty, cranein2, clear(X), ontable2(X)],
		[del(handempty), del(clear(X)), del(ontable2(X)),
				add(holding(X))]).

move(putdown(X), [holding(X), cranein1],
		[del(holding(X)), add(ontable1(X)), add(clear(X)),
				  add(handempty)]).

move(putdown(X), [holding(X), cranein2],
		[del(holding(X)), add(ontable2(X)), add(clear(X)),
				  add(handempty)]).

move(stack(X, Y), [holding(X), clear(Y), ontable1(Y), cranein1],
		[del(holding(X)), del(clear(Y)), add(handempty), add(on1(X, Y)),
				  add(clear(X))]).

move(stack(X, Y), [holding(X), clear(Y), on1(Y, _), cranein1],
		[del(holding(X)), del(clear(Y)), add(handempty), add(on1(X, Y)),
				  add(clear(X))]).

move(stack(X, Y), [holding(X), clear(Y), ontable2(Y), cranein2],
		[del(holding(X)), del(clear(Y)), add(handempty), add(on2(X, Y)),
				  add(clear(X))]).

move(stack(X, Y), [holding(X), clear(Y), on2(Y, _), cranein2],
		[del(holding(X)), del(clear(Y)), add(handempty), add(on2(X, Y)),
				  add(clear(X))]).

move(goroom1, [cranein2],
		[del(cranein2), add(cranein1)]).

move(goroom2, [cranein1],
		[del(cranein1), add(cranein2)]).

/* run commands */

go(S, G) :- plan(S, G, [S], []).

test :- go([handempty, cranein1, ontable1(b), ontable1(c), on1(a, b), clear(c), clear(a)],
	          [handempty, cranein1, ontable1(c), on1(a,b), on1(b, c), clear(a)]).

test2 :- go([handempty, cranein1, ontable1(b), ontable1(c), on1(a, b), clear(c), clear(a)],
	          [handempty, cranein1, ontable2(b), on2(c, b), on2(a, c), clear(a)]).
