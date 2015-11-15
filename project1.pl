% Import the words.
:- discontiguous(s/6).
:- discontiguous(s/4).
:- consult('wn_s.pl').
% Import the definitions.
:- consult('wn_g').
% Import the pronto morph engine.
:- consult('pronto_morph_engine').

:- consult('312-pess').



%% Pass a word atom in the first argument and second argument returns the corresponding definition(s)
%%
%% ?- definition('hello',G).
%% G = 'an expression of greeting; "every morning they exchanged polite hellos"'.
definition(Word, Definition) :- s(SynsetId,_,Word,_,_,_), g(SynsetId, Definition).

%% reads from the user input a word and outputs all the possible morphs
%%
%% E.g
%% ?- word_line_morph.
%% triples
%% [[[triples]],[[tripl,-pl]],[[triplis,-pl]],[[triple,-s]]]
%% true .
word_line_morph :- read_word(X), morph_chars_bag([_|X],Y), write(Y).

morph(W,M) :- morph_atoms([W], [[M|_]|_]).

go :-
greeting, 
repeat, 
write('> '), 
read(X), 
do(X), 
X == quit.

greeting :-
write('This is the Native Prolog shell.'), nl, 
write('Enter load, consult, or quit at the prompt.'), nl.

do(load) :- load_kb, !.

do(goal) :- goal, !.

do(solve) :- solve, !.

do(help) :- help, !.

do(rule) :- add_rule, !.

do(quit).

do(X) :-
write(X), 
write('is not a legal command.'), nl, 
fail.

add_rule :-
write('Enter a new rule followed by a period: '),
read_sentence(X),
process(['rule:'|X]),
nl,
write('Rule loaded'),
nl.

load_kb :-
write('Enter file name: '), 
read(F), 
load_rules(F).

goal :-
write('Enter the new goal, followed by a period: '), 
set_top_goal(F),
write('Understood goal: '),
write_sentence(F),nl.

help :-
write('Type help. load. solve. or quit. at the prompt. Notice the period after each command!'), nl.

%% sample command for question #3
%%
%% ?- load_rules('first_week_tasks_3.kb').
%% ?- listing(n).
%% n(thing).
%% n(project).
%% n(instructor).
%% n(word).
%% true
%% ?- listing(v).
%% v(lift).
%% true.
%% ?- listing(adj).
%% adj(late).
%% adj(tired).
%% adj(last).
%% ?- listing(adv).
%% adv(silly).