% Import the words.
:- discontiguous(s/6).
:- discontiguous(s/4).
:- consult('wn_s.pl').

% Import the definitions.
:- consult('wn_g').

% Import the pronto morph engine.
:- consult('pronto_morph_engine').

% Import the 312 Prolog Expert System Shell
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

verb(W) :- morph(W,M), s(_,_,M,v,_,_), assertz(v(W)).
noun(W) :- morph(W,M), s(_,_,M,n,_,_), assertz(n(W)).
adverb(W) :- morph(W,M), s(_,_,M,r,_,_), assertz(adv(W)).
adjective(W) :- morph(W,M), s(_,_,M,s,_,_), assertz(adj(W)).
adjective(W) :- morph(W,M), s(_,_,M,a,_,_), assertz(adj(W)).

add_unknown_words([]).
add_unknown_words([Head|Tail]) :- add_word(Head), add_unknown_words(Tail).
add_word(W) :- \+v(W), verb(W).
add_word(W) :- \+n(W), noun(W).
add_word(W) :- \+adv(W), adverb(W).
add_word(W) :- \+adj(W), adjective(W).
add_word(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interpreter loop                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

go :-
greeting,
repeat,
write('> '),
read(X),
do(X),
X == quit.

%%%%%%%%%%%%%%%%%%% Commands %%%%%%%%%%%%%%%%%%%%%%%%%%

greeting :-
write('This is the Native Prolog shell.'), nl,
write('Enter load, goal, solve, rule, help, or quit at the prompt.'), nl.

do(load) :- load_kb, !.

do(goal) :- goal, !.

do(solve) :- solve, !.

do(rule) :- add_rule, !.

do(help) :- help, !.

do(quit).

do(X) :-
write(X),
write(' is not a legal command.'), nl,
fail.

load_kb :-
write('Enter file name in single quotes, followed by a period: '),
read(X),
load_rules(X).

goal :-
write('Enter the new goal, followed by a period: '),
set_top_goal(X),
write('Understood goal: '),
write_sentence(X), nl.

add_rule :-
write('Enter a new rule, followed by a period: '),
read_sentence(X),
add_unknown_words(X),
process(['rule:'|X]), nl,
write('Rule loaded'), nl.

help :-
write('Type help. load. goal. solve. rule. or quit. at the prompt. Notice the period after each command!'), nl.
