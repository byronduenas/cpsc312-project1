%% See full project at
%% https://github.com/byronduenas/Prolog-Expert-System-Shell

% Import the words.
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

%% sample command for question #3
%%
%% ?- load_rules('first_week_tasks_3.kb').
%% [[thing,noun]][[lift,verb]][[late,adjective],[project,noun],[silly,adverb],[tired,adjective],[instructor,noun]][[last,adjective],[word,noun]]rules loaded
%% true.
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