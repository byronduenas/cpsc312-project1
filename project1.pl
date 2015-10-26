% Import the words.
:- consult('wn_s.pl').
% Import the definitions.
:- consult('wn_g').
% Import the pronto morph engine.
:- consult('pronto_morph_engine').

:- consult('312-pess-grammar').

definition(Word, Definition) :- s(SynsetId,_,Word,_,_,_), g(SynsetId, Definition).

word_line_morph :- read_word(X), morph_chars_bag([_|X],Y), write(Y).