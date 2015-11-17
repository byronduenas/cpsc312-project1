
%% Byron Duenas
%% 34095117
%% v5e8
%%
%% Tongli Li
%% 15688112
%% w6d8
%%
%% Brian Taylor
%% 52311859
%% z2b0b

% Hide discontiguous warnings made by WordNet files
:- discontiguous(s/6).
:- discontiguous(s/4).

% Import the words.
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

% Takes word W and returns a morph M
morph(W,M) :- morph_atoms([W], [[M|_]|_]).

% If a morph of the word is found in WordNet and its not already asserted then assert it
verb(W) :- morph(W,M), s(_,_,M,v,_,_), \+v(W), assertz(v(W)).
noun(W) :- morph(W,M), s(_,_,M,n,_,_), \+n(W), assertz(n(W)).
adverb(W) :- morph(W,M), s(_,_,M,r,_,_), \+adv(W), assertz(adv(W)).
adjective(W) :- morph(W,M), s(_,_,M,s,_,_), \+adj(W), assertz(adj(W)).
adjective(W) :- morph(W,M), s(_,_,M,a,_,_), \+adj(W), assertz(adj(W)).

% Takes a list, and for each unknown word asserts v/1, adj/1, n/1, and adv/1 clauses
add_unknown_words([]).
add_unknown_words([Head|Tail]) :- add_word(Head), add_unknown_words(Tail).

%% Ignore adding certain keywords used by grammar
add_word(is) :- !.
add_word(a) :- !.
add_word(are) :- !.
add_word(has) :- !.
add_word(have) :- !.
add_word(if) :- !.
add_word(then) :- !.
add_word(does) :- !.
add_word(an) :- !.
add_word(and) :- !.
add_word(its) :- !.
add_word(the) :- !.
add_word(it) :- !.

% Only call verb(W) if verb(W) is not asserted
add_word(W) :- \+v(W), verb(W), fail.
% Only call noun(W) if noun(W) is not asserted
add_word(W) :- \+n(W), noun(W), fail.
% Only call adverb(W) if adverb(W) is not asserted
add_word(W) :- \+adv(W), adverb(W), fail.
% Only call adjective(W) if adjective(W) is not asserted
add_word(W) :- \+adj(W), adjective(W), fail.
% Always succeed so add_unknown_words/1 never fails
add_word(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interpreter loop                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The main loop that executes functions based on user input.
main :-
greeting,
repeat,
write('> '),
read(X),
do(X),
X == quit.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Commands                                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Greets the user by writing the following message to the console
greeting :-
write('This is the CPSC312 Prolog Expert System Shell.'), nl,
write('Based on Amzi\'s "native Prolog shell".'), nl,
write('Type help. load. goal. solve. rule. list. clear. or quit. at the prompt.'), nl,
write('Notice the period after each command!'), nl.

% Various shell commands that call meaningful helper functions
do(load) :- load_kb, !.

do(goal) :- goal, !.

do(solve) :- solve_helper, !.

do(rule) :- add_rule, !.

do(list) :- list_rules, !.

do(clear) :- clear_helper, !.

do(help) :- help, !.

do(quit).

% Show error message when user inputs illegal command
do(X) :-
write(X),
write(' is not a legal command.'), nl,
fail.

%%%%%%%%%%%%%%%%%%% Helper Functions %%%%%%%%%%%%%%%%%%%%%%%%%%

% Prompts the user to enter a filename of a knowledge base and calls load_rules/1 to assert the rules
% Also prints out the list of rules loaded from the knowledge base
load_kb :-
write('Enter file name in single quotes, followed by a period: '),
read(X),
load_rules(X),
write('Understood: '), nl,
list_rules,
write('Rules loaded.'), nl.

% Prompts the user to enter a goal and sets a new top goal by calling set_top_goal/1
% Also prints out the new understood goal after inputting it
goal :-
write('Enter the new goal, followed by a period: '),
set_top_goal(X),
write('Understood goal: '),
write_sentence(X), nl.

% Fall back to the default goal when the entered rule is invalid.
goal :-
write('Unable to understand goal, goal set to "what is it".'), nl.

% If rule/2 are loaded and the rule that exists is not the top goal rule.
solve_helper :-
current_predicate(rule/2),
rule(X,_),
X \= top_goal(_),
solve.

% Fall back when no rule/2 are loaded.
solve_helper :-
write('Cannot solve, no rules are loaded.'), nl.

% Prompts the user to enter a new rule and adds new words contained in the rule, and asserts the new rule
% Also prints out the new understood rule after inputting it
add_rule :-
write('Enter a new rule, followed by a period: '),
read_sentence(X),
add_unknown_words(X),
process(['rule:'|X]),
write('Understood rule: '),
write_sentence(X), nl.

% If rule/2 are loaded and the rule that exists is not the top goal rule.
list_rules :-
current_predicate(rule/2),
rule(X,_),
X \= top_goal(_),
list_rules_exist.

% Fall back when the only rule/2 loaded is the top goal rule.
list_rules :-
current_predicate(rule/2),
rule(X,_),
X = top_goal(_),
write('No rules are loaded.'), nl.

% Terminate rule listing cleanly.
list_rules :-
current_predicate(rule/2).

% If no rule/2 are loaded.
list_rules :-
not(current_predicate(rule/2)),
write('No rules are loaded.'), nl.

% Write each rule that exists that is not the top goal.
list_rules_exist :-
rule(X,Y),
X \= top_goal(_),
plain_gloss([rule(X,Y)], T),
write_sentence(T), nl,
fail.

list_rules_exist :-
rule(X,_),
X = top_goal(_).

clear_helper :-
clear_db,
write('Cleared rules database.'), nl.

% Prints out to the user the list of available commands
help :-
write('Type help. load. goal. solve. rule. list. clear. or quit. at the prompt.'), nl,
write('Notice the period after each command!'), nl.
