% swipl -s test.pl or [test] in swipl interpreter = consult('test.pl').
% halt.. to exit swipl
% trace. - start the debugger for tracing the prolog evaluation
% notrace. - stop debugger

% :- - define rule (like 'if')
% , - AND
% ; - OR
% ! - if all before cut is true it execute rest and stop
%   - if all before cut is false the cut is not called and continue
%   -!WRONG! in the beginning: dont continue
%   -!WRONG! in body(even at the end): stop if current clause is true or continue on false

% not, \+ - NOT: not(Goal) :- call(Goal), !, fail. not(Goal).
% write() - write on stdout
% writeq() - write on stdout with displaying quates
% read(X) - read form stdin 
% get(X) - get single character from stdin as ascii value
% put(X) - put single character itself (not ascii value)
% tab(n) - place n spaces
% nl - newline
% listing(predicate) - list all facts with this predicate
% listing.           - list whole database
% '' - map text to atom
% "" - map text to string - list of integers/characters (atoms of length 1)

% fact: loves(romeo, juliet).
%   loves - predicate
%   romeo, juliet - atoms/constants

% rule: loves(romeo, juliet) :- A,B.
%   loves(romeo, juliet) if A and B
%   head: loves
%   neck: :-
%   body: A,B

% facts and rules = clauses
% Facts and Rules are in database Queries are used in interpreter to check different facts and rules
% atoms/constants - start with lowercase letters
% variables       - start with Uppercase letters
% predicates names are also atoms

% male(X), female(Y). - all combination of male and female

% rules - when u want to say a fact depend on a group of other facts
%   cant use undefined predicate
%   can use defined predicate with other atoms (it evaluate to FALSE)
% _ - anonymous variable, dont show the posible values of this argument
% predicate(_). - checks if predicate exists

:- initialization(main).
main :- write('Hello World!').

loves(romeo, juliet).
loves(juliet, rOmeo) :- loves(romeo, juliet).
loves(juliet, romeo) :- loves(romeo, juliet).

'romeo dog'.

% -----------------
happy(juliet).
happy(alice).
with_john(alice).
brr(X) :- true.

dance(alice) :-
    happy(alice),
    with_john(alice).


dance(juliet) :-
    happy(juliet),
    brr(juliet).
% -----------------

% -----------------
female(alice).
female(betsy).
female(diana).

parent(albert, bob).
parent(albert, betsy).
parent(albert, bill).

parent(alice, bob).
parent(alice, betsy).
parent(alice, bill).

parent(bob, carl).
parent(bob, charlie).

get_grandchild :-
    parent(albert, X),
    parent(X, Y),
    write("Albert, grandchild is "),
    write(Y), nl.


% format ('~w ~s ~n', [X, 'some string'])
%   ~w - variable
%   ~s - string
%   ~n - new line
%   ~2f - float with 2 decimal places
get_grandparent :-
    parent(X, carl),
    parent(X, charlie),
    parent(Y, X),
    format("~w ~s grandparent ~n", [Y, "is the"]).
% -----------------

% -----------------
grandparent(C, G) :-
    parent(P, C),
    parent(G, P).
% -----------------

% -----------------
stabs(tybalt, mercutio, sword).
hates(romeo, X) :- stabs(X, mercutio, sword).
% -----------------

% -----------------
% cases of predicate
grade(5) :- write('Go kindergarden').
grade(6) :- write('Go school').
grade(Other) :-
    Grade is Other - 5,
    format('Your grade is ~w', [Grade]).
% Var = value (Var is value)
%   = is kinda same as 'is'
%   value could be constant,variable or some expression
%   'is' evaluate the expression
% -----------------

% -----------------
% complex terms/structures
%   functors: functors has arity (example: likes(mary, pizza), likes/2 is the functor)
has(alber, olive). % No
owns(albert, pet(cat, olive)). % Yes

customer(tom, smith, 20.55).
customer(sally, smith, 120.55).

get_cust_bal(FName, LName) :-
    customer(FName, LName, Bal),
    write(FName), tab(1),
    format("~w owes us $~2f ~n", [LName, Bal]).

vertical(line(point(X, Y), point(X, Y2))).
% -----------------

% -----------------
% comparsion
alice = alice.       % true
'alice' = alice.     % true
\+ (alice = albert). % true
3 >= 15.             % false
3 =< 15.             % true

% = with variable set its value literally (dont compare it and dont evaluate)
W = alice.

% variables could be assign to anything also another var
Rand1 = Rand2.

rich(money, X) = rich(Y, no_debt).
%   X = no_debt,
%   Y = money.
% -----------------

% -----------------
% recursion
%   examples uses parent predicate
related_wrapper(X, Y) :- parent(X, Y).
related_wrapper(X, Y) :- parent(X, Z), related_wrapper(Z, Y).

related(X, Y) :- related_wrapper(X, Y);related_wrapper(Y, X).

or(X, Y) :- X, !.
or(X, Y) :- Y.
% -----------------

% -----------------
% expressions
(3* 10) >= (50/2).
5+5 = 1+9. % returns false cuz = dont evaluate expressions

% =:= - operator for equality that evaluate expressions
% =\= - operator for unequality that evaluate expressions
5+5 =:= 1+9. % true
5+5 =\= 2*8. % true


% random(bot, top, X) - give X random value between bot and top
% between(0, 10, X) - iterate over all values between 0 and 10
% succ(2, X) - increments 2 with 1

% abs(n) - return absolute value of n
% max(x, y) - return max of x and y
% min(x, y) - return min of x and y
% truncate(n) - just remove the floating point values (round(10.56) = 10)
% round(10.56) - convert float to integer (in this example 11)
% floor(n) - round down
% ceiling(n) - round up
% ** - power
X is 2 ** 3.
% // - divides and disregards decimals (integer division)
X is 5//2. % 2
% mod(x, y) - reminder of x/y
X is mod(5, 2). % 1

% other functions: sqrt, sin, cos, tan, asin, acos, atan, atan2, sinh, asinh, acosh, atanh, log, log10, exp, pi, e 
% -----------------

% -----------------
% writing to file
% open(File, write, Stream) - opens File(name of file) with write(function) and use Stream(the connection to the file) var to save values on it
% close(Stream) - closes the connection to the file
% end_of_file - marks the end of file
write_to_file(File, Text) :-
    open(File, write, Stream),
    write(Stream, Text), nl,
    close(Stream).

read_file(File) :-
    open(File, read, Stream),
    get_char(Stream, Char1),
    process_stream(Char1, Stream),
    close(Stream).

process_stream(end_of_file, _) :- !. % ! is named cut. It stops execution
process_stream(Char, Stream) :-
    write(Char),
    get_char(Stream, Char1),
    process_stream(Char1, Stream).
% -----------------

% -----------------
% looping
count_down(Low, High) :-
    between(Low, High, Y),
    Z is High - Y,
    write(Z), nl.

count_up(Low, High) :-
    between(Low, High, Y),
    Z is Low + Y,
    write(Z), nl.

guess_num :- loop(start).

loop(15) :- write("You guessed it").
loop(X) :- 
    X \= 15,
    write(X),
    write(" is not the number"), nl,
    write("Guess number "),
    read(Guess),
    loop(Guess).
% -----------------

% -----------------
% change database, aka predicates
% :- dynamic(predicate/arity) - marks predicates as dynamic (add this before they are used :D )
:- dynamic(father/2).

% assertz - add the predicate value to the end
% asserta - add the predicate value to the beginning
asserta(father(ivan, gosho)).

% retract(predicate(..)) - delete a clause
% retractall(predicate(_, _)) - delete all clauses that match
retract(father(ivan, gosho)).
% -----------------

% -----------------
% list
% [new_val | [old, list, data]] - constructor of list
write([albert | [alice, bob]]), nl.

% [H | T] - H is head, T is tail
[H | T] = [1,2,3]. % H is 1
[H1, H2 | T] = [1,2,3].
[_, H2, _ | _] = [1,2,3].

% lists could have diffrent kind of elements
[A, [H|T], B, C | T2] = [1, ["zdr", t], b, c, t2]. % [1, ['a' 'zdr', 5], 2, 'c'].

% member(el, List) - check if el is in List
% reverse(List, X) - reverse List in X
% append(l1, l2, X) - append l1 and l2
member(X, [1,2,3]).

d :- L = [1,2,3], member(2, L).

write_list([]).
write_list([H | T]) :-
    write(H), nl,
    write_list(T).
% -----------------

% -----------------
% strings
% name(string, X) - make X a list of all ascii values of string's characters
% name(X, ascii_list) - make X a string of characters refered to ascii_list values
name('zdr', Name). % Name = [122, 100, 114].

join_str([], X) :- X = "".
join_str([H|T], X) :-
    join_str(T, Y),
    name(H, HList),
    name(Y, YList),
    append(HList, YList, XList),
    name(X, XList).

% Tuple term construction with the ,/2 operator is right-associative so:
(1,2,3,4) = (1, (2, (3, 4))).
% -----------------


%------------------
X = some_atom, atom(X). % means atom(X) is true if X is bound to some_atom
% X = some_atom