% dasdasd
likes(mary,food).
likes(ivan,food).
likes(mary,wine).
likes(john,wine).% dasdasd
likes(john,mary).

male(james1).
male(charles1).
male(charles2).
male(james2).
male(george1).

female(catherine).
female(elizabeth).
female(sophia).

parent(charles1, james1).
parent(elizabeth, james1).
parent(charles2, charles1).
parent(catherine, charles1).
parent(james2, charles1).
parent(sophia, elizabeth).
parent(george1, sophia).

happy(yolanda).
listens2Music(mia).
listens2Music(yolanda):-  happy(yolanda).
playsAirGuitar(mia):-  listens2Music(mia).
playsAirGuitar(yolanda):-  listens2Music(yolanda).

happy(vincent).
% dasdasd
listens2Music(butch).
% dasdasd
   playsAirGuitar(
         %asfgsdgdfg
         vincent):-
   % dasdasd
         (listens2Music(vincent),
         % dasdasd
         happy(vincent)).
   playsAirGuitar(
         %adfsadasds
         butch):-
         happy(butch).
   playsAirGuitar(butch):-
         listens2Music(butch).

woman(mia).
   woman(jody).
   woman(yolanda).
   
   loves(vincent,mia).
   loves(marsellus,mia).
   loves(pumpkin,honey_bunny).
   loves(honey_bunny,pumpkin).

        loves(vincent,mia).
   loves(marsellus,mia).
   loves(pumpkin,honey_bunny).
   loves(honey_bunny,pumpkin).
   
% dasdasd
% dasdasd
jealous(X,Y):-  loves(X,Z),  loves(Y,Z).  

% term(g(Y),g(g)) :- Y=gosh;Y=5;true.

term(z,z).

% dasdasd
   d.
% f :- d,d;d;d,z.
z(b) :- true.
g(Y).%Y = 5.
% p(a(X), X) :- z(Y), X.

% p(g(d,f),f) :- write("DA").

% gen(0) :- gen(0).

% p(z(), X) :- gen(0).

gen(a(X, b, f(Y)), z) :- true, X.
gen2(a(X, b, v), z).
g(X, X) :- g(X).

p(g(X)).
z(5).
% infiniteLoopTest(X) :- X < 5, X is X+1, infiniteLoopTest(X).

% infiniteLoopTest :- infiniteLoopTest(0).

% gen(a(d, b, f(Y)), z) :- true, X.
pp(z).
gg(X) :- pp(X).
gg(X) :- gg(X).

nn(ff()).

% true.
% not(false).
% false :- not(true).

% dd :- false.
dd :- false;true.
z(X) :- X.
