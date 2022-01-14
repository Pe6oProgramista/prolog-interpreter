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
d.
nn(ff()).
ff().

succ(one, two).
succ(two, three).
succ(three, four).
succ(four, five).
succ(five, six).
succ(six, seven).
eq(X, X).
lt(X, Y) :- succ(X, Y).
lt(X, Y) :- succ(X, Z), lt(Z, Y).
lte(X, Y) :- eq(X, Y).
lte(X, Y) :- lt(X, Y).
gt(X, Y) :- lt(Y, X).
gte(X, Y) :- lte(Y, X).
range(X, Y, Z) :- lte(X, Y), lte(Y, Z).

test(a).
test(a).
test(b).
test(a).

pff :- test(X), test(Y), eq(X,Y).

gen(X, Z) :- eq(X, Z); gen(s(X), Z).

or(X,Y) :- X; not(X), Y.