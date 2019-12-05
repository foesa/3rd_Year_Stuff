numeral(0).
numeral(s(X))  :-  numeral(X).
numeral(X+Y) :- numeral(X),numeral(Y).
numeral(p(X)) :- numeral(X).

add(0,Y,Y).
add(s(X),Y,s(Z))  :- add(X,Y,Z).

add2(-(X-F),Y,Z):- subtract(F-X,Y,Z).
add2(X,-(Y-F),Z):- subtract(X,F-Y,Z).
add2(X,Y-F,Z):- subtract(X,Y-F,Z).
add2(X-F,Y,Z):- subtract(X-F,Y,Z).
add2(-X,Y+F,Z) :- minus(X,R),add2(Y,F,W),add2(R,W,Z).
add2(X+F,-Y,Z) :- minus(Y,R),add2(X,F,W), add2(R,W,Z).
add2(-X,Y,Z) :- minus(X,R),add2(R,Y,Z).
add2(X,-Y,Z) :- minus(Y,R),add2(R,X,Z).

add2(s(p(X)),Y,Z):- add2(X,Y,Z).
add2(X,s(p(Y)),Z):- add2(Y,X,Z).
add2(p(s(X)),Y,Z):- add2(X,Y,Z).
add2(X,p(s(Y)),Z):- add2(Y,X,Z).
add2(p(0)+s(X),Y,Z):- add2(X,Y,Z).
add2(X,p(0)+s(Y),Z):- add2(Y,X,Z).

add2(X,s(0+Y),s(T)):- add2(0,Y+X,T).
add2(0,Y+F,T):- add2(Y,F,T).
add2(s(X)+F,Y,s(T)):- add(X,F,Z),add2(Y,Z,T).
add2(X,s(Y)+F,s(T)):- add(Y,F,Z),add2(X,Z,T).
add2(s(Y),R,s(T)) :- add(Y,R,T).

add2(0,Y,Y).
add2(p(X),s(Y),Z):- add2(X,Y,Z).
add2(s(X),p(Y),Z):-add2(X,Y,Z).
add2(p(X),Y,p(Z)) :- add(X,Y,Z).


minus(s(p(X)),Z):-minus(X,Z).
minus(p(s(X)),Z):- minus(X,Z).
minus(s(X),p(Z)) :- minus(X,Z).
minus(p(X),s(Z)) :- minus(X,Z).
minus(Z,0).

subtract(X-F,Y,Z):- subtract(X,F,R),subtract(R,Y,Z).
subtract(X,Y-F,Z) :- subtract(Y,F,R),subtract(X,R,Z).
subtract(s(X),p(Y),Z) :- add2(X,Y,Z).
subtract(p(X),s(Y),Z) :- add2(X,Y,Z).
subtract(X,-Y,Z):- add2(X,Y,Z).
subtract(X,Y,Z):- add2(-Y,X,Z).
