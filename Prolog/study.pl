lengther([],0).
lengther([_|L],N):- lengther(L,X), N is X +1.

min([Y],X):- X = Y.
min([H|T],X):- min(T,X), X < H.
min([H|T],H):- min(T,X), X > H.

max([Y],X):- X = Y.
max([H|T],H):- max(T,X), X <H.
max([H|T],X):- max(T,X), X>H.

lessSome(L1,L2):- max(L2,X), lessSomer(L1,X).
lessSomer([H|_],X):- H<X.
lessSomer([_|T],X):- lessSomer(T,X).

appender([],L,L).
appender([H|T],L,[H|X]) :- appender(T,L,X).

split(X,[],[],[]).
split(X,[H|T],[H|Y],L):- X > H, split(X,T,Y,L).
split(X,[H|T],Y,[H|L]):- X < H, split(X,T,Y,L).
split(X,[H|T],Y,L):- H =:= X, split(X,T,Y,L).

median(L,X) :- length(L,V),1 is mod(V,2),split(X,L,Y,Z),
length(Y,F),length(Z,R), F =:= R.

remove(X,[],[]).
remove(X,[H|T],[H|V]):- X =\= H, remove(X,T,V).
remove(X,[H|T],V):- X =:= H, remove(X,T,V).

sublist(X,B,E,R) :- N is 1,sublist(X,B,E,R,N). /* X is input list, B is where sublist starts inclusive,
E is where sublist ends inclusive and R is return list. N is used to keep track of how many vals in the list.
 */
sublist(X,B,E,[],C) :- C > E.
sublist([H|T],B,E,[H|V],C) :- C >= B,C =< E, N is C+1, sublist(T,B,E,V,N).
sublist([H|T],B,E,V,C) :- N is C +1, sublist(T,B,E,V,N).

harmonic(K,H,C,[V|T],U):- C =:= 1, V is 1, N is C+1,H is V ,harmonic(K,H,N,T,U).
harmonic(K,H,C,[V|T],U):- C =< K, V is (1/C + H), N is C+1, R is H + 1/C, harmonic(K,R,N,T,U).
harmonic(K,H,C,[],U):- C > K, U is H.
harmonic(K,U) :- harmonic(K,H,1,V,U).
