%q1
isSet([]).
isSet([H|T]) :- \+member(H,T), isSet(T).

moreThanOne([H|T]) :- member(X,T), H \= X; moreThanOne(T).

moreThan(L, Num) :- numDistinct(L,0,X), X > Num.
numDistinct([],X,X).
numDistinct([H|T],Acc,X) :- \+member(H,T),
                            NewAcc is Acc+1,
                            numDistinct(T, NewAcc,X).
numDistinct([H|T],Acc,X) :- member(H,T),
                            numDistinct(T, Acc,X).
