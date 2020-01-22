% 2017
%1
%a
% False
% does not terminate
% does not terminate, goes out of stack
% L = []
% false
% X = [c]

%c
lessSome(List1, List2):-  member(X, List1), member(Y, List2),
                          number(X), number(Y), X<Y, !.
%d
lessAll([], _ ).
lessAll([H|T], List2):- lessThanAll(H, List2), lessAll(T, List2).
lessThanAll(_, []).
lessThanAll(X, [H|T]):- X<H, lessThanAll(X,T).

%e
append([], List, List).
append([H|T], List2, [H|List3]):- append(T, List2, List3).

%2a
sum([X], X).
sum([X,Y|List], Sum):- sum([Y|List], PR), Sum is PR+X.

%b
length1(List, N):- tailLength(List, 0, N).

tailLength([], Acc, Acc).
tailLength([_|T], Acc, N):- NewAcc is Acc+1, tailLength(T, NewAcc, N).

%c
split(Number, List, Small, Big):- member(Number, List), allNumeral(List), spl(Number, List, Small, Big).
allNumeral([]).
allNumeral([H|T]):- number(H), allNumeral(T).

spl(_, [], [], []).
spl(Number, [H|T], [H|Small], Big):- Number>H, spl(Number, T, Small, Big).
spl(Number, [H|T], Small, Big):- Number =:= H, spl(Number, T, Small, Big).
spl(Number, [H|T], Small, [H|Big]):- Number<H, spl(Number, T, Small, Big).

%d
median(List, Median) :-  isSet(List), split(Median, List, Small, Big), length1(Big, N), length1(Small, N).

isSet([]).
isSet([H|T]):- nonMember(H, T), isSet(T).

nonMember(_, []).
nonMember(X, [H|T]):- X\=H, nonMember(X, T).

medianAlt(List, Median) :-  setof(X, member(X, List), List),
                            split(Median, List, Small, Big), length1(Big, N), length1(Small, N).

%e
remove(X, List, Rest) :- member(X, List), rm(X, List, Rest).

rm(_, [], []).
rm(X, [X|T], Rest):- rm(X, T, Rest).
rm(X, [H|T], [H|Rest]):- X\=H, rm(X, T, Rest).

%f
permute([],[]).
permute([H|T], List2):- remove(H, List2, NewList2), permute(T, NewList2).


% 2016
% a
% X = 1
% False
% False
% true
% False
% true
% dictionary error, though should be true on older versions
% false
% non instantiated for X
% L = [], for some reason


%b
isSet2([]).
isSet2([H|T]):- nonMember(H, T), isSet2(T).

nonMember2(_, []).
nonMember2(X, [H|T]):- X\=H, nonMember2(X, T).

isSetAlt(List) :-  setof(X, member(X, List), List).

%c
moreThanOne(List):- setof(X, member(X, List), SetList), length(SetList, N), N>1.

%d
moreThan(List, Number):- setof(X, member(X, List), SetList), length(SetList, N), N>Number.

%2
%a i
fac(0, 1).
fac(N, Factorial):- N>0, LowerN is N-1, fac(LowerN, LowerFactorial), Factorial is LowerFactorial * N.

% ii

fac2(N, Factorial):- tailFac(N, 1, Factorial).

tailFac(0, Fac, Fac).
tailFac(N, Acc, Fac):- N>0, LowerN is N-1, NewAcc is Acc*N, tailFac(LowerN, NewAcc, Fac).

%b i
fib(0, 0).
fib(1, 1).
fib(N, Fibonnacci):- N>1,
                     LowerN is N-1, LowerN2 is N-2,
                     fib(LowerN, PR1), fib(LowerN2, PR2),
                     Fibonnacci is PR1+ PR2.

% ii
tailFib(N, Fibonnacci):- fibAcc(N, 0, 1, Fibonnacci).

fibAcc(0, _, Fib, Fib).
fibAcc(N, A, B, Fib):- N>0, NewB is B+A, LowerN is N-1,
                       fibAcc(LowerN, B, NewB, Fib).


%15
%1a
% first two are facts, third is a rule, third is best as will verify an item is a lamb and is white

%b
% False
% X = 3+2.
% False
% args not instantiated
% X = 5.
% false
% true.
% true

%c
% see 2 c 2017

%d
sumOfPowers(N,SoP):- sOP(N, 0, SoP).

sOP(0, SoP, SoP).
sOP(N, Acc, SoP):- N>0, NewAcc is Acc + N**N, LowerN is N-1, sOP(LowerN, NewAcc, SoP).

%2
%a
member1(X, [X|_]):- !.
member1(X, [H|T]):- H\=X,  member1(X, T).

%b
% cut is green

%c
last(X, [X]) :- !.
last(X, [_|T]) :- last(X, T).

%d
multiple(X, List):- findall(C, (member(C, List), X=C), XList), length(XList, N), N>1.

%e
next(A, B, [A, B|_]).
next(A, B, [_|T]):- next(A,B,T).

%f can bugger right off
