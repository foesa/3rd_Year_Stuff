% 2017

% ---------------------------------------------------------------------------
% Q1
% ---------------------------------------------------------------------------
% a) Write Prologs response to the following queries

% i)
member(0, [1,2]).
false
% This is false since 0 is not a member of the set [1,2]

% ii)
member(X, X).
X = [X|_1340]
% This instantiates X to an array with X as the head and then a variable

% iii)
setof(X, member(X,X),L).
STACK OVERFLOW
% Repeatedly tries to find the occurances of X in member(X) which is unlimited
% setof(X, Y, L) find's all X's that satisfy Y and returns them in L

% iv)
findall(X, \+member(X,X), L).
L = []
% Findall is like setof except when it fails instantiates L to []

% v)
number(X).
false
% This is false since X is a variable and not a number

% vi)
[a|[b,c]] = .(a,.(b,X)).
false
% The first term is not a valid list, otherwise it would return X = c

% b) What is an anonymous variable and why is it also called a singleton variable

% The anonymous variable in prolog is the _ symbol and it is used in predicate
% and logic when you do not care about a value. Say if you only cared about the
% head of a list you could just do [H|_]. It is also called the singleton variable
% as this is an error that warns you that a variable is named and not used, instead
% it should be replaced with _.

% c) Define a binary predicate lessSome(List1,List2) that is true exactly if List1
% and List2 are lists of numbers, and some member of List1 is less than some member
% of list2.

lessSome(L1, L2) :- isNumberList(L1), isNumberList(L2), hasLess(L1,L2).

isNumberList([]).
isNumberList([H|T]) :- number(H), isNumberList(T).

isLessThan(_, []) :- false.
isLessThan(X, [H|T]) :- X < H; isLessThan(X, T).

hasLess(_, []) :- false.
hasLess([H1|T1], L2) :- isLessThan(H1, L2); hasLess(T1, L2).

% d) Define a binary predicate lessAll(List1, List2) that is true exactly is List1
% and List2 are lists of numbers, and every member of List1 is less than List2

lessAll(L1, L2) :- isNumberList(L1), isNumberList(L2), allLess(L1,L2).

isNumberList([]).
isNumberList([H|T]) :- number(H), isNumberList(T).

isLessThan(_, []).
isLessThan(X, [H|T]) :- X < H, isLessThan(X, T).

allLess([], _).
allLess([H1|T1], L2) :- isLessThan(H1, L2), allLess(T1, L2).


% e) i) Define append(List1, List2, List3) such that append([1,2,3], [2], L)
% will return L = [1,2,3,2]

append([], L, L).
append([H1|T1], L2, [H1|T3]) :- append(T1, L2, T3).

% e) ii) To remove duplications in List3 we might use setof in two different ways

union1([],[],[]).
union1(L1, L2, L3) :- setof(X, (member(X, L1);member(X,L2)), L3).

union2([],[],[]).
union2(L1, L2, L3) :-
  setof(X, (member(X, L1); member(X, L2)), U),
  setof(X, member(X, L3), U).

% Union 1 works well, it first checks that X is a member of L1, if that doesn't
% succeed it checks if it is a member of L2 and returns the list L3 of all members

% Union 2 also works well but in this predicate you would have to define the union
% as L3 when calling it as it depends on L3 to be a list and not a variable.

% e) iii) What unary predicate p(List) can be used below to combine the predicates
% union1 and union2 as follows:

union(L1, L2, L3) :-
  (p(L3), !, union1(L1, L2, L3));
  union2(L1, L2, L3)

% The predicate that can be used is as follows:

union(L1, L2, L3) :-
  (var(L3), !, union1(L1, L2, L3));
  union2(L1, L2, L3)

% ---------------------------------------------------------------------------
% Q2
% ---------------------------------------------------------------------------
% a) Define a binary predicate sum(List, Sum) that is true exactly if List is a
% non empty list of numbers that add up to Sum.

sum(List, Sum) :- validList(List), sumOfList(List,0, Y), Y =:= Sum.

validList([]).
validList([H|T]) :- number(H), validList(T).

sumOfList([], Acc, Acc).
sumOfList([H|T], Acc, Sum) :- NewAcc is Acc + H, sumOfList(T, Acc, Sum).

% b) The predicate length(List, N) below computes the length of a list

length([], 0).
length([_|T], N) :- length(T, M), N is M+1.

% A tail-recursive predicate is a predicate where the recursive call is the last
% function invoked in the evaluation of the body of the function

length_tail_recursive(List, Length) :- length_acc(List, 0, Length).
length_acc([], Acc, Acc).
length_acc([_|T], Acc, Length) :- NewAcc is Acc + 1, length_acc(T, NewAcc, Length).

% c) Define a predicate split(Number, List, Small, Big) that is true exactly whenever
%     - Number is a member of List
%     - List is a list of isNumber
%     - Small is the list of all numbers in List smaller than Number
%     - Big is the list of all numbers in List larger than Number

split(Number, List, Small, Big) :-
  member(Number, List),
  isNumberList(List),
  splitList(Number,List,[],[],Small,Big).

splitList(_, [], Small, Big, Small, Big).
splitList(Number, [H|T],SmallAcc, BigAcc, Small, Big) :-
  (H < Number, append([H], SmallAcc, NewSmallAcc), splitList(Number, T, NewSmallAcc, BigAcc, Small, Big));
  (H > Number, append([H], BigAcc, NewBigAcc), splitList(Number, T, SmallAcc, NewBigAcc, Small, Big));
  splitList(Number, T, SmallAcc, BigAcc, Small, Big).

isNumberList([]).
isNumberList([H|T]) :- number(H), isNumberList(T).


isNumberList([]).
isNumberList([H|T]) :- number(H), isNumberList(T).

% d) Define a predicate median(List, Median) that holds precisely when
%     - List is a list of odd length where List is a number that occurs occurance
%     - Median is the median of the list - there are as many smaller & bigger members

median(List, Median) :-
  isNumberList(List),
  length(List, L), 1 is mod(L, 2),
  isUniqueList(List),
  medianAcc(List, Median).

medianAcc([H|T], Median) :-
  (splitList(H, [H|T], Small, Big), length(Small, LeftSize), length(Big, RightSize), LeftSize == RightSize, Median = H);
  (splitList(H, [H|T], Small, Big), length(Small, LeftSize), length(Big, RightSize), LeftSize \== RightSize, medianAcc(T, Median)).

isUniqueList(List) :- isUniqueListAcc(List, []).

isUniqueListAcc([H|T], Acc) :- \+member(H, Acc), append([H], Acc, NewAcc), isUniqueListAcc(T, NewAcc).
isUniqueListAcc([], _).

% e) Define a predicate remove(X, List, Rest) that is true exactly when X is a
%   member of List and Rest is the list resulting from removing X from List

remove(X, List, Rest) :-
  member(X, List),
  removeAcc(X, List, [], Rest).

removeAcc(_, [], Acc, Acc).
removeAcc(X, [H|T], Acc, Rest) :- H \== X, append([H], Acc, NewAcc), removeAcc(X, T, NewAcc, Rest).
removeAcc(X, [_|T], Acc, Rest) :- removeAcc(X, T, Acc, Rest).

% f) Use the predicate remove(X, List, Rest) to define a predicate permute(List1, List2)
%   that is true exactly when List2 is a permutation of List1 (that is, List2 differs from
%   List1 at most by a reordering of its members)

insert(X, L1, L) :- remove(X, L, L1). % what do you remove from L to give L1

permutation([X], [X]).

permutation([H|T], L) :-
  permutation(T, T1),
  insert(H, T1, L).
