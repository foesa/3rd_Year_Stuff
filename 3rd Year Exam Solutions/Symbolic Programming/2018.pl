% q1 c)

% For x to be able to fly it must be a bird and not a penguin
fly(X) :- bird(X), \+ penguin(X).

% q2 a)
% Define a binary predicate member(X,L) that is true when X is a member of the list L
member(X, [H|T]) :- X == H; member(X, T).

% Other implementation
member(X, [X|_]) :- !.
member(X, [H|T]) :- X \= H, member(X,T).

% q2 b)
% Define a binary predicate nonmember(X,L) that is true when X is not a member of the list L
nonmember(_, []).
nonmember(X, [H|T]) :- X \= H, nonmember(X, T).

% Other implementation
nonmember(X, L) :- \+ member(X,L).

% q2 c)
% Define the 3-ary predicate diff(X, L1, L2) that is true when X is a member of L1 but not L2
diff(X, L1, L2) :- member(X, L1), nonmember(X, L2).

% q2 d)
% Define the 4-ary predicate sublist(+L, +Begin, +End, ?Subl) that given a list L and positive integers
% Begin and End returns the SubL consisting of members of L between list positions begin and end both included
sublist(L, B, E, Subl) :- subl_acc(L, B, E, [], Subl)

subl_acc([], _, _, Acc, Acc).

subl_acc([H|T], B, E, Acc, SubL) :-
  H >= B,
  H =< E,
  append([H], Acc, NewAcc),
  subl(T, B, E, NewAcc, SubL).

subl_acc([_|T], B, E, Acc, SubL) :-
  subl_acc(T, B, E, Acc, SubL).

% q2 e)
% Define a predicate to return the nth harmonic number
harmonic(N, H) :- harmonic_acc(N, 1, 0, H).

harmonic_acc(N, AccNum, AccTotal, H) :-
  AccNum =< N,
  NewAccTotal is AccTotal + (1/AccNum),
  NewAccNum is AccNum + 1,
  harmonic_acc(N, NewAccNum, NewAccTotal, H).

harmonic_acc(N, AccNum, AccTotal, H) :-
  AccNum =:= (N + 1),
  H = AccTotal.

% q3 a)
% What are difference lists and how are they useful?


% Write a DCG for the set of strings a^n b^n+m c^m
s -> ap, bp.

ap -> [].
ap -> [a], ap, [b].

bp -> [].
bp -> [b], bp, [c].
