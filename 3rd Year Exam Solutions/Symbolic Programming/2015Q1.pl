split(N, L, Small, Big) :- smallerL(N, L, Small), geL(N,L,Big).

smallerL(N, [], []).
smallerL(N, [H|T], [H|Small]) :- H < N, smallerL(N, T, Small).
smallerL(N, [H|T], Small) :- H >= N, smallerL(N, T, Small).

geL(N, [], []).
geL(N, [H|T], [H|Big]) :- H >= N, geL(N, T, Big).
geL(N, [H|T], Big) :- H < N, geL(N, T, Big).

% I swear I didn't look at this - 
% http://stackoverflow.com/questions/9576494/prolog-factorial-recursion

sop(N, R) :- sop(N, 0, R).
sop(0, R, R) :- !.
sop(N, Acc, R) :-
    NewN is N - 1,
    NewAcc is Acc + N**N,
    sop(NewN, NewAcc, R).
