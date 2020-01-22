%% 2a
sum([H], H).
sum([H|T], Total) :- sum(T, SubTotal), Total is H + SubTotal.
%% 2b - tail recursive means the final call is recursive call, allowing interpreter to forget callers context/scope
len([],0).
len(L,N) :- len(L,N,0).
len([],Acc,Acc).
len([_|T],N,Acc) :- NewAcc is Acc+1, len(T,N,NewAcc).
%% 2c
split(Number, List, Small, Big) :-
  findall(S, (member(S, List), number(S), S < Number), Small),
  findall(B, (member(B, List), number(B), B > Number), Big).
%% 2d
median(List,Median) :- member(Median, List), split(Median, List, Small, Big), length(Small, Y), length(Big, Y), !.
%% 2e
remove(X,List,Rest) :- findall(Y, (member(Y, List), Y =\= X), Rest).
%% 2f
permute([], []).
permute([LH|LT], L2) :- remove(LH, L2, NewL2), permute(LT, NewL2).
