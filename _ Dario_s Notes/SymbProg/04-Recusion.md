# Definitions
- Prolog predicates can be defined recursively
- A predicate is recursively defined if one or more rules in its definition refers to itself

```Prolog
isDigesting(X, Y) :- justAte(X, Y).
isDigesting(X, Y) :- justAte(X, Z), isDigesting(Z, Y).

justAte(mosjuito, blood(john)).
justAte(frog, mosjuito).
justAte(stork, frog).

?- isDigesting(stork, mosjuito).
```

```Prolog
child(anna, bridget).
child(bridget, caroline).
child(caroline, donna).
child(donna, emily).

descend(X, Y) :- child(X, Y).
descend(X, Y) :- child(X, Z), child(Z, Y).
descend(X, Y) :- child(X, Z), child(Z, U), chld(U, Y).

?- descend(anna, donna).
no.

descend(X, Y) :- child(X, Y).
descend(X, Y) :- child(X, Z), descend(Z, Y).
```

- Suppose we use the following way to write numbers
    1. `0` is a numeral
	2. `X` is a numeral, then so it `succ(X)`

```Prolog
numeral(0).
numeral(succ(X)) :- numeral(X).

?- numeral(succ(succ(succ(succ(0))))).
yes
?- numeral(X).
X=0;
X=succ(0);
X=succ(succ(0));
X=succ(succ(succ(0)));
X=succ(succ(succ(succ(0))));
```

# Prolog and Logic
- Prolog was the first reasonable attempt to create to logic programming language
    - Programmer gives a declarative specification of the problem, using the language of logic
	- The programmer should not have to tell the computer what to do
	- To get information, the programmer simply asks a query
- Prolog does some important steps in this direction, but nevertheless, Prolog is not a full logic programming language!
- Prolog has a specific way of answering queries
    - Search knowledge bse from top to bottom
	- Processes clauses from left to right
	- Backtracking to recover from bad choices
