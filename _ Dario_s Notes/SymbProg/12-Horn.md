# Horn Clauses
- A **literal** is an atomic formula or its negation
- A **clause** is a disjunction of literals
- A **Horn clause** is a clause with exactly one positive literal
- A **Horn formula** is a conjunctive normal form formula whose clauses are all Horn

## Example
- Prolog

```Prolog
c :- a, b.
a.
b.
```

- Horn formula:

```Prolog
[c, ¬a, ¬b] [a] [b]
```

## Resolution
- Resolution is a single inference rule
- It takes two clauses and produces a new clauses
- The new clauses is implied by the two old clauses
    - The two old clauses need to have complementary literals
	- The new clause contains all the literals of both old clauses except the complementary ones

## Why Horn Clauses?
- Resolution of two Horn clauses always results in a Horn clause
- Resolution of a goal clause and a definite clause is always a goal clause
- Horn clauses have better computational properties than normal clauses
- Prolog is based on computing with Horn clauses

## Alfred Horn
- The name *Horn clause* comes from **Alfred Horn**, who discovered the significance of such clauses
