# Notation
- $\mathbb{Z}$ is the integers
- $\mathbb{R}$ is the real numbers
- {...} is a set
- A $\subset$ B means set A is a subset of set B
- A $\in$ B means A in a member of set B
- $\emptyset$ is the empty set
- |A| is the number of elements in set A
- | means "such that"
- P(E) means that the probability of event E, although Prob(E), P(E) can be used

# Sample Spaces
Same space S: the set of all possible outcomes of an experiment

- Coin flip, $\{Head, Tails\}$
- Roll of a die, $\{1, 2, 3, 4, 5, 6\}$
- Number of emails in a database, $\{z \mid z \in \mathbb{Z}, z \geq 0\}$

# Events
Event $E$: a subset of sample spaces $S$, $E \subset S$. A set of possible outcomes when an experiment is performed

- Coin comes up heads, $\{Heads\}$
- Die roll is less than 3, $\{1, 2\}$

## Set Operations
- $E \cup R = F \cup E$
- $(E \cup F) \cup G = E \cup (F \cup G)$
- $E \cap (F \cup G) = (E \cap F) \cup (E \cap G)$
- $E \cup (F \cap G) = (E \cup F) \cap (E \cup G)$

## Axioms for Events
- If E and F are events then so are:
    - $E \cup F$
	- $E \cap F$
	- $E^{C}$ and $F^{C}$
- Consequently
    - For events $E_{i}, i=1, 2, ..., n$ then
    	- $E_{1} \cup E_{2}$ is an event
		- $(E_{1} \cup E_{2}) \cup E_{3} = E_{1} \cup E_{2} \cup E_{3}$ is an event
		- $\cup_{i=1}^{n} E_{i}$ is an event
		- $\cap_{i=1}^{n} E_{i}$ is an event
	- $S$ is an event since $S = E \cup E^{C}$ for any event E
	- The empty set $\emptyset$ is an event since $S^{C} = \emptyset$
	- Axioms really needs for sets with infinite numbers of elements. A technicality but we'll confine ourselves to intersections, unions and complements when talking about events

# Axioms of Probability
$$P(E) = \lim_{n \rightarrow \infty} \frac{n(E)}{n}$$ where $n(E)$ is the number of times event $E$ occurs in $n$ trials

What basic properties does this quanitity always have

- Axiom 1: $0 \leq P(E) \leq Q$
- Axiom 2: $P(S) = 1$, where S is sample space
- Axiom 3: If E and F are mutually exclusive, $(E \cap F = \emptyset)$ then $P(E \cup F) = P(E) + P(F)$

## Implications
$P(E^{C}) = 1 - P(E)$

$E \subset F$ implies that $P(E) \leq P(F)$

$P(E \cup F) = P(E) + P(F) - P(E \cap F)$

# Equally Likely Outcomes
In some experiments all outcomes are equally likely, e.g. tossing a fair coin

- $P(S) = P(\{Heads, Tails\}) = 1$
- $P(\{Heads, Tails\}) = P(\{Heads\}) + P(\{Tails\}) = 2p = 1$
- $p = \frac{1}{2}$

## Rolling Two Dice
Roll two 6-sides dice. What is the probability that the dice sum to 7.

- $S = \{(1, 1), (1, 2), \dots, (6, 5), (6, 6)\}$
- $E = \{(1, 6), (2, 5), (3, 4), (4, 3), (5, 2), (6, 1)\}$
- $P(E) = \frac{6}{36} = \frac{1}{6}$

## Drawing Balls from a Bag
Have a bag containing 4 red balls and 3 white balls. Draw three balls.

What is the probability of drawing 1 red ball and 2 white balls.

- $\binom{7}{3} = 35$ ways. $\mid S \mid = 35$
- $E = \binom{4}{1} \binom{3}{2}=12$
- $P(\text{1 red ball and 2 white balls}) = \frac{12}{35}$

## Important Trick
Often its hard to count the numbers of times an event E occurs, but easy to count the number of times event E does **not** occur.

Use $P(E) = 1-P(E^{C})$, where $E^{C}$ is the event that E does not occur.

We flip a coin 3 times. What is the probability that there is at least one heads?

- $\mid S \mid = 2^{3} = 8$
- $E^{C} = \{T, T, T\}, P(E^{C}) = \frac{1}{8}$
- $P(E) = 1 - P(E^{C}) = 1- \frac{1}{8}$
