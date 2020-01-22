# Conditional Probability
**Conditional probability** is the probability that event $E$ occurs *given* that event $F$ has already occured. Call this "conditioning" on $F$. Writen as $P(E \mid F)$.

- Same meaning as $P(E \text{ given } F \text{ already observed})$
- Its a probability (satisfies all the axioms, will see this shortly), with:
    - Sample space $S$ resticted to those outcomes consistent with $F$, i.e. $S \cap F$
	- Event space $E$ restricted to those outcomes consistent with $F$, i.e. $E \cap F$

With equally likely outcomes

$P(E \mid F) = \frac{\text{number of outcomes in } E \text{ consistent with } F}{\text{number of outcomes in } S \text {consistent with } F} = \frac{\mid E \cap F \mid}{\mid S \cap F \mid}$

Note that $\mid S \cap F \mid = \mid F \mid$ so

$P(E \mid F) = \frac{\mid E \cap F \mid}{\mid S \cap F \mid} = \frac{\mid E \cap F \mid}{\mid F \mid} = \frac{\mid E \cap F \mid}{\mid S \mid} \frac{\mid S \mid}{\mid F \mid} = \frac{\frac{\mid E \cap F \mid}{\mid S \mid}}{\frac{\mid F \mid}{\mid S \mid}} = \frac{P(E \cap F)}{P(F)}$

General definition: $P(E \mid F) = \frac{P(E \cap F)}{P(F)}$ where $P(F) > 0$.

Implies $P(E \cap F) = P(E \mid F)P(F)$ known as the *chain rule* - it's important!

If $P(F) = 0$?

- $P(E \mid F)$ is undefined
- Can't condition on something that can't happened

P(E|F) is a probability - it satisfies all the properties of ordinary probabilities

- $0 \leq P(E \mid F) \leq 1$
- $P(S \mid F) = 1$
- If E~1~, E~2~ are mutually exclusive events then $P(E_{1} \cup E_{2} \mid F) = P(E_{1} \mid F) + P(E_{2} \mid F)$

# Marginalisation
Suppose we have mutually exclusive events F~1~, F~2~, ..., F~n~ such that $F_{1} \cup F_{2} \cup \dots \cup F_{n} = S$ then $$P(E) = P(E \cap F_{1}) + P(E \cap F_{2}) + \dots + P(E \cap F_{n})$$

Marginalisation is very handy, example:

- Roll two coins. What is the probability that the first coin is heads?
- Event E is the first coin heads, F~1~ is second coin heads, F~2~ is second coin tails
- $P(E) = P(E \cap F_{1}) + P(E \cap F_{2}) = \frac{1}{2} \times \frac{1}{2} + \frac{1}{2} \times \frac{1}{2}= \frac{1}{2}$

# Bayes Rule
Recall $P(E \cap F) = P(E \mid F)P(F)$

Clearly, and also $P(F \cap E) = P(F \mid E)P(E)$

But $P(E \cap F) = P(F \cap E)$, so $P(E \mid F)P(F) = P(F \mid E)P(E)$

i.e. $P(E \mid F) = \frac{P(F \mid E)P(E)}{P(F)}$
