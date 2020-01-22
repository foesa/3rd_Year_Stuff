# Independence
In English, two events $E$ and $F$ are independent if the order in which they occur doesn't matter. Alternatively, if observed one doesn't affect the other.

- Event $E$ survive parachute jump, event $F$ is event that put a parachute on. We expect the order to matter: jumping and then putting parachute on (event $E$ then $F$) is not the same as putting parachute on and jumping ($F$ then $E$)
- Draw a ball from a bag with green balls and orange balls. Then draw another. For second ball there are fewer balls left in big (since have taken one out), so expect chance of drawing a green ball to have changed.
- Toss a coin twice. We expect that the outcome of the second toss does not depend on the outcome of the first.

Two events $E$ and $F$ are **independent** if $P(E \cap F) = P(E)P(F)$

When events $E$ and $F$ are independent then $P(E \mid F) = P(E)$ (recall chain rule: $P(E \cap F) = P(E \mid F)P(F)$). Note: $P(E \mid F) = P(E)$ is **not** used as the definition, however.

Otherwise, $E$ and $F$ are **dependent** events.

Examples:

- Pick a random LC student - are the events "applied to TCD" and "applied to UCD" independent?
    - Probably not, if you apply to one you're more likely to apply to the other.
- Pick a random person in Ireland - are the events "applied to TCD" and "has brown eyes" independent?
    - Probably yes, colour of eyes probably not related to whether you're at TCD or not

Three events $E$, $F$ and $G$ are independent if they are pairwise independent *and* triply independent.

- $P(E \cap F \cap G) = P(E)P(F)P(G)$
- $P(E \cap F) = P(E)P(F)$
- $P(E \cap G) = P(E)P(G)$
- $P(G \cap F) = P(G)P(F)$

Are three events independent if they are pairwise independent?

- For balls in an urn numbers 110, 101, 011, 000
- Let $A_{k}$ be the event of a 1 in the kth place
- $P(A_{k}) = \frac{1}{2}, P(A_{i} \cap A_{k}) = \frac{1}{4}, P(A_{1} \cap A_{2} \cap A_{3}) = 0$

# Conditional Independence
Say we roll two 6-sided dice

- $S = \{(1, 1), (1, 2), (1, 3), \dots , (6, 1), (6, 2), \dots \}$ (36 possibilities)
- $E$ is the event that the first dice comes up 1
- $F$ is the event that the second dice comes up 6
- So $E \cap F$ is the event that the first dice is 1 and the second 6
- $G$ is the event that the dice sum to 7

Clearly E and F are independent: $P(E \cap F) = P(E)P(F) = \frac{1}{6} \times \frac{1}{6} = \frac{1}{36}$

- Now suppose that we have observed event $G$. What are the probabilities of events $E$, $F$ and $E \cap F$ now?
- $S \cap G = \{ (1, 6), (2, 5), (3, 4), (4, 3), (5, 2), (6, 1) \}$
- $E \cap G = \{ (1, 6) \}$, $F \cap G = \{ (1, 6) \}$
- $P(E \mid G) = \frac{1}{6}$, $P(F \mid G) = \frac{1}{6}$
- $P(E \cap F \mid G) = \frac{1}{6} \neq P(E \mid G)P(F \mid G) \rightarrow$ dependent

**Key takeaway**: Independent events can become dependent when we condition on additional information. Also dependent events can become independent.

Two events E and F are called **conditionally indepedentent given G** if $$P(E \cap F \mid G) = P(E \mid G)P(F \mid G)$$

It follows that $P(E \mid F \cap G) = P(E \mid G)$ (apply Bayes rule $P(E \mid F \cap G) = P(E \cap F \mid G)/P(F \mid G)$)

- In English, event after observing event G the events E and F still do not depend on one another
- If E and F are independent, does it follow that $P(E \cap F \mid G) = P(E \mid G)P(F \mid G)$? No.

# Breaking Dependence
Take the following three events:

- Sample space S = {days of week}
- A is that is is not a Monday, $P(A) = \frac{6}{7}$
- B is that is is a Saturday, $P(B) = \frac{1}{7}$
- C is that it is the weekend

Note that A and B are dependent events $(P(A \cap B) = \frac{1}{7} \neq P(A)P(B)$. What happens when we condition on C?

- $P(A \mid C) = 1, P(B \mid C) = \frac{1}{2}$
- $P(A \cap B \mid C) = \frac{1}{2} = P(A \mid C)P(B \mid C)$
- Dependent events can become independent by conditioning on additional information
