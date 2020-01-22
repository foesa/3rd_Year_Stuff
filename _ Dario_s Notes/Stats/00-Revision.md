# Counting & Random Events
Bags of red & black balls

- With replacement
- Without replacement

Bag with 10 balls - how many ways can we take out 2 balls?

1. With replacement - $10 \times 10 = 100$
2. Without replacement - $10 \times 9 = 90$

3 red & 7 black balls - how many way can we take out:

- 1 red then 1 black
    1. With replacement - $\frac{3 \times 7}{10 \times 10}$
    2. Without replacement - $\frac{3 \times 7}{10 \times 9}$
- 2 reds
    1. With replacement - $\frac{3 \times 3}{10 \times 10}$
    2. Without replacement - $\frac{3 \times 2}{10 \times 9}$
- 2 red & 3 black
    1. With replacement - $\frac{(3 \times 3)(7 \times 7 \times 7)}{10^{5}} = (\frac{3}{10})^{2}(\frac{7}{10})^{3}$
    2. Without replacement - $\frac{(3 \times 2)(7 \times 6 \times 5)}{10 \times 9 \times 8 \times 7 \times 6} \times \frac{5!}{2!3!}$

# Definitions
- Sample space
- Random event
- Random variable - Mapping from $S \rightarrow \mathbb{R}$

## Conditional Probability
$P(E \mid F) = \text{the probability of } E \text{ given } F \text{ has already happened}$
$$P(E \mid F) = \frac{P(E \cap F)}{P(F)}$$

## Chain Rule
$$P(E \cap F) = P(E \mid F)P(F)$$

## Marginalisation
$$P(E) = P(E \cap F_{1}) + P(E \cap F_{2}) + \dots + P(E \cap F_{n})$$
$$P(E) = P(E \mid F_{1})P(F_{1}) + P(E \mid F_{2})P(F_{2}) + \dots + P(E \mid F_{n})P(F_{n})$$
given

- $F_{1}, F_{2}, \dots, F_{n}$ are mutually exclusive
- $F_{1} \cup F_{2} \cup \dots \cup F_{n} = S$

## Bayes Rule
$$P(E \mid F) = \frac{P(F \mid E)P(E)}{P(F)}$$

## Independence
$$P(E \cap F) = P(E)P(F)$$
$$P(E \mid F) = P(E)$$

### Example Question
$X \in \{1, 2, 3\}, Y \in \{1, 2, 3\}, V = X+Y \in \{2, 3, 4, 5, 6\}$

Are $X$ & $V$ Independent?

$P(V=2 \land X=2) = 0$

$P(V=2) = (\frac{1}{3})^{2}$

$P(X=2) = \frac{1}{3}$

$P(V=2) \times P(X=2) \neq P(V=2 \land X=2)$

## Expected Value
$$E[X] = \sum_{i=1}^{n} x_{i} P(X=x_{i})$$

### Linearity
Random variable $X$ takes values $x_{1}, x_{2}, \dots, x_{n}$ so,

$E[aX+b] = \sum_{i=1}^{n}(ax_{i}+b)P(X=x_{i})$

$= \sum_{i=1}^{n} ax_{i}P(X=x_{i}) + \sum_{i=1}^{n} bP(X=x_{i})$

$= a \sum_{i=1}^{n} x_{i}P(X=x_{i}) + b \sum_{i=1}^{n} P(X=x_{i})$

$= aE[X]+b$

### Two Random Variables
$E[aX+bY] = \sum_{x} \sum_{y} (ax+by)P(X=x \cap Y=y)$

$= a \sum_{x} \sum_{y} xP(X=x \cap Y=y) + b \sum_{x} \sum_{y} yP(X=x \cap Y=y)$

$= a \sum_{x} xP(X=x) + b \sum_{y} yP(Y=y)$

$= aE[X]+bE[Y]$

since $\sum_{y} P(X=x \cap Y=y) = P(X)$

### Independent Random Variables
$E[XY] = \sum_{x} \sum_{y} xy P(X=x \text{ and } Y=y)$

$= \sum_{x} \sum_{y} xy P(X=x)P(Y=y)$

$= \sum_{x} xP(X=x) \sum_{y} yP(Y=y)$

$=E[X]E[Y]$

## Variance
$$Var(X) = \sum_{i=1}^{n} (x_{i} - \mu)^{2} P(x_{i})$$

with $\mu = E[X]$

### Non Linearity
$Var(aX+b) = E[(aX+b)^{2}]-E[aX+b]^2$

$=E[a^{2} X^{2} + 2abX + b^{2}] - (aE[X]+b)^{2}$

$= a^{2} E[X^{2}] +2abE[X] + b^{2} - a^{2} E[X]^{2} - 2abE[X] - b^{2}$

$= a^{2} E[X^{2}] - a^{2} E[X]^{2}$

$= a^{2} (E[X^{2}]-E[X]^{2})$

$= a^{2} Var(X)$

### Indepedent Random Variables
$Var(X+Y) = E[(X+Y)^{2}] - E[X+Y]^{2}$

$=E[X^{2} + 2XY +Y^{2}] - (E[X]+E[Y])^{2}$

$=E[X^{2}] + 2E[XY] + E[Y^{2}] - E[X]^{2} - 2E[X]E[Y] - E[Y]^{2}$

$=E[X^{2}] - E[X]^{2} + E[Y^{2}] - E[Y]^{2} + 2E[XY] - 2E[X]E[Y]$

$=E[X^{2}] - E[X]^{2} + E[Y^{2}] - E[Y]^{2}$

$= Var(X)+Var(Y)$

# Inequalities
- Markov
- Cheyshev
    - Law of large numbers
- Chernoff
    - Frequency interpretation of probability

**Will give us Chebyshev and Chernoff if needed**

## Markov
$$P(X \geq a) \leq \frac{E(X)}{a} \text{ for all } a > 0$$

## Chebyshev
$$P( \mid X - \mu \mid \geq k) \leq \frac{\sigma^{2}}{k^{2}} \text{ for all } k > 0$$

## Chernoff
$$P(X \geq a) \leq \min_{t>0} e^{ta} e^{\log{E}(e^{tX})}$$

# Confidence Intervals
$P(a \leq X \leq b)$

- Inequalities (esp. chernoff & cheybshev)
- Cental limit theorem
- Bootstrapping

## CLT
$$X = \frac{1}{N} \sum_{k=1}^{N} X_{k} \sim N(E(X_{i}), \frac{Var(X_{i})}{n})$$

# Linear Regression Model
$$Y=\sum_{i=1}^{m} \Theta^{(i)} x^{(i)} + M, M \sim N(0, 1), \Theta^{(i)} \sim N(0, \lambda)$$

$$f_{D \mid \Theta} (d \mid \vec{\theta}) \propto L(\theta) = \exp(-\sum_{j=1}^{n}(y_{j}-\sum_{i=1}^{m} \theta^{(i)}x_{j}^{(i)}*{(i)})^{2}/2), f_{\Theta^{(i)}}(\theta) \propto \exp(-\theta^{2} / 2\lambda)$$
