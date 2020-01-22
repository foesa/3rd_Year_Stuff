# Conditional Probability
## Marginalisation
$P(E \cap F_{i}) = P(F_{i} \mid E)P(E)$ so,

$P(E \cap F_{1}) + P(E \cap F_{2}) + \dots P(E \cap F_{n})$

$= P(F_{1} \mid E)P(E) + P(F_{2} \mid E)P(E) + \dots P(F_{n} \mid E)P(E)$

$= (P(F_{1} \mid E) + P(F_{2} \mid E) + \dots P(F_{n} \mid E))P(E)$

$= P(E)$

since $P(F_{1} \mid E) + P(F_{2} \mid E) + \dots P(F_{n} \mid E) = P(S \mid E) = 1$

# Random Variables
## Marginalisation
$P(X = x \text{ and } Y=y_{i}) = P(Y = y_{i} \mid X=x)P(X=x)$ so,

$\sum_{i=1}^{m} P(X=x \text{ and } Y=y_{i}) = \sum_{i=1}^{m} P(Y=y_{i} \mid X=x) P(X=x)$

$= P(X=x) \sum_{i=1}^{m} P(Y=y_{i} \mid X=x)$

$= P(X=x)$

since $\sum_{i=1}^{m} P(Y=y_{i} \mid X=x) = 1$

# Expected Value
## Linearity
Random variable $X$ takes values $x_{1}, x_{2}, \dots, x_{n}$ so,

$E[aX+b] = \sum_{i=1}^{n}(ax_{i}+b)P(X=x_{i})$

$= \sum_{i=1}^{n} ax_{i}P(X=x_{i}) + \sum_{i=1}^{n} bP(X=x_{i})$

$= a \sum_{i=1}^{n} x_{i}P(X=x_{i}) + b \sum_{i=1}^{n} P(X=x_{i})$

$= aE[X]+b$

## Two Random Variables
$E[aX+bY] = \sum_{x} \sum_{y} (ax+by)P(X=x \cap Y=y)$

$= a \sum_{x} \sum_{y} xP(X=x \cap Y=y) + b \sum_{x} \sum_{y} yP(X=x \cap Y=y)$

$= a \sum_{x} xP(X=x) + b \sum_{y} yP(Y=y)$

$= aE[X]+bE[Y]$

since $\sum_{y} P(X=x \cap Y=y) = P(X)$

## Independent Random Variables
$E[XY] = \sum_{x} \sum_{y} xy P(X=x \text{ and } Y=y)$

$= \sum_{x} \sum_{y} xy P(X=x)P(Y=y)$

$= \sum_{x} xP(X=x) \sum_{y} yP(Y=y)$

$=E[X]E[Y]$

# Variance
$Var(X) = \sum_{i=1}^{n} (x_{i} - \mu)^{2} P(x_{i})$

$= \sum_{i=1}^{n} (x_{i}^{2} - 2 \mu x_{i} + \mu^{2}) P(x_{i})$

$= \sum_{i=1}^{n} x_{i}^{2} P(x_{i}) - 2 \sum_{i=1}^{n} x_{i} P(x_{i}) \mu + \mu^{2} \sum_{i=1}^{n} P(x_{i})$

$= E[X^{2}] - 2 \mu^{2} + \mu^{2}$

$= E[X^{2}] - (E[X])^2$

## Non Linearity
$Var(aX+b) = E[(aX+b)^{2}]-E[aX+b]^2$

$=E[a^{2} X^{2} + 2abX + b^{2}] - (aE[X]+b)^{2}$

$= a^{2} E[X^{2}] +2abE[X] + b^{2} - a^{2} E[X]^{2} - 2abE[X] - b^{2}$

$= a^{2} E[X^{2}] - a^{2} E[X]^{2}$

$= a^{2} (E[X^{2}]-E[X]^{2})$

$= a^{2} Var(X)$

## Indepedent Random Variables
$Var(X+Y) = E[(X+Y)^{2}] - E[X+Y]^{2}$

$=E[X^{2} + 2XY +Y^{2}] - (E[X]+E[Y])^{2}$

$=E[X^{2}] + 2E[XY] + E[Y^{2}] - E[X]^{2} - 2E[X]E[Y] - E[Y]^{2}$

$=E[X^{2}] - E[X]^{2} + E[Y^{2}] - E[Y]^{2} + 2E[XY] - 2E[X]E[Y]$

$=E[X^{2}] - E[X]^{2} + E[Y^{2}] - E[Y]^{2}$

$= Var(X)+Var(Y)$

# Covariance
$Cov(X, Y) = E[(X-\mu_{x})(Y-\mu_{y})]$

$Cov(X, Y) = E[XY-X \mu_{y} - Y \mu_{x} + \mu_{x} \mu_{y}]$

$= E[XY] - E[X]\mu_{y} - E[Y] \mu_{x} + \mu_{x} \mu_{y}$

$= E[XY] - \mu_{x} \mu_{y} - \mu_{x} \mu_{y} + \mu_{x} \mu_{y}$

$= E[XY] - \mu_{x} \mu_{y}$

$= E[XY] - E[X]E[Y]$

# Inequalities
## Markov
Let indicator $I_{a}(X) = 1$ if $X \geq a$ and $I_{a}(X)=0$. Then $aI_{a}(X) \leq X$, i.e. $I_{a}(X) \leq \frac{X}{a}$

$E(I_{a}(X)) \leq E(\frac{X}{a}) = \frac{E(X)}{a}$

$E(I_{a}(X)) = P(X \geq a) \leq \frac{E(X)}{a}$

## Chebyshev
Since $(X - \mu)^{2}$ is a non-negative random variable we can apply Markov's inequality with $a=k^{2}$ to get

$P((X - \mu)^{2} \geq k^{2}) \leq \frac{E((X- \mu )^{2})}{k^{2}} = \frac{\sigma^{2}}{k^{2}}$

Note that $(X - \mu)^{2} \geq k^{2} \Leftrightarrow \mid X - \mu \mid \geq k$, so

$P( \mid X - \mu \mid \geq k) \leq \frac{\sigma^{2}}{k^{2}}$

## Chernoff
$P(X \geq a) = P(e^{tX} \geq e^{ta}) \text{ for } t > 0$

By Markov's inequality:

$P(X \geq a) = P(e^{tX} \geq e^{ta}) \leq \frac{E(e^{tX})}{e^{ta}} = e^{-ta} E(e^{tX})$

This holds for all $t>0$, so might as well choose the one that minimises it.

# Weak Law of Large Numbers
$E(\bar{X}) = E(\frac{1}{N} \sum_{k=1}^{N} X_{k}) = \frac{1}{N} \sum_{k=1}^{N} E(X_{k}) = \mu$

$var(\bar{X}) = var(\frac{1}{N} \sum_{k=1}^{N} X_{k}) = \frac{N \sigma^{2}}{N^{2}} = \frac{\sigma^{2}}{N}$
