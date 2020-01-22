# Combinations
$$\binom{n}{k} = \frac{n(n-1)(n-2) \dots (n-k-1)}{k!} = \frac{n!}{(n-k)!k!}$$

# Conditional Probability
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

# Independent Events
$$P(E \cap F) = P(E)P(F)$$

$$P(E \mid F) = P(E)$$

## Conditionally Independent
$$P(E \cap F \mid G) = P(E \mid G)P(F \mid G)$$

# Binomial Random Variable
Sum of $i$ successes out of $n$ trials.

$$P(X=i) = \binom{n}{i} p^{i} (1-p)^{n-i}, i=0, 1, \dots, n$$

# Expected Value of Random Variable
$$E[X] = \sum_{i=1}^{n} x_{i} P(X=x_{i})$$

## Variance
$$Var(X) = \sum_{i=1}^{n} (x_{i} - \mu)^{2} P(x_{i})$$

with $\mu = E[X]$

# Covariance
Say $E[X]=\mu_{x}$ and $E[Y]=\mu_{y}$ then

$$Cov(X, Y) = E[(X-\mu_{x})(Y-\mu_{y})] = E[XY]-E[X]E[Y]$$

# Correlation
$$Corr(X, Y) = \frac{Cov(X, Y)}{\sqrt{Var(X)Var(Y)}}$$

# Inequalities
## Markov
$$P(X \geq a) \leq \frac{E(X)}{a} \text{ for all } a > 0$$

## Chebyshev
$$P( \mid X - \mu \mid \geq k) \leq \frac{\sigma^{2}}{k^{2}} \text{ for all } k > 0$$

## Chernoff
$$P(X \geq a) \leq \min_{t>0} e^{ta} e^{\log{E}(e^{tX})}$$

### Binomial RV
$$P(X \geq (1-\delta)np) \leq e^{-np((1+\delta)\log{(1+\delta)}-\delta)}$$
$$P(X \geq (1-\delta)\mu) \leq e^{-\mu((1+\delta)\log{(1+\delta)}-\delta)}$$

# Distribution of Sample Mean
$$\bar{X} = \frac{1}{N} \sum_{k=1}^{N} X_{k}$$

## Expected Value
$$E[\bar{X}] = \frac{1}{N} \sum_{k=1}^{N} E[X_{k}]$$

## Variance
$$Var(\bar{X}) = \frac{\sigma^{2}}{N}$$

# Weak Law of Large Numbers
$$P(\mid \bar{X} - \mu \mid \geq \epsilon ) \rightarrow 0 \text{ as } N \rightarrow \infty$$

By Chebyshev's inequality: $$P(\mid \bar{X} - \mu \mid \geq \epsilon) \leq \frac{\sigma^{2}}{N \epsilon^{2}}$$

# Continuous Random Variables
## Cumulative and Probability Distribution Function
CDF is $F_{Y}(y)$, PDF is $f_{Y}(y)$

$$P(a < Y \leq b) = F_{Y}(b)-F_{Y}(a)$$
$$F_{Y}(y) = \int_{-\infty}^{y} f_{Y}(t)dt$$
$$P(a < Y \leq b) = \int^{b}_{a} f_{Y} (t)dt$$

### Independent CDF
$$P(X \leq x \land Y \leq y) = P(X \leq x) P(Y \leq y)$$ i.e. $$F_{XY}(x, y)=F_{X}(x) F_{Y}(y)$$
$$F_{XY}(x, y)= \int_{-\infty}^{x} \int_{-\infty}^{y} f_{XY} (u, v) du\: dv$$

### Conditional PDF
$$f_{X \mid Y}(x \mid y) = \frac{f_{XY}(x, y)}{f_{Y}(y)}$$

### Chain Rule for PDF
$$f_{XY}(x, y) = f_{X \mid Y}(x \mid y)f_{Y}(y) = f_{Y \mid X}(y \mid x)f_{X}(x)$$

### Marginalisation of PDFs
$$f_{X}(x) = \int_{-\infty}^{\infty} f_{XY}(x, y)dy$$

### Bayes Rule for PDFs
$$f_{Y \mid X}(y \mid x) = \frac{f_{X \mid Y}(x \mid y) f_{Y}(y)}{f_{X}(x)}$$

# Normal Distribution
$Y \sim N(\mu, \sigma^{2})$ when it has PDF $$f_{Y}(y) = \frac{1}{\sigma\sqrt{(2\pi)}}e^{-\frac{(y-\mu)^{2}}{2\sigma^{2}}}$$

## Central Limit Theorem
$$\bar{X} \sim N(\mu, \frac{\sigma^{2}}{N}) \text{ as } N \rightarrow \infty$$

# Linear Regression Model
$$Y = \sum_{i=1}^{m} \Theta^{(i)}x^{(i)}+M$$ where $\vec{\Theta}$ is a vector of unknown (random) parameters and $M$ is random noise

- $M$ is Gaussian with mean 0 and variance 1, $M \sim N(0, 1)$
- $\Theta^{(i)}$ is Gaussian with mean 0 and variance $\lambda$ (where $\lambda$ is known), $\Theta^{(i)} \sim N(0, \lambda)$

This is equivalent to $$f_{Y \mid X, \vec{\Theta}}(y \mid x, \vec{\theta}) = \frac{1}{\sqrt{2 \pi}} \exp (-(y-\sum_{i=1}^{m} \theta^{(i)}x^{(i)})^{2}/2)$$ given $\vec{\Theta}=\vec{\theta}$. Model also assumes $\Theta^{(i)} \sim N(0, \lambda)$ i.e. $$f_{\Theta^{(i)}}(\theta) \propto \exp (-\theta^{2}/2\lambda)$$

## Parameter Estimation
$$f_{\Theta \mid D}(\vec{\theta} \mid d) = \frac{f_{D \mid \Theta}(d \mid \vec{\theta}) f_{\Theta}(\vec{\theta})}{f_{D}(d)}$$

### Maximum Likelihood Estimation
$$\log f_{D \mid \Theta}(d \mid \vec{\theta}) \propto \log L(\theta) = -\frac{1}{2} \sum_{j=1}^{n}(y_{j} - \sum_{i=1}^{m} \theta^{(i)} x_{j}^{(i)})^{2}$$
$$\theta = \frac{\sum_{j=1}^{n} y_{j}x_{j}}{\sum_{j=1}^{n} x_{j}^{2}}$$

### Maximum a Posteriori (MAP) Estimation
$$\theta = \frac{\sum^{n}_{j=1} y_{j} x_{j}}{\frac{1}{\lambda}+\sum_{j=1}^{n} x_{j}^{2}}$$

# Logistic Regression Model
$$P(Y=1 \mid \Theta=\vec{\theta}, \vec{X} = \vec{x}) = \frac{1}{1+\exp (-z)}, z=\sum_{i=1}^{m} \theta^{(i)}x^{(i)}$$
$$P(Y=0 \mid \Theta=\vec{\theta}, \vec{X} = \vec{x}) = \frac{\exp (-z)}{1+\exp (-z)}$$

## Parameter Estimation
$$P(D=d \mid \Theta = \vec{\theta}) = \prod_{k=1}^{n} (\frac{1}{1+\exp (- z_{k})})^{y_{k}} (\frac{\exp (-z_{k})}{1+\exp(-z_{k})})^{1-y_{k}}$$ where $z_{k} = \sum_{i=1}^{m} \theta^{(i)} x_{k}^{(i)}$

## Maximum Likelihood Estimation
$$P(Y=1 \mid \Theta=\infty, \vec{X} = \vec{x}) = \frac{1}{1+\exp (-z)}, z=\theta^{(1)}x^{(1)} \begin{cases} 1 & x^{(1)} = -1 \\ 0 & x^{(1)} = 0 \end{cases}$$
$$P(Y=0 \mid \Theta=\infty, \vec{X} = \vec{x}) = 1-P(Y=1 \mid \Theta=\infty, \vec{X} = \vec{x})$$
