# Cumulative Distribution Functions
Suppose $X$ and $Y$ are two random variables

- $F_{XY}(x, y) = P(X \leq x \text{ and } Y \leq y)$ is the cumulative distribution function for $X$ and $Y$
- When $X$ and $Y$ are independent then $$P(X \leq x \text{ and } Y \leq y) = P(X \leq x)P(Y \leq y)$$ i.e. $$F_{XY}(x, y) = F_{X}(x)F_{Y}(y)$$

When $X$ and $Y$ are discrete random variable taking values $\{x_{1}, \dots, x_{n}\}$ and $\{y_{1}, \dots, y_{m}\}$

- $F_{XY}(x, y) = \sum_{i: x_{i} \leq x} \sum_{j: y_{j} \leq y} P(X=x_{i} \text{ and } Y=y_{j})$

When $X$ and $Y$ are jointly continuous-valued random variables there exists a probability density function (PDF) $f_{XY} (x, y) \geq 0$ such that $$F_{XY}(x, y) \int_{\infty}^{x} \int_{-\infty}^{y} f_{XY} (u, v) du\: dv$$

Can think of $P(u \leq X \leq u + du \text{ and } v \leq Y \leq v+dv) \approx f_{XY}(u, v)du\: dv$ when $du, dv$ are infinitesimally small.

# Conditional Probability Density Function
Suppose $X$ and $Y$ are two continuous random variables with joint PDF $f_{XY}(x, y)$. Define conditional PDF: $$f_{X \sim Y}(x \sim y) = \frac{f_{XY}(x, y)}{f_{Y}(y)}$$

Compare with conditional probability for discrete RVs: $$P(X=x \mid Y=y) = \frac{P(X=x \text{ and } Y=y}{P(Y=y)}$$

## Chain Rule for PDFs
Since $$f_{X \mid Y}(x \mid y) = \frac{f_{XY}(x, y)}{f_{Y}(y)}$$ the chain rule also holds for PDFs: $$f_{XY} (x, y) = f_{X \mid Y}(x \mid y) f_{Y}(y) = f_{Y \mid X}(y \mid x) f_{X}(x)$$

Also

- $\int_{-\infty}^{\infty} f_{X \mid Y}(x \mid y) dx = \frac{\int_{-\infty}^{\infty}f_{XY}(x, y)dx}{f_{Y}(y)} = \frac{f_{Y}(y)}{f_{Y}(y)} = 1$
- We can marginalise PDFs: $$\int_{-\infty}^{\infty} f_{XY}(x, y)dy = \int_{-\infty}^{\infty} f_{Y \mid X} (y \mid x) f_{X}(x) dy = d_{X}(x) \int_{-\infty}^{\infty} f_{Y \mid X}(y \mid x)dy = f_{X}(x)$$

## Bayes Rule for PDFs
Since $$f_{X \mid Y} (x \mid y) = \frac{f_{XY}(x, y)}{f_{Y}(y)}$$ then $$f_{X \mid Y} (x \mid y) f_{Y}(y) = f_{XY}(x, y) = f_{Y \mid X}(y \mid x) f_{X}(x)$$ and so we have Bayes Rule for PDFs: $$f_{Y \mid X}(Y \mid X) = \frac{f_{X \mid Y}(x \mid y) f_{Y}(y)}{f_{X}(x)}$$

## Independence
Suppose $X$ and $Y$ are two continuous random variables with joint PDF $f_{XY}(x, y)$. Then $X$ and $Y$ are independent when $$f_{XY}(x, y) = f_{X}(x)f_{Y}(y)$$

Why? $$P(X \leq x \text{ and } Y \leq y) = \int_{-\infty}^{x} \int_{-\infty}^{y} f_{XY}(u, v)dudv = \int_{-\infty}^{x} f_{X}(u)du \int_{-\infty}^{y} f_{Y}(v)dv = P(X \leq x) P(Y \leq y)$$
