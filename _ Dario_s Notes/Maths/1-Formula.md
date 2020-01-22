# Estimation
## Intermediate Value Theorem
For a continuous function $f(x)$ over the interval $[a,b]$, $\forall M, f(a) \le M \le f(b), \exists\ c$ such that $f(c) = M$.

## Taylor Series
For a function $f(x)$, $n+1$ times differentiable in an interval containing $x_{0}$, the Taylor Series is

$$T(x) = \sum\limits_{i=0}^{n} \dfrac{(x-x_{0})^{i}}{i!} f^{(i)}(x_{0})$$

# Solving Nonlinear Equations
## Bisection Method
1. Choose points $a$ and $b$ with the solution between this domain
2. $x_{ns} = \frac{a+b}{2}$
3. Calculate $f(a)$, $f(b)$ and $f(x_{ns})$
4. Determine if the solution if between $[a, x_{ns}]$ or $[b, x_{ns}]$
5. Go to step two

## Regula Falsi Method
1. Choose points $a$ and $b$ with the solution between this domain
2. $x_{ns} = \frac{af(b) - bf(a)}{f(b)-f(a)}$
3. Calculate $f(a)$, $f(b)$ and $f(x_{ns})$
4. Determine if the solution if between $[a, x_{ns}]$ or $[b, x_{ns}]$
5. Go to step two

## Newton Method
1. Choose $(x_{1}, f(x_{1}))$ as a starting point near the solution
2. $x_{2} = x_{1} - \frac{f(x_{1})}{f'(x_{1})}$
3. Repeat step two with the result

## Secant Method
1. Choose $(x_{1}, f(x_{1})), (x_{2}, f(x_{2}))$ as starting points new the solution
2. $x_{3} = x_{2} - \frac{f(x_{2})(x_{1}-x_{2})}{f(x_{1})-f(x_{2})}$
3. Repeat step two with $(x_{2}, f(x_{2})), (x_{3}, f(x_{3}))$

## Fixed Point Iteration Method
1. Write $x = g(x)$ in the different ways
    - E.g. $xe^{0.5x}+1.2x-5=0$
	- Case a: $x=\frac{5-xe^{0.5x}}{1.2}$
	- Case b: $x=\frac{5}{e^{0.5x}+1.2}$
	- Case c: $x=\frac{5-1.2x}{e^{0.5x}}$
2. Work out $g'(x)$ for the edge values of where the root lies and use the equation where $\mid g'(x) \mid < 1$
3. Use this equation and start with one of the edge values and iterate using each result to the desired accuracy

## Systems of Nonlinear Equations
### Newton Method
1. Choose points $x_{1}, y_{2}$
2. Solve for the Jacobian determinant $$J(f_{1}, f_{2}) = \det \left( \begin{array}{cc}
\frac{\delta f_{1}}{\delta x} & \frac{\delta f_{1}}{\delta y}  \\
\frac{\delta f_{2}}{\delta x} & \frac{\delta f_{2}}{\delta y} \end{array} \right)$$
3. $$\Delta x = \frac{f_{2}(x_{1}, y_{1}) \frac{\delta f_{1}}{\delta y}-f_{1}(x_{1}, y_{1}) \frac{\delta f_{2}}{\delta y}}{J(f_{1}, f_{2})}$$
4. $$\Delta y = \frac{f_{1}(x_{1}, y_{1}) \frac{\delta f_{2}}{\delta x}-f_{2}(x_{1}, y_{1}) \frac{\delta f_{1}}{\delta x}}{J(f_{1}, f_{2})}$$
5. Solve $x_{2} = x_{1} + \Delta x$ and $y_{2} = y_{1} \Delta y$
6. Repeat from step 3

### Fixed Point Iteration Method
1. Choose points $x_{1}, y_{1}$
2. Get $f_{1}$ and $f_{2}$ in terms of $x$ and $y$
3. Solve for $x_{2}, y_{2}$
4. Repeat from step 2

# Systems of Linear Equations
## Gaussian Elimination Method
Given:

$a_{11}x_{1} + a_{12}x_{2} + a_{13}x_{3} + a_{14}x_{4} = b_{1}$

$a_{21}x_{2} + a_{22}x_{2} + a_{23}x_{3} + a_{24}x_{4} = b_{2}$

$a_{31}x_{3} + a_{32}x_{3} + a_{33}x_{3} + a_{34}x_{4} = b_{3}$

$a_{41}x_{4} + a_{42}x_{2} + a_{43}x_{3} + a_{44}x_{4} = b_{4}$

1. Take the first equation (the pivot equation and $a_{11}$ is called the pivot coefficient)
2. Multiply the first equation by $m_{21} = \frac{a_{21}}{a_{11}}$ and subtract this from the second equation (elminitating $x_{1}$)
3. Repeat for each equation with $m_{i1} = \frac{a_{i1}}{a_{11}}$
4. Repeat the process, taking the subsequent equations are the pivot equations.
    - Example: $a_{22}$ becomes the pivot element, removing $x_{2}$ from equations

Result:

$a_{11}x_{1} + a_{12}x_{2} + a_{13}x_{3} + a_{14}x_{4} = b_{1}$

$a'_{22}x_{2} + a'_{23}x_{3} + a'_{24}x_{4} = b'_{2}$

$a'_{33}x_{3} + a'_{34}x_{4} = b'_{3}$

$a'_{44}x_{4} = b'_{4}$

### With Pivoting
Given:

$a_{11}x_{1} + a_{12}x_{2} + a_{13}x_{3} + a_{14}x_{4} = b_{1}$

$0 + 0 + a_{23}x_{3} + a_{24}x_{4} = b_{2}$

$0 + a_{32}x_{3} + a_{33}x_{3} + a_{34}x_{4} = b_{3}$

$0 + a_{42}x_{2} + a_{43}x_{3} + a_{44}x_{4} = b_{4}$

If a pivot element is 0, swap some equations:

$a_{11}x_{1} + a_{12}x_{2} + a_{13}x_{3} + a_{14}x_{4} = b_{1}$

$0 + a_{32}x_{3} + a_{33}x_{3} + a_{34}x_{4} = b_{3}$

$0 + 0 + a_{23}x_{3} + a_{24}x_{4} = b_{2}$

$0 + a_{42}x_{2} + a_{43}x_{3} + a_{44}x_{4} = b_{4}$

## Gauss-Jordan Elimination Method
The same as the Gaussian Elimination Method except

1. The pivot equation is normalised by dividing all the terms in the pivot equation by the pivot coefficient (making the pivot coefficient 1)
2. The pivot equation is used to eliminate the off-diagonal terms in **all** equations

## LU Decomposition Method
1. Calculate first column of $[L]$; $L_{i1}=a_{i1}$
2. Substitute 1s in the diagonal of $[U]$; $U_{ii} = 1$
3. Calculate the elements in the first row of $[U]$ (Except $U_{11}$); $U_{1j} = \frac{a_{1j}}{L_{11}}$
4. Calculate the rest of the elements row after row where $$a_{ij} = \sum_{k=1}^{k=j} L_{ik}U_{kj}$$
    - Example: $a_{33} = L_{31}U_{13}+L_{32}U_{23}+L_{33}U_{33} = L_{31}U{13}+L_{32}U_{23}+L_{33}$ ($U_{ii}=1$)
5. $[L][y]=[b]$ to solve for $y$
6. $[U][x]=[y]$ to solve for $x$

## Norms
$$\left[ \begin{array}{ccc}
a_{11}x_{1} + a_{12}x_{2} + a_{13}x_{3} = b_{1}  \\
a_{21}x_{2} + a_{22}x_{2} + a_{23}x_{3} = b_{2}  \\
a_{31}x_{3} + a_{32}x_{3} + a_{33}x_{3} = b_{3} \end{array} \right]$$

1-Norm: $$\mid \mid a \mid \mid_{1} = max[\mid a_{11} \mid + \mid a_{21} \mid + \mid a_{31} \mid, \mid a_{12} \mid + \mid a_{22} \mid + \mid a_{32} \mid, \mid a_{13} \mid + \mid a_{23} \mid + \mid a_{33} \mid] $$

Infinity Norm: $$\mid \mid a \mid \mid_{\infty} = max[\mid a_{11} \mid + \mid a_{12} \mid + \mid a_{13} \mid, \mid a_{21} \mid + \mid a_{22} \mid + \mid a_{23} \mid, \mid a_{31} \mid + \mid a_{32} \mid + \mid a_{33} \mid] $$

Condition Number: $\mid \mid [a] \mid \mid \times \mid \mid [a]^{-1} \mid \mid$

# Eigenvalues and Eigenvectors
For a given square matrix $[a]$ the number of $\lambda$ is an eigenvalue of the matrix if $[a][u] = \lambda [u]$. The vector $[u]$ is a column vector called the eigenvector associated with the eigenvalue $\lambda$. If should be noted that there are usually more than one eigenvalue and eigenvector. In fact, in an $n \times n$ matrix there are $n$ eigenvalues and an infinite number of eigenvectors.

## Characteristic Equation
$\det[a-\lambda I]=0$ where $[a]$ is the matrix, $I$ is the identity matrix, and $\lambda$ is a polynomial equation whose roots are the eigenvalues. This can be used to solve $[a][u]=\lambda [u]$ for the eigenvectors.

## Basic Power Method
$n \times n$ matrix $[a]$ with $n$ distinct real eigenvalues $\lambda_{1}, \lambda_{2}, \dots, \lambda_{n}$ and $n$ associated eigenvectores $[u]_{1}, [u]_{2}, \dots, [u]_{n}$.

1. Choose a non-zero vector $[x]_{1}$.
2. Multiply by $[a]$
3. Factor out the largest element in the resulting vector to get $[x]_{2}$ and a factor
4. Go back to step one with $[x]_{2}$

$[x]_{\infty} = [u]_{1}$ where $[u]_{1}$ is the eigenvector corresponding to the largest eigenvalue.

## Inverse Power Method
This method is used to find the smallest eigenvalue. Apply the power method using $[a]^{-1}$

## QR Factorisation
Step 1

1. Choose $[c]$ to be the first column of $[a]$
2. The first element in vector $[e]$ is $1$ is the first element of $[c]$ is postivie, otherwise is it negative. The rest is $0$
3. Calculate $[H]^{(1)} = [I] - \frac{2}{[v]^{T}[v]}[v][v]^{T}$ where $[v] = [c] + \mid \mid [c] \mid \mid_{2} [e]$ where $\mid \mid [c] \mid \mid_{2} = \sqrt{c_{1}^{2}+c_{2}^{2}+\dots+c_{n}^{2}}$
4. $[Q]^{(1)}=[H]^{(1)}$ and $[R]^{(1)}=[H]^{(1)}[a]$

Step 2

1. Vector $[c]$ is now defined as the second column of $[R]^{(1)}$
2. The second element in vector $[e]$ is $1$ is the first element of $[c]$ is postivie, otherwise is it negative. The rest is $0$
3. Calculate $H^{(2)}$
4. $[Q]^{(2)} = [Q]^{(1)} [H]^{(2)}$ and $[R]^{(2)} = [R]^{(1)} [H]^{(2)}$

Continue on, taking $[c]$ as the third column of $[R]^{(2)}$, $[Q]^{(3)} = [Q]^{(2)}[H]^{(3)}$ and $[R]^{(3)}=[R]^{(2)} [H]^{(3)}$

# Curve Fitting and Interpolation
## Linear Equation
$$y=a_{1}x+a_{0}$$
$$\text{Error}=\sum_{i=1}^{n} r_{i}^{2} =\sum_{i=1}^{n} (y_{i}-f(x_{i}))^{2} =\sum_{i=1}^{n} (y_{i}-(a_{1}x_{i}+a_{0}))^{2}$$

### Linear Least-Squares Regression:

1. Calculate
    - $S_{x} = \sum_{i=1}^{n} x_{i}$
	- $S_{y} = \sum_{i=1}^{n} y_{i}$
	- $S_{xy} = \sum_{i=1}^{n} x_{i} y_{i}$
	- $S_{xx} = \sum_{i=1}^{n} x_{i}^{2}$
2. $a_{1} = \frac{n S_{xy} - S_{x} S_{y}}{n S_{xx} - (S_{x})^{2}}$
3. $a_{0} = \frac{S_{xx} S_{y} - S_{xy} S_{x}}{n S_{xx} - (S_{x})^{2}}$

## Nonlinear Equation in Linear Form
| Nonlinear Equation | Linear Form |
|---|---|
| $y=bx^{m}$ | $\ln{(y)}=m\ln{(x)}+\ln{(b)}$ |
| $y=be^{mx}$ | $\ln{(y)}=mx+\ln{(b)}$ |
| $y=b10^{mx}$ | $\log{(y)}=mx+\log{(b)}$ |
| $y=\frac{1}{mx+b}$ | $\frac{1}{y}=mx+b$ |
| $y=\frac{mx}{b+x}$ | $\frac{1}{y}=\frac{b}{m}\frac{1}{x}+\frac{1}{m}$ |

Use least-squares regression on a nonlinear equation's linear form.

## Quadratic and Higher-Order Polynomials
If the polynomial, of order $m$, that is used for the curve fitting is: $$f(x)=a_{n}x^{m} + a_{m-1}x^{m-1} + \dots + a_{1}x + a_{0}$$ then, for a given set of $n$ data points, (where $m<n-1$), then total error is given by $$\text{Error}=\sum_{i=1}^{n} (y_{i}-(a_{n}x^{m} + a_{m-1}x^{m-1} + \dots + a_{1}x + a_{0}))^{2}$$

Example of a second order polynomial:

1. $na_{0}+(\sum_{i=1}^{n} x_{i})a_{1} + (\sum_{i=1}^{n} x_{i}^{2})a_{2} = \sum_{i=1}^{n} y_{i}$
2. $(\sum_{i=1}^{n} x_{i}) a_{0} + (\sum_{i=1}^{n} x_{i}^{2}) a_{1} + (\sum_{i=1}^{n} x_{i}^{3}) a_{2} = \sum_{i=1}^{n} x_{i}y_{i}$
3. $(\sum_{i=1}^{n} x_{i}^{2}) a_{0} + (\sum_{i=1}^{n} x_{i}^{3}) a_{1} + (\sum_{i=1}^{n} x_{i}^{4}) a_{2} = \sum_{i=1}^{n} x_{i}^{2}y_{i}$

The solution of the system of equations give the values $a_{0}, a_{1}, a_{2}$

## Single Polynomial
### Lagrange Polynomials
First-order polynomial:

$$f(x) = \frac{x-x_{2}}{x_{1}-x_{2}}y_{1} + \frac{x-x_{1}}{x_{2}-x_{1}}y_{2}$$

Second-order polynomial:

$$f(x) = \frac{(x-x_{2})(x-x_{3})}{(x_{1}-x_{2})(x_{1}-x_{3})}y_{1} + \frac{(x-x_{1})(x-x_{3})}{(x_{2}-x_{1})(x_{2}-x_{3})}y_{2} + \frac{(x-x_{1})(x-x_{2})}{(x_{3}-x_{1})(x_{3}-x_{2})}y_{3}$$

And it goes on...

Sneaky fourth-order polynomial:

$f(x) = \frac{(x-x_{2})(x-x_{3})(x-x_{4})(x-x_{5})}{(x_{1}-x_{2})(x_{1}-x_{3})(x_{1}-x_{4})(x_{1}-x_{5})}y_{1} + \frac{(x-x_{1})(x-x_{3})(x-x_{4})(x-x_{5})}{(x_{2}-x_{1})(x_{2}-x_{3})(x_{2}-x_{4})(x_{2}-x_{5})}y_{2} + \frac{(x-x_{1})(x-x_{2})(x-x_{4})(x-x_{5})}{(x_{3}-x_{1})(x_{3}-x_{2})(x_{3}-x_{4})(x_{3}-x_{5})}y_{3} + \frac{(x-x_{1})(x-x_{2})(x-x_{3})(x-x_{5})}{(x_{4}-x_{1})(x_{4}-x_{2})(x_{4}-x_{3})(x_{4}-x_{5})}y_{4} + \frac{(x-x_{1})(x-x_{2})(x-x_{3})(x-x_{4})}{(x_{5}-x_{1})(x_{5}-x_{2})(x_{5}-x_{3})(x_{5}-x_{4})}y_{5}$

### Newton's Polynomials
First-order polynomial:

$$f(x)=a_{1}+a_{2}(x-x_{1})$$

1. $a_{1}=y_{1}$
2. $a_{2}=\frac{y_{2}-y_{1}}{x_{2}-x_{1}}$

Second-order polynomial:

$$f(x)=a_{1}+a_{2}(x-x_{1})+a_{3}(x-x_{1})(x-x_{2})$$

1. $a_{1}=y_{1}$
2. $a_{2}=\frac{y_{2}-y_{1}}{x_{2}-x_{1}}$
3. $a_{3}=\frac{\frac{y_{3}-y_{2}}{x_{3}-x_{2}}-\frac{y_{2}-y_{1}}{x_{2}-x_{1}}}{x_{3}-x_{1}}$

And it goes on...

## Piecewise Interpolation
### Linear Splines
$$f_{i}(x)=\frac{(x-x_{i+1})}{(x_{i}-x_{i+1})}y_{i}+\frac{(x-x_{i})}{(x_{i+1}-x_{i})}y_{i+1}$$ for $i=1, 2, \dots, n-1$

# Numerical Differentation
## Finite Difference Approximation of the Derivative
$f'(x)$ of a functi(on $f(x)$ at point $x=a$ is defined by $$\left. \frac{df(x)}{dx} \right|_{x=a} = f'(a) = \lim_{x \rightarrow a} \frac{f(x)-f(a)}{x-a}$$

Forward difference: $$\left. \frac{df(x)}{dx} \right|_{x=x_{i}} = \frac{f(x_{i+1})-f(x_{i})}{x_{i+1}-x_{i}}$$

Backward difference: $$\left. \frac{df(x)}{dx} \right|_{x=x_{i}} = \frac{f(x_{i})-f(x_{i-1})}{x_{i}-x_{i-1}}$$

Central difference: $$\left. \frac{df(x)}{dx} \right|_{x=x_{i}} = \frac{f(x_{i+1})-f(x_{i-1})}{x_{i+1}-x_{i-1}}$$

## Finite Difference Formulas using Taylor Series Expansion
Taylor series: $$f(x) = f(a) + \frac{f'(a)}{1!}(x-a) + \frac{f''(a)}{2!}(x-a)^{2} + \frac{f'''(a)}{3!}(x-a)^{3} + \dots$$

Truncation error: $O(h)$

### Derivation
**Two point forward difference formula for first derivative**

The value of a function at point $x_{i+1}$ can be approximated by a Taylor series in terms of the value of the function and its derivates at point $x_{i}$ $$f(x_{i+1}) = f(x_{i}) + f'(x_{i})(x_{i+1}-x_{i}) + \frac{f''(x_{i})}{2!}(x_{i+1}-x_{i})^{2}+\frac{f'''(x_{i})}{3!}(x_{i+1}-x_{i})^{3}+\dots$$

By using two-term Taylor series expansion with a remainder, this can be written as $$f(x_{i+1}) = f(x_{i}) + f'(x_{i})(x_{i+1}-x_{i}) + \frac{f''(\xi)}{2!}(x_{i+1}-x_{i})^{2}$$ where $\xi$ is a value between $x_{i}$ and $x_{i+1}$. This can be solved for $f'(x_{i})$ giving $$f'(x_{i}) = \frac{f(x_{i+1})-f(x_{i})}{h}-\frac{f''(\xi)}{2!}h = \frac{f(x_{i+1})-f(x_{i})}{h}+O(h)$$ where $h=x_{i+1}-x_{i}$

**Two point backward difference formula for first derivative**

Likewise, the function at point $x_{i-1}$ is approximated by a Taylor series in terms of the value of the function and its derivates at point $x_{i}$ $$f(x_{i-1}) = f(x_{i}) - f'(x_{i})(x_{i}-x_{i-1}) + \frac{f''(x_{i})}{2!}(x_{i}-x_{i-1})^{2}-\frac{f'''(x_{i})}{3!}(x_{i}-x_{i-1})^{3}+\dots$$ $$f(x_{i-1}) = f(x_{i}) - f'(x_{i})(x_{i}-x_{i-1})+\frac{f''(\xi)}{2!}(x_{i}-x_{i-1})^{2}$$ $$f'(x_{i}) = \frac{f(x_{i})-f(x_{i-1})}{h}+\frac{f''(\xi)}{2!}h = \frac{f(x_{i})-f(x_{i-1})}{h}+O(h)$$ where $h=x_{i}-x_{i-1}$

**Two point central difference formula for first derivative**

$f(x_{i+1})$ and $f(x_{i-1})$ can be derived using three terms in the Taylor series expansion. $$f(x_{i+1}) = f(x_{i}) + f'(x_{i})(x_{i+1}-x_{i}) + \frac{f''(x_{i})}{2!}(x_{i+1}-x_{i})^{2}+\frac{f'''(\xi_{1})}{3!}(x_{i+1}-x_{i})^{3}$$ where $\xi_{1}$ is a value between $x_{i+1}$ and $x_{i}$ and $$f(x_{i-1}) = f(x_{i}) - f'(x_{i})(x_{i}-x_{i-1}) + \frac{f''(x_{i})}{2!}(x_{i}-x_{i-1})^{2}-\frac{f'''(\xi_{2})}{3!}(x_{i}-x_{i-1})^{3}$$ where $\xi_{2}$ is a value between $x_{i}$ and $x_{i-1}$

Subtracting these gives us $$f(x_{i+1})-f(x_{i-1}) = 2f'(x_{i})h + \frac{f'''(\xi_{1})}{3!}h^{3} + \frac{f'''(\xi_{2})}{3!}h^{3}$$ this can be solved for $f'(x_{i})$ $$f'(x_{i}) = \frac{f(x_{i+1})-f(x_{i-1})}{h^{2}}+O(h^{2})$$

**x-point difference formula for first derivative**
The 3-point forward difference formula uses $f(x_{i+2})$, $f(x_{i+1})$ and $f(x_{i})$ to calculate $f'(x_{i})$. This can be continued for $x$-points, using forward, backward and central differences.

**x-point difference formula for nth derivative**
The Taylor series can be expanded with any number of points to get a higher order derivative.

## Differentiation Formulas using Lagrange Polynomials
The two-point central, three-point forward and three-point backward difference formulas are obtained by considering points $(x_{i}, y_{i}), (x_{i+1}, y_{i+2}). (x_{i+2}, y_{i+2})$. The polynomial passing through the points can be obtained in Lagrange form, and differentiated.

## Richardson's Extrapolation
$$D=\frac{1}{3}(4D\frac{h}{2}-D(h))+O(h^{4})$$ where $h$ is the spacing between points

A more accurate approximationg: $$D=\frac{1}{15}(16D\frac{h}{2}-D(h))+O(h^{6})$$

# Numerical Integration
$$I(f) = \int_{a}^{b} f(x)dx$$

## Rectangle Method
$$I(f) = \int_{a}^{b} f(a)dx = f(a)(b-a)$$

or

$$I(f) = \int_{a}^{b} f(b)dx = f(b)(b-a)$$

Composite: $$I(f) = \int_{a}^{b} f(x)dx \approx h \sum_{i=1}^{N} f(x_{i})$$ where $h=x_{i+1}-x_{i}$

## Midpoint Method
$$I(f) = \int_{a}^{b} f(x)dx \approx _int_{a}^{b} f(\frac{a+b}{2})dx = f(\frac{a+b}{2})(b-a)$$

Composite: $$I(f) = \int_{a}^{b} f(x)dx \approx h \sum_{i=1}^{N} f(\frac{x_{i}+x_{i+1}}{2})$$

## Trapezoidal Method
$$I(f) \approx \frac{[f(a)+f(b)]}{2}(b-a)$$

Composite: $$I(f) \approx \frac{h}{2}[f(a)+f(b)]+h \sum_{i=2}^{N} f(x_{i})$$

## Simpson's Method (hinting to come up)
### Simpson's 1/3 Method (quadratic)
$$I = \int_{x_{i}}^{x_{3}} f(x)dx \approx \int_{x_{1}}^{x_{3}} p(x) dx = \frac{h}{3}[f(x_{1})+4f(x_{2})+f(x_{3})]=\frac{h}{3}[f(a)+4f(\frac{a+b}{2})+f(b)]$$

Composite: $$I(f) \approx \frac{h}{3}[f(a) + 4 \sum_{i=2,4,6}^{N} f(x_{i}) + 2 \sum_{j=3,5,7}^{N-1} f(x_{j}) + f(b)]$$

Can only be used if

- The subinterval must be equally spaced
- The number of subintervals within $[a, b]$ must be an even number

### Simpson's 3/8 Method (cubic)
$$I = \int_{a}^{b} f(x)dx \approx \int_{a}^{b} p(x)dx = \frac{3}{8} h[f(a) + 3f(x_{2}) + 3f(x_{3}) + f(b)]$$

Composite: $$I(f) \approx \frac{3h}{8} [f(a) + 3 \sum_{i=2,5,8}^{N-1}[f(x_{i})+f(x_{i+1})] + 2 \sum_{j=4,7,10}^{N-2} f(x_{j}) + f(b)]$$

Can only be used if

- The subinterval must be equally spaced
- The number of subintervals within $[a, b]$ must be divisible by 3
