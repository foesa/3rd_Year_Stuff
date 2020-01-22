# Error Processing
1. `<s>` $\rightarrow$ a
2. `<s>` $\rightarrow$ (`<s><r>`
3. `<r>` $\rightarrow$ ,`<s><r>`
4. `<r>` $\rightarrow$ )

Grammar for S-Expressions `(a, (a, a), ((a), a))`

|               |a   |,   |(   |)   |$\dashv$|
|---------------|----|----|----|----|--------|
|`<s>`          |1   |R_a_|2   |R_b_|R_c_    |
|`<r>`          |R_d_|3   |R_e_|4   |R_f_    |
|$\triangledown$|R_g_|R_h_|R_i_|R_j_|Accept  |

> Starting Stack: $\triangledown$`<s>`

1. Pop, Advance
2. Replace(`<r><s>`), Advance
3. Push(`<s>`), Advance
4. Pop, Advance

- R_a_, R_b_: ",/)" occurs when s-expression expected
- R_c_: s-expression incomplete
- R_d_, R_e_: Missing comma
- R_g_, R_h_, R_i_, R_j_: * occurs after s-expression

## ((a(,a)$\dashv$
|Stack                     |Input          |
|--------------------------|---------------|
|$\triangledown$`<s>`      |((a(,a)$\dashv$|
|$\triangledown$`<r><s>`   |(a(,a)$\dashv$ |
|$\triangledown$`<r><r><s>`|a(,a)$\dashv$  |
|$\triangledown$`<r><r>`   |(,a)$\dashv$   |

> **Missing comma** - Not very useful error message when pointing at comma

## E-List
1. `<E>` $\rightarrow$ `<T><E-List>`
2. `<E-List>` $\rightarrow$ +`<T><E-List>`
3. `<E-List>` $\rightarrow \varepsilon$
4. `<T>` $\rightarrow$ `<P><T-List>`
5. `<T-List>` $\rightarrow$ *`<P><T-List>`
6. `<T-List>` $\rightarrow \varepsilon$
7. `<P>` $\rightarrow$ (`<E>`)
8. `<P>` $\rightarrow$ ident


|               |Ident|+  |*    |(  |)     |$\dashv$|
|---------------|-----|---|-----|---|------|--------|
|`<E>`          |     |   |     |   |      |        |
|`<E-List>`     |     |2  |     |   |      |        |
|`<T>`          |     |   |     |   |      |        |
|`<T-List>`     |     |   |     |   |      |        |
|`<P>`          |     |   |     |   |      |        |
|)              |     |   |     |   |      |        |
|$\triangledown$|     |   |     |   |      |        |
|{ADD}          |     |out|{ADD}|Pop|Retain|        |
|{MULT}         |     |   |     |   |      |        |

2. Replace(`<E-List>`{ADD}`<T>`), Advance

# Removal of Left-Recursion
- `<E>` $\rightarrow$ `<E>`+`<T>`
- `<E>` $\rightarrow$ `<T>`

Replaced by

- `<E>` $\rightarrow$ `<T><E-List>`
- `<E-List>` $\rightarrow$ +`<T><E-List>`
- `<E-List>` $\rightarrow \varepsilon$

## Left Factoring
- `<E>` $\rightarrow$ ident(`<params>`)
- `<E>` $\rightarrow$ ident

Replaced by

- `<E>` $\rightarrow$ ident`<param part>`
- `<param part>` $\rightarrow$ (`<params>`)
- `<param part>` $\rightarrow \varepsilon$

# The If Statement is Not LL
- `<s>` $\rightarrow$ if `<c>` then `<s>` else `<s>`
- `<s>` $\rightarrow$ if `<c>` then `<s>`

If A=B then if C=D then P $\leftarrow$ Q else X $\leftarrow$ Y

## Left Factor
1. `<s>` $\rightarrow$ if `<c>` then `<s> <else part>`
2. `<else part>` $\rightarrow$ else `<s>`
3. `<else part>` $\rightarrow \varepsilon$
    - Follow(`<else part>`) = Follow(`<s>`) = {else, $\dashv$}

|             |if|then|else|$\dashv$|
|-------------|--|----|----|--------|
|`<s>`        |1 |    |    |        |
|`<else part>`|  |    |2,3 |3       |

1. Replace(`<else part><s>` then `<c>`), Advance
2. Repace(`<s>`), Advance
3. Pop, Retain
