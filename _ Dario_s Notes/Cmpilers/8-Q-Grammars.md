# Quasi-Simple Grammars
1. `<s>` $\rightarrow$ a`<A><s>`
2. `<s>` $\rightarrow$ b
3. `<A>` $\rightarrow$ c`<A><s>`
4. `<A>` $\rightarrow \varepsilon$

## aacbb
- `<s>` $\overset{1}\Rightarrow$ a`<A><s>`
- a`<A><s>` $\overset{4}\Rightarrow$ a`<s>`
- a`<s>` $\overset{1}\Rightarrow$ aa`<A><s>`
- aa`<A><s>` $\overset{3}\Rightarrow$ aac`<A><s><s>`
- aac`<A><s><s>` $\overset{4}\Rightarrow$ aac`<s><s>`
- aac`<s><s>` $\overset{2}\Rightarrow$ aacb`<s>`
- aacb`<s>` $\overset{2}\Rightarrow$ aacbb

# FOLLOW and SELECT Sets
|`<s>`$\dashv$                  |
|-------------------------------|
|`<s>` $\rightarrow$ a`<A>`     |
|`<A>` $\rightarrow$ c`<A>`b    |
|`<A>` $\rightarrow \varepsilon$|

- FOLLOW(`<A>`) = {b} + FOLLOW(`<s>`)
    - `<A><s>` $\overset{1}\Rightarrow$ `<A>`a`<A><s>`
    - `<A><s>` $\overset{2}\Rightarrow$ `<A>`b
- FOLLOW(`<s>`)  = {a, $\dashv$}
	
> In the case `<s>` $\rightarrow$ a`<A>`, the only thing that could follow `<A>` here is whatever could have followed `<s>`. I.e. if you have `<s>` $\rightarrow$ `<s>`c, then FOLLOW(`<A>`) = {c}

1. `<s>` $\rightarrow$ a`<A><s>`
2. `<s>` $\rightarrow$ b
3. `<A>` $\rightarrow$ c`<A><s>`
4. `<A>` $\rightarrow \varepsilon$

- FOLLOW(`<A>`) = {a} + {b}

|               |a|b|c|$\dashv$|
|---------------|-|-|-|--------|
|`<s>`          |1|2| |        |
|`<A>`          |4|4|3|        |
|$\triangledown$| | | |ACCEPT  |

1. REPLACE(`<s><A>`), ADVANCE
    - or PUSH(`<A>`), ADVANCE
	- (`<s>` already on the stack)
2. POP, ADVANCE
3. REPLACE(`<s><A>`), ADVANCE
4. POP, RETAIN

- Action number 4 happens in the follow set of `<A>`

|Stack                     |Input        |
|--------------------------|-------------|
|$\triangledown$`<s>`      |aabcc$\dashv$|
|$\triangledown$`<s><A>`   |acbb$\dashv$ |
|$\triangledown$`<s>`      |acbb$\dashv$ |
|$\triangledown$`<s><A>`   |cbb$\dashv$  |
|$\triangledown$`<s><s><A>`|bb$\dashv$   |
|$\triangledown$`<s><s>`   |bb$\dashv$   |
|$\triangledown$`<s>`      |b$\dashv$    |
|$\triangledown$           |$\dashv$     |

**ACCEPT**

- Show that it's a left most derivation
