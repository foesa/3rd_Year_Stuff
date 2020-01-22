# LL (1) Grammars
1. `<s>` $\rightarrow$ `<A>`b`<B>`
2. `<s>` $\rightarrow$ d
3. `<A>` $\rightarrow$ `<C><A>`b
4. `<A>` $\rightarrow$ `<B>`
5. `<B>` $\rightarrow$ c`<s>`d
6. `<B>` $\rightarrow \varepsilon$
7. `<C>` $\rightarrow$ a
8. `<c>` $\rightarrow$ ed

## bcdd
- `<s>` $\overset{1}\Rightarrow$ `<A>`b`<B>`
- `<A>`b`<B>` $\overset{4}\Rightarrow$ `<B>`b`<B>`
- `<B>`b`<B>` $\overset{6}\Rightarrow$ b`<B>`
- b`<B>` $\overset{5}\Rightarrow$ bc`<s>`d
- bc`<s>`d $\overset{2}\Rightarrow$ bcdd

| |First $\alpha$  |Follow(`<X>`)                            |Select(`<X>` $\rightarrow \alpha$)      |
|-|----------------|-----------------------------------------|----------------------------------------|
|1|`<A>`b={a,b,c,e}|                                         |{a,b,c,e}                               |
|2|{d}             |                                         |{d}                                     |
|3|`<C>`={a,e}     |                                         |{a,e}                                   |
|4|`<B>`={c}       |                                         |First(`<B>`)+Follow(`<A>`)={c}+{b}={b,c}|
|5|{c}             |                                         |{c}                                     |
|6|{}              |Follow(`<B>`)=Follow(`<A>`)+Follow(`<s>`)|Follow(`<B>`)={b,d,$\dashv$}            |
| |                |={b}$\cup${d,$dashv$}={b,d,$dashv$}      |                                        |
|7|{a}             |                                         |{a}                                     |
|8|{e}             |                                         |{e}                                     |

> 4 and 6 are nullable, i.e. can derive $\varepsilon$

|               |a  |b           |c|d           |e  |$\dashv$|
|-|-|-|-|-|-|-|
|`<s>`          |1  |1           |1|2           |1  |        |
|`<A>`          |3  |4           |4|            |3  |        |
|`<B>`          |*6*|6           |5|6           |*6*|        |
|`<C>`          |7  |            | |            |8  |        |
|b              |   |Pop, Advance| |            |   |        |
|d              |   |            | |Pop, Advance|   |        |
|$\triangledown$|   |            | |            |   |        |

1. Replace(`<B>`b`<A>`), Retain
2. Pop, Advance
3. Replace(b`<A><C>`), Retain
4. Replace(`<B>`), Retain
5. Replace(d`<s>`), Advance
6. Pop, Retain
7. Pop, Advance
8. Replace(d), Advance

## Is an <E-List> Grammar LL?
### Grammar
1. `<E>` $\rightarrow$ `<T><E-List>`
2. `<E-List>` $\rightarrow$ +`<T><E-List>`
3. `<E-List>` $\rightarrow \varepsilon$
4. `<T>` $\rightarrow$ `<P><T-List>`
5. `<T-List>` $\rightarrow$ *`<P><T-List>`
6. `<T-List>` $\rightarrow \varepsilon$
7. `<P>` $\rightarrow$ (`<E>`)
8. `<P>` $\rightarrow$ ident

#### Yes :)


