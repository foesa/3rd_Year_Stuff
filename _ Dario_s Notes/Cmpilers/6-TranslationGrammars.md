# Translation Grammars
- A context-free grammar in which the set of terminal symbols is partitioned into a set of input symbols and a setion of action symbols
- The strings in the language specified by a translation grammar are called activity sequences
- A context-free grammar may be converted into a translation grammar by inserting action symbols at appropriate locations within the productions from the context-free grammar
- A translation grammar in which all the action symbols specify output routines is termed a string translation grammar

> Design a Translation Grammar to convert an arithmetic expression from infix to postfix form

- `A+B*C+D`
- `ABC*+D+`
- `A{A}+B{B}*C{C}{*}{+}+D{D}{+}`
	- Represented the input and the output
- `{A}` - action symbol to output symbol `A`

###### Grammar
1. `<E>` $\rightarrow$ `<E>+<T> {+}`
2. `<E>` $\rightarrow$ `<T>`
3. `<T>` $\rightarrow$ `<T>*<P> {*}`
4. `<T>` $\rightarrow$ `<P>`
5. `<P>` $\rightarrow$ `(<E>)`
6. `<P>` $\rightarrow$ `ident {ident}`

> `A+B*C+D`

- `<E>` $\overset{1}\Rightarrow$ `<E>+<T> {+}`
- `<E>+<T> {+}` $\overset{1}\Rightarrow$ `<E>+<T>+<T> {+}{+}`
- `<E>+<T>+<T> {+}{+}` $\overset{2}\Rightarrow$ `<T>+<T>+<T> {+}{+}`
- `<T>+<T>+<T> {+}{+}` $\overset{4}\Rightarrow$ `<P>+<T>+<T> {+}{+}`
- `<P>+<T>+<T> {+}{+}` $\overset{6}\Rightarrow$ $ident_{A}$`+<T>+<T> {identA}{+}{+}`
- $ident_{A}$`+<T>+<T> {+}{+}{identA}` $\overset{3}\Rightarrow$ $ident_{A}$`+<T>*<P>+<T> {identA}{*}{+}{+}`
- $ident_{A}$`+<T>*<P>+<T> {+}{+}{identA}{*}` $\overset{*}\Rightarrow$ $ident_{A}$`+`$ident_{B}$`*`$ident_{C}$`+`$ident_{D}$ {identA}{identB}{identC}{*}{+}{identD}{+}`

###### E-list Grammar
1. `<E>` $\rightarrow$ `<T><E-list>`
2. `<E-list>` $\rightarrow$ `+<T> {+}<E-list>`
3. `<E-list>` $\rightarrow \varepsilon$
4. `<T>` $\rightarrow$ `<P><T-list>`
5. `<T-list>` $\rightarrow$ `*<P> {*}<T-List>`
6. `<T-list>` $\rightarrow \varepsilon$
7. `<P>` $\rightarrow$ `(<E>)`
8. `<P>` $\rightarrow$ `ident {ident}`

- `A` is an attribute

# Synthesized Attributes
1. `<S>` $\rightarrow$ `<E> {answer}`
2. `<E>` $\rightarrow$ `<E>+<T>`
3. `<E>` $\rightarrow$ `<T>`
4. `<T>` $\rightarrow$ `<T>*<P>`
5. `<T>` $\rightarrow$ `<P>`
6. `<P>` $\rightarrow$ `(<E>)`
7. `<P>` $\rightarrow$ `const`

## (1+2)*(3+4)

- `<S>` $\overset{1}\Rightarrow$ `<E> {answer}`
- `<E> {answer}` $\overset{3}\Rightarrow$ `<T> {answer}`
- `<T> {answer}` $\overset{4}\Rightarrow$ `<T>*<P> {answer}`
- `<T>*<P> {answer}` $\overset{5}\Rightarrow$ `<P>*<P> {answer}`
- `<P>*<P> {answer}` $\overset{6}\Rightarrow$ `(<E>)*<P> {answer}`
- `(<E>)*<P> {answer}` $\overset{2}\Rightarrow$ `(<E>+<T>)*<P> {answer}`
- `(<E>+<T>)*<P> {answer}` $\overset{3}\Rightarrow$ `(<T>*<T>)+<P> {answer}`
- `(<T>*<T>)+<P> {answer}` $\overset{5}\Rightarrow$ `(<P>*<T>)+<P> {answer}`
- `(<P>*<T>)+<P> {answer}` $\overset{7}\Rightarrow$ `(`$const_{1}$`*<T>)+<P> {answer}`
- `(`$const_{1}$`*<T>)+<P> {answer}` $\overset{*}\Rightarrow$ `(`$const_{1}$`*`$const_{2}$`)+<P> {answer}`
- `(`$const_{1}$`*`$const_{2}$`)+<P> {answer}` $\overset{*}\Rightarrow$ `(`$const_{1}$`*`$const_{2}$`)+(`$const_{3}$`*`$const_{4}$`) {answer}`

> Attribute that goes up: synthesized

> Attributes that go accross: inherited

- $const_{1}$ synthesizes attribute `<P>`, which synthesizes attributes `<T>`, which synthesizes attribute `<E>`
- $const_{2}$ synthesizes attribute `<P>`, which synthesizes attributes `<T>`
- `(<E>)` is synthesized by `<E>` and `<T>`
- Eventually, `{answer}` inherits `<E>`
- `{answer}` - action symbol to print the value of inherited attribute

## 1+2*3

- `<S>` $\overset{1}\Rightarrow$ `<E> {answer}`
- `<E> {answer}` $\overset{2}\Rightarrow$ `<E>+<T> {answer}`
- `<E>+<T> {answer} ` $\overset{4}\Rightarrow$ `<E>+<T>*<P> {answer}`
- `<E>+<T>*<P> {answer}` $\overset{3}\Rightarrow$ `<T>+<T>*<P> {answer}`
- `<T>+<T>*<P> {answer}` $\overset{5}\Rightarrow$ `<P>+<T>*<P> {answer}`
- `<P>+<T>*<P> {answer}` $\overset{7}\Rightarrow$ $const_{1}$`+<T>*<P> {answer}`
- $const_{1}$`+<T>*<P> {answer}` $\overset{5}\Rightarrow$ $const_{1}$`+<P>*<P> {answer}`
- $const_{1}$`+<P>*<P> {answer}` $\overset{7}\Rightarrow$ $const_{1}$`+`$const_{2}$`*<P> {answer}`
- $const_{1}$`+`$const_{2}$`*<P> {answer}` $\overset{7}\Rightarrow$ $const_{1}$`+`$const_{2}$`*`$const_{3}$` {answer}`

# Inherited Attributes
- Consider the Context Free Grammar:
    - `<decl>` $\rightarrow$ `type ident <ident list>`
	- `<ident list>` $\rightarrow$ `, ident <ident list>`
	- `<ident list>` $\rightarrow \varepsilon$
- Where ident is a lexical token with
    - Class part "ident"
	- Value pointer to symbol table entry describing
    	- The identifier 
		- Type is a lexical token with class part "type" and value bool, bloat, int

1. `<decl>` $\rightarrow$ `type ident {set type} <ident list>`
2. `<ident list>` $\rightarrow$ `, ident {set type} <ident list>`
3. `<ident list>` $\rightarrow \varepsilon$

- `<decl>` $\overset{1}\Rightarrow$ $type_{int}$ $ident_{pointerA}$ `{set type} <ident list>`

> `{set type}` inherits `pointer A` from `ident` and `int` from `type`

- $type_{int}$ $ident_{pointerA}$ `{set type} <ident list>` $\overset{2}\Rightarrow$ $type_{int}$ $ident_{pointerA}$ `{set type} ,` $ident_{pointerB}$ `{set type} <ident list>`

> `<ident list>` inherits int

> `{set type}` inherits `pointer B` from `ident` and `int` from `<ident list>`

## $A_{1}*(B_{2}+C_{3})$

###### Symbol Table
| |Name|Type|Address|
|-|-|-|-|
|1|A|
|2|B|
|3|C|
|4|
|5|

- Allocate a new symbol-table entry for (describing) a partial result

- *Infix* - A*(B+C)
- *Postfix* - ABC+*
- Activity Sequence
    - `A*(B+C {ADD}) {MULT}`
	- `{`$Add_{2, 3, 4}$`} {`$Mult_{1, 4, 5}$`}`

1. $<E> \rightarrow <E>+<T>$ `{`ADD`}`
2. $<E> \rightarrow <T>$
3. $<T> \rightarrow <T>*<P>$ `{`MULT`}`
4. $<T> \rightarrow <P>$
5. $<P> \rightarrow (<E>)$
6. $<P> \rightarrow ident$

- $<E> \overset{2}\Rightarrow <T>$
- $<T> \overset{3}\Rightarrow <T>*<P>$`{`MULT`}`
- $<T>*<P>$`{`MULT`}` $\overset{3}\Rightarrow <P>*<P>$`{`MULT`}`
- $<P>*<P>$`{`MULT`}` $\overset{6}\Rightarrow ident_{1}*<P>$`{`MULT`}`
- $ident_{1}*<P>$`{`MULT`}` $\overset{5}\Rightarrow ident_{1}*(<E>)$`{`MULT`}`
- $ident_{1}*(<E>)$`{`MULT`}` $\overset{1}\Rightarrow ident_{1}*(<E>+<T>)$`{`MULT`} {`ADD`}`
- $ident_{1}*(<E>+<T>)$`{`MULT`} {`ADD`}` $\overset{*}\Rightarrow ident_{1}*(ident_{2}+ident_{3})$`{`MULT`} {`ADD`}`

1. $<E>_{p} \rightarrow <E>_{q}+<T>_{v}$ `{`$ADD_{s, t, u}$`}`
    - $s \leftarrow q$
	- $t \leftarrow v$
	- $(p, u) \leftarrow NEW T$
2. $<E>_{p} \rightarrow <T>_{q}$
3. $<T>_{p} \rightarrow <T>_{q}*<P>_{v}$ `{`$MULT_{s, t, u}$`}`
    - $s \leftarrow q$
	- $t \leftarrow v$
	- $(p, u) \leftarrow NEW T$
4. $<T>_{p} \rightarrow <P>_{q}$
5. $<P>_{p} \rightarrow (<E>_{q})$
6. $<P>_{p} \rightarrow ident_{q}$

> Where $<E>_{p}, <T>_{p}$ & $<P>_{p}$ synthesized $p$, all action symbol attributes are inherited and $NEW T$ allocated a new symbol table entry (for) describing a partial result

- Design Attributed Translations for:
1. $<E> \rightarrow <E> <ADDOP> <T>$
2.
    - $<VARIABLE> \rightarrow ident$
    - $<VARIABLE> \rightarrow ident[<E>]$
3. $ident := <E>$

## $<s> \rightarrow REPEAT <s> UNITL <c>$

- *REPEAT* and *UNTIL* aren't real - syntactic sugar

#### Flow
1. $EXECUTE <s>$
2. $EVALUATE <c>$
3. If False, back to 1

#### Translation
1. {LABEL}
2. $<s>$
3. $<c>$
4. {$JUMPF_{1}$}

- $<s> \rightarrow REPEAT$ `{`$LABEL_{p}$`}` $<s> UNTIL <c>_{q}$ `{`$JUMPF_{r, s}$`}`
    - $r \leftarrow q$
	- $(p, s) \leftarrow NEW L$

> Where $<c>_{p}$ synthesized $p$, all action symbol attributes are inherited and $NEW L$ allocates a new symbol table entry for a table

## $<s> \rightarrow IF <c> THEN <s>$

#### Flow
1. EVALUATE `<c>`
2. True? False?
3. If true, `<s>`
4. If false, skip 3

#### Translation
1. `<c>`
2. {JUMPF~4~}
3. `<s>`
4. {LABEL}

- `<s>` $\rightarrow$ IF `<c>`~p~ {JUMPF~q,r~} THEN `<s>` {LABEL~s~}
    - q $\leftarrow$ p
	- (r, s) $\leftarrow$ NEW L

> Where `<c>`~p~ synthesized `p`, all action symbol attributes are inherited and `NEW L` allocates a new symbol table entry for a table

#### Symbol Table
|Type |Label Value|
|-|-|
|LABEL|A00128...  |

- for $(r, s)$, both $r$ and $s$ pointing to the same entry in symbol table
- If your jump come before the label, it's not in the symbol table yet
- If only 0 in the symbol table, but the address of the following instruction in the symbol table
- When see the label, check if symbol table entry is 0
- If it is, label replaces placeholder value in object code

## <s> $\rightarrow$ WHILE <c> DO <s>

#### Flow
1. EVALUATE `<c>`
2. True? False?
3. If true, `<s>`
4. Go back to 1
5. If false, skip 3 and 4

#### Translation
1. {LABEL}
2. `<c>`
3. {JUMPF~6~}
4. `<s>`
5. {JUMP~1~}
6. {LABEL}

- `<s>` $\rightarrow$ WHILE {LABEL~t~} `<c>`~p~ {JUMPF~q,r~} DO `<s>` {JUMP~u~} {LABEL~s~}
    - (t, u) $\leftarrow$ NEW L
    - q $\leftarrow$ p
	- (r, s) $\leftarrow$ NEW L

## Sample Program
```
PROGRAM TEST(INPUT, OUTPUT)
VAR
	CH: CHAR.
	X, Y: REAL,
	I, J, K: INTEGER;
BEGIN

END.
```

- `<program>`~p~ $\rightarrow$ PROGRAM IDENT~q~ (`<IDENT LIST>`); `<DECLERATIONS>` `<COMPOUND STATEMENT>`.
    - p $\leftarrow$ q
    - (`<ident list>`~p~ {SET FILE}~q~);...
        - p $\leftarrow$ q
    - `<ident list>`~p~ $\rightarrow$ `<ident list>`~q~, IDENT~r~ {LINK ID}~s,t~
        - s $\leftarrow$ r
    	- t $\leftarrow$ q

> *link id links the identifier pointed to by `s` in front of the linked-list of identifiers pointed to by `t`*

-
    - `<ident list>`~p~ $\rightarrow$ IDENT~q~
        - p $\leftarrow$ q
    - `<declerations>`~p~ {ALLOCATE}~q~
        - p $\leftarrow$ q
	- `<declerations>`~p~ $\rightarrow \varepsilon$
    	- p $\leftarrow$ NIL
	- `<declerations>`~p~ $\rightarrow$ VAR `<dec list>`~q~
    	- `<dec list>`~p~ $\rightarrow$ `<ident list>`~q~ : `<type>`~r~ {SET TYPE}~s,t~ `<more decs>`~u~


|Name  |TYPE|ADDRESS|...|ID LINK|DEC LINK|
|-     |-   |-      |-  |-      |-       |
|TEST  |
|INPUT |FILE|
|OUTPUT|FILE|
|CH    |CHAR|
|X     |REAL|
|Y     |REAL|
|I     |INT |
|J     |INT |
|K     |INT |
|...   |

- ID Link starts as `null`

## Consider the Context-Free Grammar
1. `<S>` $\rightarrow$ ident := `<E>`
2. `<E>` $\rightarrow$ ident `<R>`
3. `<R>` $\rightarrow$ + ident `<R>`
4. `<R>` $\rightarrow$ * ident `<R>`
5. `<R>` $\rightarrow \varepsilon$

`X := A+B*C`

- There's no precendence
- If using this to parse, you'll get
    - `A+B*C`
	- Not `A+(B*C)`

### Translation Grammar
1. `<S>` $\rightarrow$ ident := `<E>` {assign}
2. `<E>` $\rightarrow$ ident `<R>`
3. `<R>` $\rightarrow$ + ident {add} `<R>`
4. `<R>` $\rightarrow$ * ident {mul} `<R>`
5. `<R>` $\rightarrow \varepsilon$

### Attributed Translation Grammar
1. `<S>` $\rightarrow$ ident~p~ := `<E>`~q~ {assign~r,s~}
    - r $\leftarrow$ q
	- s $\leftarrow$ p
2. `<E>`~p~ $\rightarrow$ ident~q~ `<R>`~r,s~
    - r $\leftarrow$ q
	- p $\leftarrow$ s
3. `<R>`~p,q~ $\rightarrow$ + ident~r~ {add~s,t,u~} `<R>`~v,w~
    - s $\leftarrow$ p
	- t $\leftarrow$ r
	- (v, u) $\leftarrow$ NEW T
	- q $\leftarrow$ w
4. `<R>`~p,q~ $\rightarrow$ * ident~r~ {mul~s,t,u~} `<R>`~v,w~
    - s $\leftarrow$ p
	- t $\leftarrow$ r
	- (v, u) $\leftarrow$ NEW T
	- q $\leftarrow$ w
5. `<R>`~p,q~ $\rightarrow \varepsilon$
    - q $\leftarrow$ p

> Where R~p~ synthesized p, all action symbol attributes are inherited, and NEW T allocated a new symbol table entry (for) describing a partial result

- Inherted attribute at $\varepsilon$ begins to synthesize back up the tree
- Eventually `<E>` at top of tree is synthesized with attribute
- {assign} inherits the result

## A*(B+C) - make derivation tree and attributions  using e-list grammar
