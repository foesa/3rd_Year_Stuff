skipTo is called at the start of every procedure for a nonterminal, so for the ELIST procedure the validSymbols are '+' and '-' and otherSymbols are what follows an ELIST

```
proc skipTo(validSymbols, otherSymbols: SymbolSet) {
    if not(symbol in validSymbols) {
	    error(0);
		validSymbols := validSymbols + otherSymbols;

		while not symbol in validSymbols {
		    do nextSymbol;
		}
	}
}
```
