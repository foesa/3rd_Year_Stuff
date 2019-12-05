%{
# include "calcwithvariables.tab.h"

void yyerror(char *s);
int yylex();

%}

%%
"+"	     { return ADD; }
"-"	     { return SUB; }
"*"	     { return MUL; }
"/"	     { return DIV; }
"|"      { return ABS; }
";"      { return SEMICOLON; }
[0-9]	 { yylval = atoi(yytext); return NUMBER; }
[a-z]    { yylval = yytext[0]; return VAR; }
":="     { return SET; }
"print " { return PRINT; }

\n      { /* ignore new line */}
[ \t]   { /* ignore white space */ }
.	    { yyerror("syntax error");}
%%
