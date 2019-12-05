%{
# include "romcalc.tab.h"
void yyerror();
%}

%%
I             { return ONE; }
V       	  { return FIVE; }
X             { return TEN; }
L             { return FIFTY; }
C             { return HUNDRED; }
D             { return FIVE_HUNDRED; }
M             { return THOUSAND; }

"+"           { return ADD; }
"-"           { return SUB; }
"*"           { return MUL; }
"/"           { return DIV; }
"{"           { return OPEN_BR; }
"}"           { return CLOSE_BR; }

[ /t]         { }
\n            { return EOL; }
.             { yyerror("syntax error"); }
%%


