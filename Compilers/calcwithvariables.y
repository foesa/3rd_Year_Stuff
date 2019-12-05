%{
#  include <stdio.h>
void yyerror();
int yylex();
int yyparse();

int vars[26];

%}

/* declare tokens */
%token NUMBER VAR
%token SET PRINT SEMICOLON
%token ADD SUB MUL DIV ABS
%%

calclist: /* nothing */
 | calclist vars
 ;

vars: VAR SET exp SEMICOLON { vars[$1-97] = $3; }
 | PRINT VAR SEMICOLON { printf("%d\n", vars[$2-97]); }
 ;

exp: factor
 | exp ADD factor { $$ = $1 + $3; }
 | exp SUB factor { $$ = $1 - $3; }
 ;

factor: term
 | factor MUL term { $$ = $1 * $3; }
 | factor DIV term { $$ = $1 / $3; }
 ;

term: VAR   { $$ = vars[$1-97]; }
 | SUB NUMBER   { $$ = $2 * -1; }
 | NUMBER   { $$ = $1; }
 ;
%%

int main()
{
  yyparse();
  return 0;
}

void yyerror(char *s)
{
  fprintf(stderr, "%s\n", s);
}
