%{
# include <stdio.h>
# include <stdlib.h>
# include <string.h>
int yylex();
void yyerror();
char* to_roman(int num);

char* SYM_HUNDREDS[9] = 
      { "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"};
char* SYM_TENS[9] =
      { "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"};
char* SYM_ONES[9] =
      { "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"};
%}

/* declare tokens */
%token ONE FIVE TEN FIFTY HUNDRED FIVE_HUNDRED THOUSAND
%token ADD SUB MUL DIV OPEN_BR CLOSE_BR
%token EOL
%%
calclist: /* nothing */
 | calclist expr EOL { printf("%s\n", to_roman($2)); }
 ;
 
expr: factor
 | expr ADD factor                { $$ = $1 + $3; }
 | expr SUB factor                { $$ = $1 - $3; }
 ;
 
factor: inner_expr
 | factor MUL inner_expr          { $$ = $1 * $3; }
 | factor DIV inner_expr          { $$ = $1 / $3; }
 ;
 
inner_expr: num
 | OPEN_BR expr CLOSE_BR          { $$ = $2; }
 ;

num: thousands hundreds tens ones { $$ = $1 + $2 + $3 + $4; }
 | thousands hundreds tens        { $$ = $1 + $2 + $3; }
 | thousands hundreds ones        { $$ = $1 + $2 + $3; }
 | thousands hundreds             { $$ = $1 + $2; }
 | thousands tens ones            { $$ = $1 + $2 + $3; }
 | thousands tens                 { $$ = $1 + $2; }
 | thousands ones                 { $$ = $1 + $2; }
 | thousands
 | hundreds tens ones             { $$ = $1 + $2 + $3; }
 | hundreds tens                  { $$ = $1 + $2; }
 | hundreds ones                  { $$ = $1 + $2; }
 | hundreds
 | tens ones                      { $$ = $1 + $2; }
 | tens
 | ones
 ;

ones: ONE                       { $$ = 1; }
 | ONE ONE                      { $$ = 2; }
 | ONE ONE ONE                  { $$ = 3; }
 | ONE FIVE                     { $$ = 4; }
 | FIVE                         { $$ = 5; }
 | FIVE ONE                     { $$ = 6; }
 | FIVE ONE ONE                 { $$ = 7; }
 | FIVE ONE ONE ONE             { $$ = 8; }
 | ONE TEN                      { $$ = 9; }
 ;

tens: TEN HUNDRED               { $$ = 90; }
 | FIFTY TEN TEN TEN            { $$ = 80; }
 | FIFTY TEN TEN                { $$ = 70; }
 | FIFTY TEN                    { $$ = 60; }
 | FIFTY                        { $$ = 50; }
 | TEN FIFTY                    { $$ = 40; }
 | TEN TEN TEN                  { $$ = 30; }
 | TEN TEN                      { $$ = 20; }                     
 | TEN                          { $$ = 10; }
 ;

hundreds:  HUNDRED THOUSAND             { $$ = 900; }
 | FIVE_HUNDRED HUNDRED HUNDRED HUNDRED { $$ = 800; }
 | FIVE_HUNDRED HUNDRED HUNDRED         { $$ = 700; } 
 | FIVE_HUNDRED HUNDRED                 { $$ = 600; }
 | FIVE_HUNDRED                         { $$ = 500; }
 | HUNDRED FIVE_HUNDRED                 { $$ = 400; } 
 | HUNDRED HUNDRED HUNDRED              { $$ = 300; }
 | HUNDRED HUNDRED                      { $$ = 200; }
 | HUNDRED                              { $$ = 100; }
 ;

thousands: THOUSAND THOUSAND THOUSAND   { $$ = 3000; }
 | THOUSAND THOUSAND                    { $$ = 2000; }
 | THOUSAND                             { $$ = 1000; }
 ;

%%

char* add_roman_unit(char* romanNum, char** symbols, int num)
{
  char* newRoman = NULL;
  if (num != 0) {
    char* symbol = symbols[num-1];
    newRoman = calloc( sizeof(romanNum) + strlen(symbol), 1);
    if (newRoman) {
        newRoman = strncpy(newRoman, romanNum, strlen(romanNum));
        newRoman = strncat(newRoman, symbol, strlen(symbol));
    }
    else
      yyerror("Ran out of memory");
  }
  return newRoman;
}

char* to_roman(int num) 
{
  if (num == 0)
    return "Z";   // actually should be nullus B)

  char* romanNum = "";
  
  if (num < 0) {
    char* newRoman = calloc( sizeof(romanNum) + 1, 1 );
    if (!newRoman)
      yyerror("Ran out of memory");

    newRoman = strncpy(newRoman, romanNum, strlen(romanNum));
    newRoman = strncat(newRoman, "-", 1);
    romanNum = newRoman;
    num = -num;

  }
  
  // assign thousands
  if (num / 1000 > 0)
  {
    int rem;
    for ( rem = num; rem >= 1000; rem -= 1000) {
      char* newRoman = calloc( sizeof(romanNum) + 1, 1 );
      if (!newRoman)
        yyerror("Ran out of memory");
        
      newRoman = strncpy(newRoman, romanNum, strlen(romanNum));
      newRoman = strncat(newRoman, "M", 1);
      romanNum = newRoman;
    }
  }
  
  // assign hundreds
  num = num % 1000;
  char* newRoman = add_roman_unit(romanNum, SYM_HUNDREDS, num/100);
  if (newRoman) {
    romanNum = newRoman;
  }
  newRoman = NULL;

   // assign tens
  num = num % 100;
  newRoman = add_roman_unit(romanNum, SYM_TENS, num/10);
  if (newRoman) {
    romanNum = newRoman;
  }
  newRoman = NULL;

  // assign ones
  num = num % 10;
  newRoman = add_roman_unit(romanNum, SYM_ONES, num);
  if (newRoman) {
    romanNum = newRoman;
  }
  return romanNum;
}

int main()
{ 
  yyparse();
  return 0;
}

void yyerror(char* s)
{
  printf("%s\n", s);
  exit(0);
}
