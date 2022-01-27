%{
#include<stdio.h>
#include<string.h>
#include "y.tab.h"
int count=1;
%}
%x comment
%%
[ ]+ ;
bool return BOOL_TYPE;
char return CHAR_TYPE;
int return INT_TYPE;
real return REAL_TYPE;
string return STR_TYPE ;
int\* return INT_POINT;
char\* return CHAR_POINT;
real\* return REAL_POINT;
if return IF;
else return ELSE;
for return FOR;
do return DO;
while return WHILE;
var return VAR;
func return FUNC;
void return VOID;
return return RETURN;
null return NONE;
"&&" return AND;
"/" return DIV;
"=" return ASSIGN;
"==" return EQ;
">" return G;
">=" return GE;
"<" return L;
"<=" return LE;
"-" return MINUS;
"!" return NOT;
"!=" return NOTEQ;
"||" return OR;
"+" return PLUS;
" * " return MUL;
"&" return ADDR;
"*"  return POINT;
"(" return POTEH;
")" return SOGER;
"{" return POTEH_BLOCK;
"}" return SOGER_BLOCK;
"++" return ADD;
"--" return SUB;
true|false { yylval=atoi(yytext); return BOOL;}
[\'][a-zA-Z0-9@#$%^&*?!][\'] { yylval=*yytext - 'a'; return CHAR;}
0|[1-9][0-9]* { yylval=atoi(yytext); return INT_DEC;} 
0[x|X][0-9A-F]+ { yylval=strtol(yytext,0,16); return INT_HEX; }
[a-zA-Z]+[\_]?[0-9a-zA-Z]* { yylval=atoi(yytext); return ID; }
[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?  {yylval=atoi(yytext); return REAL;}
[\"][^\"]*[\"] return STRING; 
"/*" BEGIN comment;
<comment>[^\n] ;
<comment>\n     ; count++;
<comment>"*/" BEGIN 0;
\n ; count++;
[ \t] ;
":" return COLON;
";" return SEMICOLON;
. return yytext[0]; 
%%





