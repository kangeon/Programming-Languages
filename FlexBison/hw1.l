%{
#include <iostream>
#include <stdlib.h>
#include <stdio.h>
#define YY_DECL int yylex()

#include "hw1.tab.h"

bool sawlet = 0; 

%}

%option noyywrap

%%
"PRINT"				{ return PRINT; }
"IF"					{ return IF; }
"THEN"				{ return THEN; }
"GOTO" 				{ return GOTO; }
"INPUT"				{ return INPUT; }
"LET"					{ sawlet=1; return LET; }
"GOSUB"				{ return GOSUB; }
"RETURN"				{ return RETURN; }
"CLEAR"				{ return CLEAR; }
"LIST"				{ return LIST; }
"RUN"					{ return RUN; }
"END"					{ return END; }
\n					{ return CR; }
"<"|">"				{ return RELOP; }
"="					{ if(sawlet==0){return RELOP;} else{sawlet=0; return ASSIGN;}}
[0-9]					{ return DIGIT; }
"*"|"/"				{ return MULDIV; }
"+"|"-"				{ return PLUMIN; }
[A-Z]					{ return VAR; }
","					{ return COMMA; }
\"+[ a-zA-Z0-9_]*+\"		{ return STRING; }
"("					{ return LPAREN; }
")"					{ return RPAREN; }
[ \t]					{ /* ignore white characters */ }
.					;
%%

 
