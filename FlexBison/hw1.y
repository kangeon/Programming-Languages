%{
#include <iostream>
#include <stdio.h>
#include <stdlib.h>

extern int yylex();
extern int yyparse();
extern FILE *yyin;

void yyerror(const char *p);
%}

%union {
	int ival;
	float fval;
	char *sval;
}

%start program

//define constant-string tokens:
%token PRINT IF THEN GOTO INPUT LET GOSUB RETURN CLEAR LIST RUN 
%token END
%token CR
%token RELOP
%token ASSIGN
%token DIGIT
%token MULDIV
%token PLUMIN
%token VAR
%token COMMA
%token STRING
%token LPAREN
%token RPAREN

%%

program : line
	   | program line

line : number statement CR
     | statement CR

statement : PRINT expr-list
	     | IF expression RELOP expression THEN statement
	     | GOTO expression
	     | INPUT var-list
	     | LET VAR ASSIGN expression
	     | GOSUB expression
	     | RETURN
	     | CLEAR
	     | LIST
	     | RUN
	     | END
	     ;

expr-list : STRING
		| expression
		| expr-list COMMA STRING
		| expr-list COMMA expression
	     ;

var-list : VAR
	    | var-list COMMA VAR
	    ;

expression : PLUMIN term
		 | expression PLUMIN term
	       ;

term : factor
	| term MULDIV factor
	;

factor : VAR
	  | number
	  | LPAREN expression RPAREN
	  ;

number : DIGIT
	  | number DIGIT
	  ;
%%

main() {
	
	if(yyparse()==0){
		std::cout << "Parse Successful" << std::endl;
	}
	else{
		std::cout << "Parse Unsuccessful" << std::endl;
	}
}

void yyerror(const char *p){
	std::cerr << "error: " << p << std::endl;
	exit(-1);
}
  

