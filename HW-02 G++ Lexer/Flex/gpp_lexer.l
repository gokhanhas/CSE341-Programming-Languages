%option noyywrap

%{
    #include "my_lexer.h"
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>

    void my_print(char *tokenName);

    void print_error();

%}


%%

"and"                   { my_print("KW_AND"); }
"or"                    { my_print("KW_OR"); }
"not"                   { my_print("KW_NOT"); }
"equal"                 { my_print("KW_EQUAL"); }
"less"                  { my_print("KW_LESS"); }
"nil" | "NIL"           { my_print("KW_NIL"); }
"list"                  { my_print("KW_LIST"); }
"append"                { my_print("KW_APPEND"); }
"concat"                { my_print("KW_CONCAT"); }
"set"                   { my_print("KW_SET"); }
"deffun"                { my_print("KW_DEFFUN"); }
"for"                   { my_print("KW_FOR"); }
"if"                    { my_print("KW_IF"); }
"exit"                  { my_print("KW_EXIT"); }
"load"                  { my_print("KW_LOAD"); }
"disp"                  { my_print("KW_DISP"); }
"true"                  { my_print("KW_TRUE"); }
"false"                 { my_print("KW_FALSE"); }

"+"                     { my_print("OP_PLUS"); }
"-"                     { my_print("OP_MINUS"); }
"/"                     { my_print("OP_DIV"); }
"*"                     { my_print("OP_MULT"); }
"("                     { my_print("OP_OP"); }
")"                     { my_print("OP_CP"); }
"**"                    { my_print("OP_DBLMULT"); }
"'"                     { my_print("OP_OC"); }
","                     { my_print("OP_COMMA"); }


[a-zA-Z][_a-zA-Z0-9]*               { my_print("IDENTIFIER"); }
[1-9][0-9]*                         { my_print("VALUE"); }
";;"+[ \t\r\f][_a-zA-Z0-9" "]*      { my_print("COMMENT"); }
[ \t\r\f]+      


.                       { print_error(); }

%%

void my_print(char *tokenName) {
    printf("TEXT : %s\tTOKEN : %s\n",yytext , tokenName);
}

void print_error(char *tokenName) {
    printf("SYNTAX__ERROR : %s cannot be tokenized !\n",yytext);
    exit(1);
}

int main(int argc, char *argv[]){

   yyin = fopen(argv[1], "r");
   yylex();
   fclose(yyin);
   return 0;
}