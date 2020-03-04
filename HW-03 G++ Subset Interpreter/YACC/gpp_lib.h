#ifndef __gpp_lib_h_
#define __gpp_lib_h_

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>

#include "y.tab.h"

int yyparse(void);
int yylex(void);
void yyerror(char *);

// function to calculate int expressions
int itisPLUS(int, int);
int itisMINUS(int, int);
int itisMULTI(int, int);
int itisDIVIDE(int, int);
int itisDBLMULT(int, int);


// function to calculate list expressions
void fillTheArray(int);
void printList(); 
void listAppend(int, int*);
void listConcat(int*,int*);

// function to calculate binary expressions
char* itisAND(char*, char*);
char* itisOR(char*, char*);
char* itisNOT(char*);
char* itisEQUAL_STR(char*, char*);
char* itisEQUAL_INT(int, int);
char* itisLESS(int,int);
char* returnString(char*);

#endif