#include "gpp_lib.h"
#define SIZE__ARRAY 9999

int array[SIZE__ARRAY];
int current=0;

int main(void) {
    for(int i=0; i<SIZE__ARRAY; ++i) {
        array[i] = -999;
    }
    current = 0;
    
    yyparse();
    return 0;
}

void yyerror(char *s) {
    fprintf(stderr, "%s\n", s);
}

int itisPLUS(int op1, int op2) {
    int temp;
    temp = op1 + op2;
    return temp;
}  

int itisMINUS(int op1, int op2) {
    int temp;
    temp = op1 - op2;
    return temp;
}  

int itisMULTI(int op1, int op2) {
    int temp;
    temp = op1 * op2;
    return temp;
} 

int itisDBLMULT(int op1, int op2) {
    int result=1;
    for(int i=0; i<op2; ++i) {
        result *= op1;
    }
    return result;
}

int itisDIVIDE(int op1, int op2) {
    int temp;
    temp = (int)(op1 / op2);
    return temp;
}

char* itisEQUAL_INT(int op1, int op2) {
    if (op1 == op2) 
        return "true";
    return "false";
}

void fillTheArray(int val) {
    array[current] = val;
    ++current;
}

void listAppend(int val, int* arr) {
    array[current] = val;
    ++current;
}

void listConcat(int* arr1, int* arr2) { }

void printList() {
    if(current == 0) {
        printf("NIL\n");
    }
    else {
        printf("( ");
        for(int i=0; i< current;++i) {
            printf("%d ",array[i]);
        }
        printf(")\n");
        current = 0;
    }   
}

char* itisAND(char* op1, char* op2) {
    if ( (strcmp(op1, "true") == 0) && (strcmp(op2, "true") == 0))
        return "true";
    return "false";
}

char* itisOR(char* op1, char* op2) {
    if ( (strcmp(op1, "true") == 0) || (strcmp(op2, "true") == 0))
        return "true";
    return "false";
}

char* itisNOT(char* op1) {
    if ( (strcmp(op1, "true") == 0))
        return "false";
    return "true";
}

char* itisEQUAL_STR(char* op1, char* op2) {
    if ( (strcmp(op1, op2) == 0))
        return "true";
    return "false";
}

char* itisLESS(int a, int b) {
    if(a <= b)
        return "true";
    return "false";
}

char* returnString(char* str) {
    return str;
}