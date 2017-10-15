
/*
   Test of the SL compiler.
   
   Returns code execution 0 even in case of errors so as not to
   confuse SuSy.
   
*/

/* Last update: "slc.c: 2017-04-21 (Fri)  14:20:45 BRT (tk)" */

#define MAIN

#include <stdio.h>
#include <stdlib.h>
#include "slc.h"
#include "tree.h"
#include "codegen.h"

extern int line_num;
extern char *yytext;

void SemanticError(char *s) {

  // No line number info on trees
  printf("Semantic error.\n");
  exit(0);
  
}

int yyerror(char *s) {
  
  printf("Lexical or syntactical error detected on line %d. Last token read: '%s'\n",line_num,yytext);
  return 0;
  
}

int main(int argc, char **argv) {
  int sh;
  int error;

  error = yyparse();
  if (error!=0) 
    return 0;  // error message printed already

  sh = stackHeight();
  if (sh>1) {
    printf("Stack hight: %d; execution aborted\n",sh);
  } else  {
    dumpTree(getTree(), 0);
    processProgram(getTree()); // generates code
  }
  return 0;
  
} // main
