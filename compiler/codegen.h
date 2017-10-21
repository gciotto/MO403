#ifndef _CODEGEN_H_
#define _CODEGEN_H_

#include "tree.h"
#include "symboltable.h"
#include "slc.h"

void processProgram(void *p);
int stackHeight();

void processFuncDecl(TreeNodePtr p, int ismain);
SymbEntryPtr processFormals(TreeNodePtr p, int* lastDispl, int* count);
void processLabels (TreeNodePtr p);
void processTypes (TreeNodePtr p);
int processVariables (TreeNodePtr p);
void processVarDecl(TreeNodePtr p, int* currentDispl);
void processFunctions (TreeNodePtr p);
void processStatements (TreeNodePtr p, int variableDispl);
void processUnlabeledStatements (TreeNodePtr unlabeledStatement);
void processAssignment (TreeNodePtr p);
TypeDescrPtr processFunctionCall (TreeNodePtr functionCall);
void processGoto (TreeNodePtr p);
void processReturn (TreeNodePtr p);
void processConditional (TreeNodePtr p);
void processLoop (TreeNodePtr p);
void processCompound (TreeNodePtr p);
TypeDescrPtr processExpr(TreeNodePtr p);
TypeDescrPtr processSimpleExpr(TreeNodePtr p);
TypeDescrPtr processTerm (TreeNodePtr p);
TypeDescrPtr processMultiplicativeFactor(TreeNodePtr p);
TypeDescrPtr processAdditiveTerm (TreeNodePtr p);
TypeDescrPtr processFactor (TreeNodePtr p);

void genCode0 (char* instr);
void genCode1 (char* instr, int param);
void genLabel1 (char* instr, int label);
void genLabel2 (char* instr, int label, int param);
void genCode2 (char* instr, int param1, int param2);

void genCodeLabel(int label);
void genCodeLabel1(int label, char* instr, int param1);
void genCodeLabel2(int label, char* instr, int param1, int param2);
#endif