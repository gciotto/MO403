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
TypeDescrPtr processArray (TreeNodePtr p, TypeDescrPtr varType);
TypeDescrPtr processFactor (TreeNodePtr p);

void genCode0 (char* instr, char* comment);
void genCode1 (char* instr, int param, char* comment);
void genCode2 (char* instr, int param1, int param2, char* comment);
void genCode3 (char* instr, int param1, int param2, int param3, char* comment);
void genLabel1 (char* instr, int label, char* comment);
void genLabel2 (char* instr, int label, int param, char* comment);
void genLabel3 (char* instr, int label, int param1, int param2, char* comment);

void genCodeLabel(int label, char* comment);
void genCodeLabel1(int label, char* instr, int param1, char* comment);
void genCodeLabel2(int label, char* instr, int param1, int param2, char* comment);

#endif
