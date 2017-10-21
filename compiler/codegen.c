#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "codegen.h"

int readInstr = 0;

void processProgram(void *p) {

        initSymbolTable();
        processFuncDecl(((TreeNodePtr) p)->comps[0], 1);
        genCode0("END");
        freeTree(p);
}

int stackHeight() {
        return getStackHeight();
}

void processFuncDecl(TreeNodePtr p, int ismain) {

        //dumpNode(p, 0);
        //dumpNode(p->comps[1], 0);

        char* fname = p->comps[1]->str;
        TypeDescrPtr resType = NULL, funType;
        int lastDispl = -4, count, i, currentDispl;

        SymbEntryPtr formals, func, auxFormals;

        incrCurrentLevel();
        if (p->comps[0]->categ != C_EMPTY)
                resType = getType(p->comps[0]);

        formals = processFormals(p->comps[2], &lastDispl, &count);

        if (resType != NULL)
                lastDispl -= resType->size;

        func = newSymbEntry(S_FUNCTION, fname);
        func->descr.functionDescr = (FunctionDescPtr) malloc (sizeof (FunctionDesc));
        func->descr.functionDescr->result = resType;
        func->descr.functionDescr->params = formals;
        func->level = getCurrentLevel() - 1;
        func->descr.functionDescr->displ = lastDispl;

        insertSymbolTable(func);

        //dumpSymbolTable ();

        saveSymbTable(func);

        if (ismain) 
                genCode0("MAIN");
        else {
                func->descr.functionDescr->entLabel = nextLabel(); 
                func->descr.functionDescr->retLabel = nextLabel();
                genCodeLabel1(func->descr.functionDescr->entLabel, "ENFN", getCurrentLevel());
        }

        processLabels (p->comps[3]->comps[0]);
        processTypes (p->comps[3]->comps[1]);
        currentDispl = processVariables (p->comps[3]->comps[2]);

        if (currentDispl > 0)
                genCode1("ALOC", currentDispl);

        //loadFormalsSymbolTable(formals);
        insertSymbolTable(formals);

        processFunctions(p->comps[3]->comps[3]);
        setSavedState(func);

        processStatements(p->comps[3]->comps[4], currentDispl);

        if (!ismain)
                genCodeLabel(func->descr.functionDescr->retLabel);

        if (currentDispl > 0)
                genCode1("DLOC", currentDispl);

        if (ismain) 
                genCode0("STOP");
        else if (func->descr.functionDescr->result != NULL)
                genCode1("RTRN", -lastDispl - (4 + func->descr.functionDescr->result->size));
        else
                genCode1("RTRN", -lastDispl - 4);

        decrCurrentLevel();

        restoreSymbTable(func);
}

void processLabels (TreeNodePtr p) {

        //dumpTree(p, 4);

        if (p->categ == C_EMPTY)
                return;

        TreeNodePtr invertedLabelList = invertList(p->comps[0]->comps[0]);

        while (invertedLabelList->categ != C_EMPTY) {

                int newLabel = nextLabel();

                SymbEntryPtr newLabelEntry = newSymbEntry(S_LABEL, invertedLabelList->str);
                newLabelEntry->descr.labelDescr = (LabelDescPtr) malloc (sizeof (LabelDesc));
                newLabelEntry->descr.labelDescr->label = invertedLabelList->str;
                newLabelEntry->descr.labelDescr->defined = newLabel;

                insertSymbolTable(newLabelEntry);

                invertedLabelList = invertedLabelList->next;
        } 
}

void processTypes (TreeNodePtr p) {
}

int processVariables (TreeNodePtr p) {

        if (p->categ == C_EMPTY)
                return 0;

        int count = 0;

        //dumpNode(p, 0);

        TreeNodePtr invertedList = invertList(p->comps[0]);

        while (invertedList->categ != C_EMPTY) {
                //dumpNode(invertedList, 0);
                processVarDecl(invertedList, &count);
                invertedList = invertedList->next;
        }

        return count;
}

void processVarDecl(TreeNodePtr p, int* currentDispl) {

        //dumpNode(p, 0);

        TreeNodePtr vars = invertList(p->comps[0]->comps[0]),
                    types = invertList(p->comps[1]->comps[0]); /* Can be an array: in this case, 
                                                                  dimensions are in the next nodes. */

        TypeDescrPtr variable_type = determineType(types);

        while (vars->categ != C_EMPTY) {

                SymbEntryPtr ste = newSymbEntry(S_VARIABLE, vars->str);
                ste->level = getCurrentLevel(); 
                ste->descr.variableDescr = (VariableDescPtr) malloc (sizeof (VariableDesc));
                ste->descr.variableDescr->displ = *currentDispl;
                ste->descr.variableDescr->type = variable_type;

                insertSymbolTable(ste); 

                *currentDispl = *currentDispl + variable_type->size;

                vars = vars->next;
        }
}

void processFunctions (TreeNodePtr p) {

        if ( p->categ == C_EMPTY)
                return;

        TreeNodePtr invertedFunctionList = invertList (p->comps[0]);

        int bodyLabel = nextLabel();
        genLabel1 ("JUMP", bodyLabel);

        while (invertedFunctionList->categ != C_EMPTY) {

                processFuncDecl (invertedFunctionList, 0);                 
                invertedFunctionList = invertedFunctionList->next;
        }

        genCodeLabel(bodyLabel);
}

void processStatements (TreeNodePtr p, int variableDispl) {

        if (p->categ == C_EMPTY)
                return;

        TreeNodePtr statements = invertList(p->comps[0]);
        statements = statements->next; /* ignores empty node */

        SymbEntryPtr labelEntry;

        while (statements != NULL) {

                if (statements->comps[0]->categ == C_IDENT) {
                        labelEntry = searchId (statements->comps[0]->str);
                        if (labelEntry == NULL || labelEntry->categ != S_LABEL) {
                                printf ("2321312\n");
                                SemanticError(NULL);
                        }

                        genCodeLabel2(labelEntry->descr.labelDescr->defined, "ENLB", labelEntry->level, variableDispl);
                        processUnlabeledStatements (statements->comps[1]);
                }
                else 
                        processUnlabeledStatements (statements->comps[0]);

                statements = statements->next;
        }

}

void processUnlabeledStatements (TreeNodePtr unlabeledStatement) {

        switch (unlabeledStatement->categ) {

                case C_ASSIGN:
                        processAssignment (unlabeledStatement);
                        break;
                case C_FUNCTION_CALL_STATEMENT:
                        processFunctionCall (unlabeledStatement->comps[0]);
                        break;
                case C_GOTO:
                        processGoto (unlabeledStatement);
                        break;
                case C_RETURN:
                        processReturn (unlabeledStatement);
                        break;
                case C_COMPOUND:
                        processCompound (unlabeledStatement);
                        break;
                case C_IF:
                        processConditional (unlabeledStatement);
                        break;
                case C_WHILE:
                        processLoop (unlabeledStatement);
                        break;

        }

} 

void processAssignment (TreeNodePtr p) {

        //dumpTree(p, 4);

        TreeNodePtr invertedVariableList = invertList (p->comps[0]);

        //dumpTree(invertedVariableList, 0);

        //dumpSymbolTable();

        SymbEntryPtr variableEntry = searchId (invertedVariableList->comps[0]->str);

        if (variableEntry == NULL)
                SemanticError(NULL);                

        TypeDescrPtr exprType = processExpr (p->comps[1]);

        //if (variableEntry->descr.variableDescr->type != exprType) {
        if (!compatibleType(variableEntry->descr.variableDescr->type, exprType)) {
                
                //printf ("dasdasda41232131 %x %x %x %x\n", INTEGER, BOOLEAN, variableEntry->descr.variableDescr->type,  exprType);
                SemanticError(NULL);
        }

        if (variableEntry->descr.variableDescr->type->constr != T_ARRAY) {
                genCode2 ("STVL", variableEntry->level, variableEntry->descr.variableDescr->displ);
        }
        else { /* ARRAYs */
        }

}

TypeDescrPtr processFunctionCall (TreeNodePtr functionCall) {

        //dumpTree(functionCall, 0);

        TreeNodePtr identifier = functionCall->comps[0],
                    expressionList = functionCall->comps[1],
                    invertedExpressionList = invertList (expressionList->comps[0]);

        SymbEntryPtr functionEntry = getFunction(identifier->str),
                     functionParams = functionEntry->descr.functionDescr->params;

        if (functionEntry != READ_FUNCTION && functionEntry->descr.functionDescr->result != NULL)
                genCode1("ALOC", functionEntry->descr.functionDescr->result->size);

        while (invertedExpressionList->categ != C_EMPTY) {

                if (functionEntry == WRITE_FUNCTION) {
                        TypeDescrPtr exprType = processExpr(invertedExpressionList);
                        genCode0 ("PRNT");
                }
                else if (functionEntry == READ_FUNCTION) {
                        genCode0 ("READ");
                        readInstr = 1;
                        TypeDescrPtr exprType = processExpr(invertedExpressionList);
                        readInstr = 0;
                }
                else {

                        TypeDescrPtr exprType = processExpr(invertedExpressionList);

                        if (functionParams == NULL) /* The function requires less parameters */
                                SemanticError(NULL);

                        if (!compatibleType(exprType, functionParams->descr.paramDescr->type)) {
                                SemanticError(NULL);
                        }

                        functionParams = functionParams->next;

                }

                invertedExpressionList = invertedExpressionList->next;
        }

        if (functionEntry != READ_FUNCTION && functionEntry != WRITE_FUNCTION) {
                if (functionParams != NULL && functionParams->categ == S_PARAMETER) { /* The function requires more parameters */
                        printf ("CZXC \n");
                        SemanticError(NULL);
                }

                 genLabel2 ("CFUN", functionEntry->descr.functionDescr->entLabel, getCurrentLevel());

        }

        return functionEntry->descr.functionDescr->result;

}

void processGoto (TreeNodePtr p) {

        SymbEntryPtr labelEntry = searchId (p->comps[0]->str);

        if (labelEntry == NULL || labelEntry->categ != S_LABEL)
                SemanticError(NULL);

        genLabel1 ("JUMP", labelEntry->descr.labelDescr->defined);
        
}

void processReturn (TreeNodePtr p) {

        SymbEntryPtr funcEntry = getSavedState();
        int result = 0;

        if (funcEntry == NULL || funcEntry->categ != S_FUNCTION) {
                printf ("fadasdas\n");
                SemanticError(NULL);
        }

        processExpr (p->comps[0]);

        if (funcEntry->descr.functionDescr->result != NULL) {
                
                if (funcEntry->descr.functionDescr->result->constr != T_ARRAY)
                        genCode2 ("STVL", getCurrentLevel(), funcEntry->descr.functionDescr->displ);
        }
   
        genLabel1 ("JUMP", funcEntry->descr.functionDescr->retLabel);
}

void processConditional (TreeNodePtr p) {

        TypeDescrPtr condition = processExpr (p->comps[0]);

        if (!compatibleType(condition, BOOLEAN))
                SemanticError(NULL);

        int elseLabel = nextLabel (), endLabel;

        if (p->comps[2]->categ != C_EMPTY)
                endLabel = nextLabel ();

        genLabel1 ("JMPF", elseLabel);

        processCompound (p->comps[1]);

        if (p->comps[2]->categ != C_EMPTY)
                genLabel1 ("JUMP", endLabel);

        genCodeLabel(elseLabel);

        if (p->comps[2]->categ != C_EMPTY) {
                processCompound (p->comps[2]);
                genCodeLabel(endLabel);
        }
        
}

void processLoop (TreeNodePtr p) {

        int condLabel = nextLabel(), endLabel = nextLabel();
        
        genCodeLabel(condLabel);
        TypeDescrPtr expr = processExpr (p->comps[0]);

        if (!compatibleType(expr, BOOLEAN))
                SemanticError(NULL);

        genLabel1 ("JMPF", endLabel);

        processCompound (p->comps[1]);

        genLabel1 ("JUMP", condLabel);

        genCodeLabel(endLabel);
}

void processCompound (TreeNodePtr p) {

        //dumpTree(p, 0);

        TreeNodePtr invertedCompoundList = invertList (p->comps[0]);

        while (invertedCompoundList != NULL) {
                processUnlabeledStatements (invertedCompoundList);
                invertedCompoundList = invertedCompoundList->next;
        }

}

TypeDescrPtr processExpr(TreeNodePtr p) {
        
        //dumpTree(p, 0);

        if (p->categ == C_EMPTY)
                return NULL;

        TypeDescrPtr simpleExpr = processSimpleExpr (p->comps[0]);

        if (p->comps[1]->categ != C_EMPTY) {

                TypeDescrPtr simpleExpr2 = processSimpleExpr (p->comps[1]->comps[1]);

                if (!compatibleType(simpleExpr, simpleExpr2)) {
                        printf ("78787\n");
                        SemanticError(NULL);
                }

                char* op = p->comps[1]->comps[0]->str;

                if (strcmp(op, "==") == 0)
                        genCode0("EQUA");
                else if (strcmp(op, "!=") == 0)
                        genCode0("DIFF");
                else if (strcmp(op, "<") == 0)
                        genCode0("LESS");
                else if (strcmp(op, "<=") == 0)
                        genCode0("LEQU");
                else if (strcmp(op, ">") == 0)
                        genCode0("GRTR");
                else genCode0("GEQU");

                return BOOLEAN;
        }

        return simpleExpr;
}

TypeDescrPtr processSimpleExpr(TreeNodePtr p) {

        TreeNodePtr unarySimpleOp = p->comps[0],
                    invertedTermList = invertList(p->comps[1]);
        TypeDescrPtr result = NULL, additiveResult;

        result = processTerm(invertedTermList);
        invertedTermList = invertedTermList->next;

        if (unarySimpleOp->categ != C_EMPTY) {

                if (!compatibleType(result, INTEGER))
                        SemanticError(NULL);

                if (strcmp(unarySimpleOp->str, "-") == 0)
                        genCode0("NEGT");
        }

        while (invertedTermList->categ != C_EMPTY) {

                if (invertedTermList->categ == C_ADDITIVE_OPERATOR_TERM) {
                        additiveResult = processAdditiveTerm(invertedTermList);
                        if (!compatibleType(additiveResult, result))
                                SemanticError(NULL);
                }

                invertedTermList = invertedTermList->next;
        }

        return result;
}

TypeDescrPtr processTerm (TreeNodePtr p) {

        TreeNodePtr invertedFactorList = invertList (p->comps[0]);
        TypeDescrPtr result = NULL;

        while (invertedFactorList->categ != C_EMPTY) {

                if (invertedFactorList->categ == C_FACTOR)
                        result = processFactor(invertedFactorList);
                else if (invertedFactorList->categ == C_MULTICATIVE_OPERATOR_FACTOR) {
                        processMultiplicativeFactor (invertedFactorList);
                }

                invertedFactorList = invertedFactorList->next;
        }

        return result;

}

TypeDescrPtr processMultiplicativeFactor(TreeNodePtr p) {

        TypeDescrPtr argument2 = processFactor (p->comps[1]);

        char* op = p->comps[0]->str;

        if (strcmp(op, "*") == 0)
                genCode0("MULT");
        else if (strcmp(op, "/") == 0)
                genCode0("DIVI");
        else if (strcmp(op, "&&") == 0)
                genCode0("LAND");

        return argument2;

}

TypeDescrPtr processAdditiveTerm (TreeNodePtr p) {

        TypeDescrPtr argument2 = processTerm (p->comps[1]);

        char* op = p->comps[0]->str;

        if (strcmp(op, "+") == 0)
                genCode0("ADDD");
        else if (strcmp(op, "-") == 0)
                genCode0("SUBT");
        else if (strcmp(op, "||") == 0)
                genCode0("LORR");

        return argument2;

}

TypeDescrPtr processFactor (TreeNodePtr p) {

        //dumpTree(p, 0);

        SymbEntryPtr variableEntry;
        TreeNodePtr invertedVariableList, invertedFactorList = invertList (p->comps[0]);
        TypeDescrPtr aux;

        //dumpTree(invertedFactorList, 0);

        switch (invertedFactorList->categ) {
                case C_INTEGER:
                        genCode1 ("LDCT", strtol(p->comps[0]->str, NULL, 10));
                        return INTEGER;

                case C_VARIABLE:

                        variableEntry = searchId (invertedFactorList->comps[0]->str);

                        if (variableEntry->categ == S_CONST) {

                                if (variableEntry == FALSE || variableEntry == TRUE) {
                                        genCode1 ("LDCT", variableEntry == FALSE ? 0 : 1);
                                        return BOOLEAN;
                                }
                        }
                        else if (variableEntry->categ == S_VARIABLE) {
                                if (variableEntry->descr.variableDescr->type->constr != T_ARRAY) {
                                        genCode2 (readInstr ? "STVL" : "LDVL", variableEntry->level, variableEntry->descr.variableDescr->displ);
                                }
                                else { /* ARRAY */
                                }
                        }
                        else if (variableEntry->categ == S_PARAMETER) {
                                if (variableEntry->descr.paramDescr->type->constr != T_ARRAY) {
                                        Passage pass_method = variableEntry->descr.paramDescr->pass;
                                        genCode2 (pass_method == P_VALUE ? "LDVL" : "LADR", variableEntry->level, variableEntry->descr.paramDescr->displ);
                                }
                                else { /* ARRAY */
                                }                                
                        }

                        
                        return variableEntry->descr.variableDescr->type;

                case C_EXPRESSION:

                        return processExpr (invertedFactorList);

                case C_FACTOR:

                        aux = processFactor (invertedFactorList);

                        /* verify if it is bool */

                        genCode0 ("LNOT");

                        return aux;

                case C_FUNCTION_CALL:

                        return processFunctionCall (invertedFactorList);

        }

}

SymbEntryPtr processFormals(TreeNodePtr p, int* lastDispl, int* count) {

        /* p's category should be C_FORMAL_PARAMETERS */

        TreeNodePtr invertedList = invertList(p->comps[0]);

        SymbEntryPtr formalParameterList = NULL, aux;
        *count = 0;

        while (invertedList->categ != C_EMPTY) {

                SymbEntryPtr formalParameter;
                Passage pass_method;

                switch (invertedList->categ) {

                        case C_EXPRESSION_PARAMETER:

                                pass_method = P_VALUE;
                                if (invertedList->comps[2]->categ == C_VAR_MECHANISM)
                                        pass_method = P_VARIABLE;

                                TypeDescrPtr paramType = getType(invertedList->comps[1]);

                                TreeNodePtr invertedIdentList = invertList(invertedList->comps[0]->comps[0]);
                                //TreeNodePtr invertedIdentList = invertList (invertedList->comps[0]->comps[0]->next);

                                while (invertedIdentList->categ != C_EMPTY) {
                                //while (invertedIdentList != NULL) {

                                        formalParameter = newSymbEntry(S_PARAMETER, invertedIdentList->str);
                                        formalParameter->descr.paramDescr = (ParameterDescPtr) malloc (sizeof (ParameterDesc));
                                        formalParameter->descr.paramDescr->pass = pass_method;
                                        formalParameter->descr.paramDescr->type = paramType;

                                        //insertSymbolTable(formalParameter);

                                        *lastDispl = *lastDispl - paramType->size;

                                        if (formalParameterList == NULL) {
                                                formalParameterList = formalParameter;
                                                aux = formalParameter;
                                        }
                                        else {
                                                //TypeDescrPtr

                                                aux->next = formalParameter;
                                                aux = formalParameter;
                                        }

                                        *count = *count + 1;
                                        invertedIdentList = invertedIdentList->next;
                                }

                                break;

                        case C_FUNCTION_PARAMETER:

                                formalParameter = newSymbEntry(S_PARAMETER, invertedList->comps[1]->str);
                                formalParameter->descr.paramDescr->displ = *lastDispl - 1;
                                formalParameter->descr.paramDescr->pass = P_VALUE;

                                TypeDescrPtr functionType = (TypeDescrPtr) malloc (sizeof (TypeDescr));
                                functionType->constr = T_FUNCTION;
                                functionType->size = 3;

                                if (invertedList->comps[0]->categ == C_EMPTY)
                                       functionType->descr.FunctionType.result = NULL;
                                else  
                                       functionType->descr.FunctionType.result = getType(invertedList->comps[0]);

                                int displ = -4, auxCount, i;
                                SymbEntryPtr functionFormals = processFormals (invertedList->comps[2], &displ, &auxCount);

                                functionType->descr.FunctionType.params = functionFormals;
                                formalParameter->descr.paramDescr->type = functionType;

                                *lastDispl = *lastDispl - functionType->size;

                                *count = *count + 1;

                                if (formalParameterList == NULL) {
                                        formalParameterList = formalParameter;
                                        aux = formalParameter;
                                }
                                else {
                                        aux->next = formalParameter;
                                        aux = formalParameter;
                                }

                                break;
                }

                invertedList = invertedList->next;
        }

        aux = formalParameterList;
        int i = 0;
        while (aux != NULL) {
                aux->descr.paramDescr->displ = *lastDispl + i;
                i += aux->descr.paramDescr->type->size;
                aux = aux->next;              
        }

        return formalParameterList;
}

void genCode0 (char* instr) {
        printf ("%s\n", instr);
}

void genCode1 (char* instr, int param) {
        printf ("\t%s %d\n", instr, param);
}

void genLabel1 (char* instr, int label) {
        printf ("\t%s L%d\n", instr, label);
}

void genLabel2 (char* instr, int label, int param) {
        printf ("\t%s L%d,%d\n", instr, label, param);
}

void genCode2 (char* instr, int param1, int param2) {
        printf ("\t%s %d,%d\n", instr, param1, param2);
}

void genCodeLabel(int label) {
        printf ("L%d:\tNOOP\n", label);
}

void genCodeLabel1(int label, char* instr, int param1) {
        printf ("L%d:\t%s %d\n", label, instr, param1);
}

void genCodeLabel2(int label, char* instr, int param1, int param2) {
        printf ("L%d:\t%s %d,%d\n", label, instr, param1, param2);
}
