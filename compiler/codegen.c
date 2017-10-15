#include <stdio.h>
#include <stdlib.h>
#include "codegen.h"

void processProgram(void *p) {

        initSymbolTable();
        dumpNode(p, 0);
        processFuncDecl(((TreeNodePtr) p)->comps[0], 1);
        genCode0("END");
        freeTree(p);
}

int stackHeight() {
        return getStackHeight();
}

void processFuncDecl(TreeNodePtr p, int ismain) {

        dumpNode(p, 0);
        dumpNode(p->comps[1], 0);

        char* fname = p->comps[1]->str;
        TypeDescrPtr resType = NULL, funType;
        int lastDispl = -4, entLabel, retLabel, count, i, currentDispl;

        SymbEntryPtr formals, func, auxFormals;

        incrCurrentLevel();
        if (p->comps[0]->categ != C_EMPTY)
                resType = getType(p->comps[0]);

        formals = processFormals(p->comps[2], &lastDispl, &count);

        /* Convert from SymbEntryPtr to ParamDescrPtr */
        /* ParameterDescPtr formalParamDescr = (ParameterDescPtr) malloc (count * sizeof (ParameterDesc));
        auxFormals = formals;
        i = 0;
        while (auxFormals != NULL) {
                formalParamDescr[i++] = *auxFormals->descr.paramDescr;
                auxFormals = auxFormals->next;
        }*/

        if (resType != NULL)
                lastDispl -= resType->size;

        func = newSymbEntry(S_FUNCTION, fname);
        func->descr.functionDescr = (FunctionDescPtr) malloc (sizeof (FunctionDesc));
        func->descr.functionDescr->result = resType;
        /* func->descr.functionDescr->params = formalParamDescr; */
        func->descr.functionDescr->params = formals;
        func->level = getCurrentLevel() - 1;
        func->descr.functionDescr->displ = lastDispl;

        insertSymbolTable(func);

        dumpSymbolTable ();

        saveSymbTable();

        if (ismain) 
                genCode0("MAIN");
        else {
                entLabel = nextLabel(); 
                retLabel = nextLabel();
                genCodeLabel1(entLabel, "ENFN", getCurrentLevel());
        }

        processLabels (p->comps[3]->comps[0]);
        processTypes (p->comps[3]->comps[1]);
        currentDispl = processVariables (p->comps[3]->comps[2]);

        if (currentDispl > 0)
                genCode1("ALOC", currentDispl);

        loadFormalsSymbolTable(formals);
        processFunctions(p->comps[3]->comps[3]);

        processStatements(p->comps[3]->comps[4]);

        if (!ismain)
                genCodeLabel(retLabel);

        if (currentDispl > 0)
                genCode1("DLOC", currentDispl);

        if (ismain) 
                genCode0("STOP");
        else 
                genCode1("RTRN", -lastDispl - 4);

        decrCurrentLevel();

        restoreSymbTable();
}

void processLabels (TreeNodePtr p) {
}

void processTypes (TreeNodePtr p) {
}

int processVariables (TreeNodePtr p) {

        if (p->categ == C_EMPTY)
                return 0;

        int count = 0;

        dumpNode(p, 0);

        TreeNodePtr invertedList = invertList(p->comps[0]);

        while (invertedList->categ != C_EMPTY) {
                dumpNode(invertedList, 0);
                processVarDecl(invertedList, &count);
                invertedList = invertedList->next;
        }

        return count;
}

void processVarDecl(TreeNodePtr p, int* currentDispl) {

        dumpNode(p, 0);

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
}

void processStatements (TreeNodePtr p) {

        if (p->categ == C_EMPTY)
                return;

        dumpNode(p, 0);
        
        TreeNodePtr statements = invertList(p->comps[0]);
        statements = statements->next; /* ignores empty node */

        dumpTree(statements, 1);

        while (statements != NULL) {

                switch (statements->comps[0]->categ) {

                        case C_IDENT:
                                break;
                        case C_ASSIGN:
                                break;
                        case C_FUNCTION_CALL_STATEMENT:
                                processFunctionCall (statements->comps[0]);
                                break;
                        case C_GOTO:
                                break;
                        case C_RETURN:
                                break;
                        case C_COMPOUND:
                                break;
                        case C_IF:
                                break;
                        case C_WHILE:
                                break;

                }

                statements = statements->next;
        }

}

void processFunctionCall (TreeNodePtr p) {

        dumpTree(p, 1);

        TreeNodePtr functionCall = p->comps[0],
                    identifier = functionCall->comps[0],
                    expressionList = functionCall->comps[1],
                    invertedExpressionList = invertList (expressionList->comps[0]);

        dumpTree(invertedExpressionList, 0);

        SymbEntryPtr functionEntry = getFunction(identifier->str),
                     functionParams = functionEntry->descr.functionDescr->params;

        if (functionEntry->descr.functionDescr->result != NULL)
                genCode1("ALOC", functionEntry->descr.functionDescr->result->size);

        while (invertedExpressionList->categ != C_EMPTY) {

                if (functionParams == NULL) /* The function requires less parameters */
                        SemanticError(NULL);

                TypeDescrPtr exprType = processExpr(invertedExpressionList);

                if (exprType != functionParams->descr.paramDescr->type)
                        SemanticError(NULL);

                functionParams = functionParams->next;
                invertedExpressionList = invertedExpressionList->next;
        }

        if (functionParams != NULL) /* The function requires more parameters */
                SemanticError(NULL);

        if (functionEntry == WRITE_FUNCTION)
                genCode0 ("PRNT");
        else if (functionEntry == READ_FUNCTION)
                genCode0 ("READ");
        else {
        }

}

TypeDescrPtr processExpr(TreeNodePtr p) {
        
        dumpTree(p, 1);
        TypeDescrPtr simpleExpr = processSimpleExpr (p->comps[0]);
}

TypeDescrPtr processSimpleExpr(TreeNodePtr p) {

        dumpTree(p, 2);
        TreeNodePtr unarySimpleOp = p->comps[0],
                    invertedTermList = invertList(p->comps[1]);
        TypeDescrPtr result = NULL;

        while (invertedTermList->categ != C_EMPTY) {

                if (invertedTermList->categ == C_TERM)
                        result = processTerm(invertedTermList);
                else if (invertedTermList->categ == C_ADDITIVE_OPERATOR_TERM) 
                        processAdditiveTerm(invertedTermList);

                invertedTermList = invertedTermList->next;
        }

        if (unarySimpleOp->categ != C_EMPTY) {
                /* Do something */
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
                }

                invertedFactorList = invertedFactorList->next;
        }

        return result;

}

TypeDescrPtr processAdditiveTerm (TreeNodePtr p) {

}

TypeDescrPtr processFactor (TreeNodePtr p) {

        dumpTree(p, 1);

        SymbEntryPtr variableEntry;
        TreeNodePtr invertedVariableList;

        switch (p->comps[0]->categ) {
                case C_INTEGER:
                        genCode1 ("LDCT", strtol(p->comps[0]->str, NULL, 10));
                        return INTEGER;

                case C_VARIABLE:

                        invertedVariableList = invertList (p->comps[0]->comps[0]);
                        variableEntry = getVariable (invertedVariableList->str);

                        if (variableEntry->descr.variableDescr->type->constr != T_ARRAY) {
                                genCode2 ("LDVL", variableEntry->level, variableEntry->descr.variableDescr->displ);
                        }
                        else {
                        }

                        return variableEntry->descr.variableDescr->type;

        }

}

SymbEntryPtr processFormals(TreeNodePtr p, int* lastDispl, int* count) {

        /* p's category should be C_FORMAL_PARAMETERS */

        dumpNode(p, 0);

        TreeNodePtr invertedList = invertList(p->comps[0]);

        SymbEntryPtr formalParameterList = NULL, aux;
        *count = 0;

        while (invertedList->categ != C_EMPTY) {

                dumpNode(invertedList, 0);

                SymbEntryPtr formalParameter;
                Passage pass_method;

                switch (invertedList->categ) {

                        case C_EXPRESSION_PARAMETER:

                                pass_method = P_VALUE;
                                if (invertedList->comps[0]->categ == C_VAR_MECHANISM)
                                        pass_method = P_VARIABLE;

                                TypeDescrPtr paramType = getType(p->comps[2]);

                                TreeNodePtr invertedIdentList = invertList(invertedList->comps[1]->comps[0]);

                                while (invertedIdentList->categ != C_EMPTY) {
                                        formalParameter = newSymbEntry(S_PARAMETER, invertedIdentList->str);
                                        formalParameter->descr.paramDescr->displ = *lastDispl - 1;
                                        formalParameter->descr.paramDescr->pass = pass_method;
                                        formalParameter->descr.paramDescr->type = paramType;

                                        *lastDispl = *lastDispl - paramType->size;

                                        if (formalParameterList == NULL) {
                                                formalParameterList = formalParameter;
                                                aux = formalParameter;
                                        }
                                        else {
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

                                /* Convert from SymbEntryPtr to ParamDescrPtr */
                                /* ParameterDescPtr formalParamDescr = (ParameterDescPtr) malloc (auxCount * sizeof (ParameterDesc));
                                i = 0;
                                while (functionFormals != NULL) {
                                        formalParamDescr[i++] = *functionFormals->descr.paramDescr;
                                        functionFormals = functionFormals->next;
                                }

                                functionType->descr.FunctionType.params = formalParamDescr; */

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

        return formalParameterList;
}

void genCode0 (char* instr) {
        printf ("\t%s\n", instr);
}

void genCode1 (char* instr, int param) {
        printf ("\t%s %d\n", instr, param);
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
