#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "codegen.h"

int readInstr = 0;
Passage referenceParameter;

/* Needed by the main function. Returns tree's stack height. */
int stackHeight() {
        return getStackHeight();
}

/* Processes the root node of the tree */
void processProgram(void *p) {

        initSymbolTable();
        processFuncDecl(((TreeNodePtr) p)->comps[0], 1);
        genCode0("END ", "");
        freeSymbolTable();
}

/* Processes function declaration nodes */
void processFuncDecl(TreeNodePtr p, int ismain) {

        char* fname = p->comps[1]->str;
        TypeDescrPtr resType = NULL, funType;
        int lastDispl = -4, i, currentDispl;

        SymbEntryPtr formals, func, auxFormals;

        /* Increases code static level */
        incrCurrentLevel();

        if (p->comps[0]->categ != C_EMPTY)
                resType = getType(p->comps[0]->str);

        /* Computes function parameters */
        formals = processFormals(p->comps[2], &lastDispl);

        if (resType != NULL)
                lastDispl -= resType->size;

        /* Allocates new S_FUNCTION entry */
        func = newSymbEntry(S_FUNCTION, fname);
        func->descr.functionDescr = (FunctionDescPtr) malloc (sizeof (FunctionDesc));
        func->descr.functionDescr->result = resType;
        func->descr.functionDescr->params = formals;
        func->level = getCurrentLevel() - 1;
        func->descr.functionDescr->displ = lastDispl;

        insertSymbolTable(func);

        /* Saves symbol table state before appending the formal parameters */
        saveSymbTable(func);

        if (ismain) 
                genCode0("MAIN", "");
        else {
                func->descr.functionDescr->entLabel = nextLabel(); 
                func->descr.functionDescr->retLabel = nextLabel();
                genCodeLabel1(func->descr.functionDescr->entLabel, "ENFN", getCurrentLevel(), fname);
        }

        processLabels (p->comps[3]->comps[0]);

        processTypes (p->comps[3]->comps[1]);

        /* Computes the number of variables for this functions */
        currentDispl = processVariables (p->comps[3]->comps[2]);

        if (currentDispl > 0)
                genCode1("ALOC", currentDispl, "");

        insertSymbolTable(formals);

        processFunctions(p->comps[3]->comps[3]);

        /* Restores saved symbol table state. This is needed in the case of nested function declarations */
        saveSymbTable(func);

        processStatements(p->comps[3]->comps[4], currentDispl);

        if (!ismain)
                genCodeLabel(func->descr.functionDescr->retLabel, "");

        if (currentDispl > 0)
                genCode1("DLOC", currentDispl, "");

        if (ismain) 
                genCode0("STOP", "");
        else if (func->descr.functionDescr->result != NULL)
                genCode1("RTRN", -lastDispl - (4 + func->descr.functionDescr->result->size), "        end function");
        else
                genCode1("RTRN", -lastDispl - 4, "        end function");

        /* Frees allocated memory for the used nodes */
        freeNode (p->comps[0]);
        freeNode (p->comps[1]);
        freeNode (p->comps[2]);
        freeNode (p->comps[3]);

        /* Decreases code static level */
        decrCurrentLevel();

        /* Removes all entries after the func entry */
        restoreSymbTable(func);
}

/* Processes declared labels */
void processLabels (TreeNodePtr p) {

        if (p->categ == C_EMPTY) {
                freeNode (p);
                return;
        }

        TreeNodePtr invertedLabelList = invertList(p->comps[0]->comps[0]), aux;

        while (invertedLabelList->categ != C_EMPTY) {

                /* Gets and increases the label count */
                int newLabel = nextLabel();

                SymbEntryPtr newLabelEntry = newSymbEntry(S_LABEL, invertedLabelList->str);
                newLabelEntry->descr.labelDescr = (LabelDescPtr) malloc (sizeof (LabelDesc));
                newLabelEntry->descr.labelDescr->label = invertedLabelList->str;
                newLabelEntry->descr.labelDescr->defined = newLabel;

                /* Inserts label into the symbol table */
                insertSymbolTable(newLabelEntry);

                aux = invertedLabelList->next;
                freeNode (invertedLabelList);
                invertedLabelList = aux;
        }

        freeNode (invertedLabelList);
        freeNode (p->comps[0]);
        freeNode (p);
}

/* Processes declared types */
void processTypes (TreeNodePtr p) {

        if (p->categ == C_EMPTY) {
                freeNode (p);
                return ;
        }

        TreeNodePtr invertedTypesList = invertList (p->comps[0]), aux;

        while (invertedTypesList->categ != C_EMPTY) {

                char *typeName = invertedTypesList->comps[0]->str;
                SymbEntryPtr typeEntry = searchId (typeName);

                if (typeEntry != NULL)
                        SemanticError(NULL);

                SymbEntryPtr newTypeEntry = newSymbEntry(S_TYPE, typeName);

                /* Gets list for multidimensional types. Returns a single node with the base type */
                TreeNodePtr invertedArrayList = invertList (invertedTypesList->comps[1]->comps[0]);

                newTypeEntry->descr.typeDescr = (TypeDescrPtr) malloc (sizeof (TypeDescr));

                /* Checks if it's an array type */
                newTypeEntry->descr.typeDescr = determineType(invertedArrayList);

                if (newTypeEntry->descr.typeDescr == NULL)
                        SemanticError(NULL);

                /* Inserts new type into the symbol table */
                insertSymbolTable(newTypeEntry);

                aux = invertedTypesList->next;
                freeNode (invertedTypesList);
                invertedTypesList = aux;                       
        }

        freeNode (invertedTypesList);
        freeNode (p);
}

/* Processes declared variables */
int processVariables (TreeNodePtr p) {

        if (p->categ == C_EMPTY) {
                freeNode (p);
                return 0;
        }

        int count = 0;

        TreeNodePtr invertedList = invertList(p->comps[0]), aux;

        while (invertedList->categ != C_EMPTY) {

                processVarDecl(invertedList, &count);

                aux = invertedList->next;
                freeNode (invertedList); 
                invertedList = aux;
        }

        freeNode (invertedList); 
        freeNode (p);

        return count;
}

/* Processes variable declarations */
void processVarDecl(TreeNodePtr p, int* currentDispl) {

        TreeNodePtr vars = invertList(p->comps[0]->comps[0]),
                    types = invertList(p->comps[1]->comps[0]), // Can be an array: in this case, 
                    aux;                                       // dimensions are in the next nodes.

        TypeDescrPtr variable_type = determineType(types);

        while (vars->categ != C_EMPTY) {

                SymbEntryPtr ste = newSymbEntry(S_VARIABLE, vars->str);
                ste->level = getCurrentLevel(); 
                ste->descr.variableDescr = (VariableDescPtr) malloc (sizeof (VariableDesc));
                ste->descr.variableDescr->displ = *currentDispl;
                ste->descr.variableDescr->type = variable_type;

                insertSymbolTable(ste); 

                *currentDispl = *currentDispl + variable_type->size;

                aux = vars->next;
                freeNode (vars); 
                vars = aux;
        }

        freeNode (vars); 
}

/* Processes function declarations */
void processFunctions (TreeNodePtr p) {

        if (p->categ == C_EMPTY) {
                freeNode (p);
                return;
        }

        TreeNodePtr invertedFunctionList = invertList (p->comps[0]), aux;

        int bodyLabel = nextLabel();
        genLabel1 ("JUMP", bodyLabel, "");

        while (invertedFunctionList->categ != C_EMPTY) {

                /* Second parameter is ismain and is false */
                processFuncDecl (invertedFunctionList, 0);

                aux = invertedFunctionList->next;
                freeNode (invertedFunctionList);
                invertedFunctionList = aux;
        }

        genCodeLabel(bodyLabel, "             body");

        freeNode (invertedFunctionList);
        freeNode (p);
}

/* Processes statements */
void processStatements (TreeNodePtr p, int variableDispl) {

        char comment[20];

        if (p->categ == C_EMPTY) {
                freeNode (p);
                return;
        }

        TreeNodePtr statements = invertList(p->comps[0]), aux; /* ignores empty node */
        
        aux = statements->next; /* ignores empty node */
        freeNode (statements);
        statements = aux; 

        SymbEntryPtr labelEntry;

        while (statements != NULL) {

                /* A statement can be either a C_IDENT node or a unlabeled statement */
                if (statements->comps[0]->categ == C_IDENT) {
                        labelEntry = searchId (statements->comps[0]->str);
                        if (labelEntry == NULL || labelEntry->categ != S_LABEL) {
                                SemanticError(NULL);
                        }

                        sprintf (comment, "%s:", statements->comps[0]->str);
                        genCodeLabel2(labelEntry->descr.labelDescr->defined, "ENLB", labelEntry->level, variableDispl, comment);
                        processUnlabeledStatements (statements->comps[1]);
                }
                else 
                        processUnlabeledStatements (statements->comps[0]);

                aux = statements->next;
                freeNode (statements->comps[0]);
                freeNode (statements->comps[1]);
                freeNode (statements);
                statements = aux;
        }

        freeNode (p);
}

/* Process a unlabeled statement */
void processUnlabeledStatements (TreeNodePtr unlabeledStatement) {

        switch (unlabeledStatement->categ) {

                case C_ASSIGN:
                        processAssignment (unlabeledStatement);
                        break;
                case C_FUNCTION_CALL_STATEMENT:
                        processFunctionCall (unlabeledStatement->comps[0]);
                        freeNode (unlabeledStatement->comps[0]);
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

/* Process an assignment */
void processAssignment (TreeNodePtr p) {

        TreeNodePtr invertedVariableList = invertList (p->comps[0]);
        TypeDescrPtr exprType, varType;
        int isArray = 0;

        /* Gets leftmost variable's symbol table entry */
        SymbEntryPtr variableEntry = searchId (invertedVariableList->comps[0]->str);

        if (variableEntry == NULL)
                SemanticError(NULL);                

        /* Retrieves correct type for it */
        switch  (variableEntry->categ) {

                case S_VARIABLE:
                        varType = variableEntry->descr.variableDescr->type;
                        break;
                case S_PARAMETER:
                        varType = variableEntry->descr.paramDescr->type;
                        break;
        }

        /* Handles array types */
        if (varType->constr == T_ARRAY) {

                isArray = 1;

                if (variableEntry->categ == S_VARIABLE)
                        genCode2 ("LADR", variableEntry->level, variableEntry->descr.variableDescr->displ, "");
                else 
                        genCode2 ("LDVL", variableEntry->level, variableEntry->descr.variableDescr->displ, "");

                varType = processArray (invertedVariableList, varType);
        }

        exprType = processExpr (p->comps[1]);

        if (!compatibleType(varType, exprType)) {
                SemanticError(NULL);
        }

        if (!isArray) {
                genCode2 ("STVL", variableEntry->level, variableEntry->descr.variableDescr->displ, "");
        }
        else { /* ARRAYs */
                genCode1 ("STMV", varType->size, "");
        }

        freeNode (invertedVariableList);
        freeNode (p->comps[1]);
}

/* Processes a function call statement */
TypeDescrPtr processFunctionCall (TreeNodePtr functionCall) {

        TreeNodePtr identifier = functionCall->comps[0],
                    expressionList = functionCall->comps[1],
                    invertedExpressionList = invertList (expressionList->comps[0]),
                    aux;

        SymbEntryPtr functionEntry = searchId(identifier->str),
                     functionParams;

        TypeDescrPtr result;

        /* Checks if the symbol table entry is a S_FUNCTION or a function parameter */
        if (functionEntry->categ == S_FUNCTION) {
                functionParams = functionEntry->descr.functionDescr->params;
                result = functionEntry->descr.functionDescr->result;
        }
        else {
                functionParams = functionEntry->descr.paramDescr->type->descr.FunctionType.params;
                result = functionEntry->descr.paramDescr->type->descr.FunctionType.result;
        }

        /* Allocates space for the return value */
        if (functionEntry != READ_FUNCTION && result != NULL)
                genCode1("ALOC", result->size, "        result");

        while (invertedExpressionList->categ != C_EMPTY) {

                /* Two special cases: write and read functions */
                if (functionEntry == WRITE_FUNCTION) {

                        referenceParameter = P_VALUE;

                        TypeDescrPtr exprType = processExpr(invertedExpressionList);
                        genCode0 ("PRNT", "");
                }
                else if (functionEntry == READ_FUNCTION) {

                        referenceParameter = P_VALUE;

                        genCode0 ("READ", "");
                        readInstr = 1;
                        TypeDescrPtr exprType = processExpr(invertedExpressionList);
                        readInstr = 0;
                }
                else {

                        referenceParameter = functionParams->descr.paramDescr->pass;

                        TypeDescrPtr exprType = processExpr(invertedExpressionList);

                        if (functionParams == NULL) /* The function requires less parameters */
                                SemanticError(NULL);

                        if (!compatibleType(exprType, functionParams->descr.paramDescr->type)) {
                                SemanticError(NULL);
                        }

                        functionParams = functionParams->next;

                }

                aux = invertedExpressionList->next;
                freeNode (invertedExpressionList);
                invertedExpressionList = aux;
        }

        if (functionEntry != READ_FUNCTION && functionEntry != WRITE_FUNCTION) {

                /* Checks for compatibility issues */
                if (functionParams != NULL && functionParams->categ == S_PARAMETER) { /* The function requires more parameters */
                        SemanticError(NULL);
                }

                /* Checks the symbol table entry category and generates the right calling instruction */
                if (functionEntry->categ == S_FUNCTION) {
                        genLabel2 ("CFUN", functionEntry->descr.functionDescr->entLabel, getCurrentLevel(), "");
                }
                else {
                        genCode3 ("CPFN", functionEntry->level, functionEntry->descr.paramDescr->displ, getCurrentLevel(), "");
                }
        }

        freeNode (invertedExpressionList);
        freeNode (functionCall->comps[0]);
        freeNode (functionCall->comps[1]);

        return result;

}

/* Process a goto statement */
void processGoto (TreeNodePtr p) {

        char comment [20];

        SymbEntryPtr labelEntry = searchId (p->comps[0]->str);

        if (labelEntry == NULL || labelEntry->categ != S_LABEL)
                SemanticError(NULL);

        sprintf (comment, "%*cgoto %s", 9, ' ', p->comps[0]->str);
        genLabel1 ("JUMP", labelEntry->descr.labelDescr->defined, comment);
        
        freeNode (p->comps[0]);
}

/* Process a return statement */
void processReturn (TreeNodePtr p) {

        /* Gets the function which is currently being processed */
        SymbEntryPtr funcEntry = getSavedState();
        int result = 0;

        if (funcEntry == NULL || funcEntry->categ != S_FUNCTION) {
                SemanticError(NULL);
        }

        processExpr (p->comps[0]);

        if (funcEntry->descr.functionDescr->result != NULL) {
                
                if (funcEntry->descr.functionDescr->result->constr != T_ARRAY)
                        genCode2 ("STVL", getCurrentLevel(), funcEntry->descr.functionDescr->displ, "");
        }
   
        genLabel1 ("JUMP", funcEntry->descr.functionDescr->retLabel, "");

        freeNode (p->comps[0]);
}

/* Process an if ... else ... statement */
void processConditional (TreeNodePtr p) {

        char comment[20];
        TypeDescrPtr condition = processExpr (p->comps[0]);

        if (!compatibleType(condition, BOOLEAN))
                SemanticError(NULL);

        int elseLabel = nextLabel (), endLabel;

        /* Checks if an else part exists */
        if (p->comps[2]->categ != C_EMPTY)
                endLabel = nextLabel ();

        sprintf (comment, "%10s", "if");
        genLabel1 ("JMPF", elseLabel, comment);

        processCompound (p->comps[1]);

        /* Checks if an else part exists */
        if (p->comps[2]->categ != C_EMPTY) {
                genLabel1 ("JUMP", endLabel, "");
                sprintf (comment, "%17s", "else");
        }
        else
                sprintf (comment, "%19s", "end if");

        
        genCodeLabel(elseLabel, comment);

        if (p->comps[2]->categ != C_EMPTY) {
                sprintf (comment, "%19s", "end if");
                processCompound (p->comps[2]);
                genCodeLabel(endLabel, comment);
        }
        
        freeNode (p->comps[0]);
        freeNode (p->comps[1]);
        freeNode (p->comps[2]);
}

/* Process a while statement */
void processLoop (TreeNodePtr p) {

        char comment[25];
        int condLabel = nextLabel(), endLabel = nextLabel();

        sprintf (comment, "%18s", "while");        
        genCodeLabel(condLabel, comment);
        TypeDescrPtr expr = processExpr (p->comps[0]);

        if (!compatibleType(expr, BOOLEAN))
                SemanticError(NULL);

        genLabel1 ("JMPF", endLabel, "");

        processCompound (p->comps[1]);

        genLabel1 ("JUMP", condLabel, "");

        sprintf (comment, "%22s", "end while");
        genCodeLabel(endLabel, comment);

        freeNode (p->comps[0]);
        freeNode (p->comps[1]);
}

/* Process compounds */
void processCompound (TreeNodePtr p) {

        TreeNodePtr invertedCompoundList = invertList (p->comps[0]), aux;

        while (invertedCompoundList != NULL) {
                processUnlabeledStatements (invertedCompoundList);

                aux = invertedCompoundList->next;
                freeNode (invertedCompoundList);
                invertedCompoundList = aux;
        }

        freeNode (invertedCompoundList);

}

/* Processes an expression */
TypeDescrPtr processExpr(TreeNodePtr p) {
        
        if (p->categ == C_EMPTY)
                return NULL;

        TypeDescrPtr simpleExpr = processSimpleExpr (p->comps[0]);

        /* Checks if there is a relational operator. In the positive case, generates the right code according to the operator */
        if (p->comps[1]->categ != C_EMPTY) {

                TypeDescrPtr simpleExpr2 = processSimpleExpr (p->comps[1]->comps[1]);

                if (!compatibleType(simpleExpr, simpleExpr2)) {
                        SemanticError(NULL);
                }

                char* op = p->comps[1]->comps[0]->str;

                if (strcmp(op, "==") == 0)
                        genCode0("EQUA", "");
                else if (strcmp(op, "!=") == 0)
                        genCode0("DIFF", "");
                else if (strcmp(op, "<") == 0)
                        genCode0("LESS", "");
                else if (strcmp(op, "<=") == 0)
                        genCode0("LEQU", "");
                else if (strcmp(op, ">") == 0)
                        genCode0("GRTR", "");
                else genCode0("GEQU", "");

                freeNode (p->comps[0]);
                free     (p->comps[1]->comps[0]);
                freeNode (p->comps[1]->comps[1]);
                freeNode (p->comps[1]);

                return BOOLEAN;
        }

        freeNode (p->comps[0]);
        freeNode (p->comps[1]);

        return simpleExpr;
}

/* Processes a simple expression */
TypeDescrPtr processSimpleExpr(TreeNodePtr p) {

        TreeNodePtr unarySimpleOp = p->comps[0],
                    invertedTermList = invertList(p->comps[1]),
                    aux;

        TypeDescrPtr result = NULL, additiveResult;

        result = processTerm(invertedTermList);

        aux = invertedTermList->next;
        freeNode (invertedTermList);
        invertedTermList = aux;

        /* Checks if there exists an unary operator */
        if (unarySimpleOp->categ != C_EMPTY) {

                if (!compatibleType(result, INTEGER))
                        SemanticError(NULL);

                if (strcmp(unarySimpleOp->str, "-") == 0)
                        genCode0("NEGT", "");
        }

        /* Generates codes for additive operators */
        while (invertedTermList->categ != C_EMPTY) {

                if (invertedTermList->categ == C_ADDITIVE_OPERATOR_TERM) {

                        additiveResult = processAdditiveTerm(invertedTermList);
                        if (!compatibleType(additiveResult, result))
                                SemanticError(NULL);
                }

                aux = invertedTermList->next;
                freeNode (invertedTermList);
                invertedTermList = aux;
        }

        free (p->comps[0]); 
        freeNode (invertedTermList);

        return result;
}

/* Processes a term */
TypeDescrPtr processTerm (TreeNodePtr p) {

        TreeNodePtr invertedFactorList = invertList (p->comps[0]), aux;
        TypeDescrPtr result = NULL;

        while (invertedFactorList->categ != C_EMPTY) {

                if (invertedFactorList->categ == C_FACTOR)
                        result = processFactor(invertedFactorList);
                else if (invertedFactorList->categ == C_MULTICATIVE_OPERATOR_FACTOR) {
                        processMultiplicativeFactor (invertedFactorList);
                }

                aux = invertedFactorList->next;
                freeNode (invertedFactorList);
                invertedFactorList = aux;
        }

        freeNode (invertedFactorList);

        return result;

}

/* Processes a multiplicative factor */
TypeDescrPtr processMultiplicativeFactor(TreeNodePtr p) {

        TypeDescrPtr argument2 = processFactor (p->comps[1]);

        char* op = p->comps[0]->str;

        if (strcmp(op, "*") == 0)
                genCode0("MULT", "");
        else if (strcmp(op, "/") == 0)
                genCode0("DIVI", "");
        else if (strcmp(op, "&&") == 0)
                genCode0("LAND", "");

        free (p->comps[0]);
        freeNode (p->comps[1]);

        return argument2;

}

/* Processes an additive term */
TypeDescrPtr processAdditiveTerm (TreeNodePtr p) {

        TypeDescrPtr argument2 = processTerm (p->comps[1]);

        char* op = p->comps[0]->str;

        if (strcmp(op, "+") == 0)
                genCode0("ADDD", "");
        else if (strcmp(op, "-") == 0)
                genCode0("SUBT", "");
        else if (strcmp(op, "||") == 0)
                genCode0("LORR", "");

        free (p->comps[0]);
        freeNode (p->comps[1]);

        return argument2;

}

/* Generates code for array indexes */
TypeDescrPtr processArray (TreeNodePtr p, TypeDescrPtr varType) {

        TypeDescrPtr exprType;
        TreeNodePtr aux;

        p = p->next;

        while (p->categ != C_EMPTY) {

                /* Generates code for the expression inside the [] */
                exprType = processExpr (p);
                if (!compatibleType (exprType, INTEGER))
                        SemanticError(NULL);

                genCode1 ("INDX", varType->descr.ArrayType.element->size, "");

                varType = varType->descr.ArrayType.element;

                aux = p->next;
                freeNode (p);
                p = aux;
        }

        freeNode (p);

        return varType;
}

/* Processes a factor statement */
TypeDescrPtr processFactor (TreeNodePtr p) {

        SymbEntryPtr variableEntry;
        TreeNodePtr invertedVariableList, invertedFactorList = invertList (p->comps[0]);
        TypeDescrPtr aux;

        switch (invertedFactorList->categ) {
                case C_INTEGER:
                        /* Generates code for an integer */
                        genCode1 ("LDCT", strtol(p->comps[0]->str, NULL, 10), "");
                        freeNode (p->comps[0]);
                        return INTEGER;

                case C_VARIABLE:

                        /* Generates code for a variable */

                        variableEntry = searchId (invertedFactorList->comps[0]->str);

                        freeNode (invertedFactorList->comps[0]);

                        if (variableEntry == NULL)
                                SemanticError(NULL);

                        /* Constant variable: true or false reserved keywords */
                        if (variableEntry->categ == S_CONST) {

                                if (variableEntry == FALSE || variableEntry == TRUE) {
                                        genCode1 ("LDCT", variableEntry == FALSE ? 0 : 1, "");

                                        freeNode (invertedFactorList);

                                        return BOOLEAN;
                                }
                        }
                        else if (variableEntry->categ == S_VARIABLE) {
                                /* one-dimensional variables */
                                if (variableEntry->descr.variableDescr->type->constr != T_ARRAY) {
                                        if (referenceParameter != P_VARIABLE)
                                                genCode2 (readInstr ? "STVL" : "LDVL", variableEntry->level, variableEntry->descr.variableDescr->displ, "");
                                        else
                                                genCode2 ("LADR", variableEntry->level, variableEntry->descr.variableDescr->displ, "");
                                }
                                else { /* ARRAY variables */

                                        genCode2 ("LADR", variableEntry->level, variableEntry->descr.variableDescr->displ, "");
                                        
                                        TypeDescrPtr varType = processArray (invertedFactorList, variableEntry->descr.variableDescr->type);

                                        if (referenceParameter != P_VARIABLE) {
                                                if (varType->size > 1)
                                                        genCode1 ("LDMV", varType->size, "");
                                                else 
                                                        genCode0 ("CONT", "");
                                        }

                                        freeNode (invertedFactorList);

                                        return varType;
                                }
                        }
                        else if (variableEntry->categ == S_PARAMETER) {
                                /* Parameter variables */

                                if (variableEntry->descr.paramDescr->type->constr != T_ARRAY) {
                                        /* one-dimensional parameters */
                                        Passage pass_method = variableEntry->descr.paramDescr->pass;
                                        genCode2 (pass_method == P_VALUE ? "LDVL" : "LADR", variableEntry->level, variableEntry->descr.paramDescr->displ, "");
                                }
                                else if (variableEntry->descr.paramDescr->type->constr == T_ARRAY) { /* ARRAY parameters */
                                        Passage pass_method = variableEntry->descr.paramDescr->pass;
                                        if (pass_method == P_VALUE) {
                                                genCode2 ("LADR", variableEntry->level, variableEntry->descr.paramDescr->displ, "");

                                                TypeDescrPtr varType = processArray (invertedFactorList, variableEntry->descr.paramDescr->type);

                                                if (varType->size > 1)
                                                        genCode1 ("LDMV", varType->size, "");
                                                else 
                                                        genCode0 ("CONT", "");

                                                freeNode (invertedFactorList);

                                                return varType;
                                        }
                                        else {
                                                genCode2 ("LDVL", variableEntry->level, variableEntry->descr.paramDescr->displ, "");
                                        }
                                }
                        }
                        else if (variableEntry->categ == S_FUNCTION) {
                                /* functions passed as parameters */

                                TypeDescrPtr functionType = (TypeDescrPtr) malloc (sizeof (TypeDescr));
                                functionType->constr = T_FUNCTION;
                                functionType->size = 3;
                                functionType->descr.FunctionType.result = variableEntry->descr.functionDescr->result;
                                functionType->descr.FunctionType.params = variableEntry->descr.functionDescr->params;

                                genLabel2 ("LGAD", variableEntry->descr.functionDescr->entLabel, variableEntry->level, "");

                                freeNode (invertedFactorList);

                                return functionType;
                        }

                        
                        freeNode (invertedFactorList);

                        return variableEntry->descr.variableDescr->type;

                case C_EXPRESSION:

                        /* Processes an expression */
                        aux = processExpr (invertedFactorList);

                        freeNode (invertedFactorList);

                        return aux;

                case C_FACTOR:

                        /* Processes a factor */
                        aux = processFactor (invertedFactorList);

                        freeNode (invertedFactorList);

                        if (compatibleType(aux, BOOLEAN)) /* verify if it is bool */
                                genCode0 ("LNOT", "");
                        else
                                SemanticError (NULL);

                        return aux;

                case C_FUNCTION_CALL:

                        /* Processes a function call */
                        aux = processFunctionCall (invertedFactorList);

                        freeNode (invertedFactorList);

                        return aux;
        }

}

/* Generates symbol table entries for the formal parameters of a given function */
SymbEntryPtr processFormals(TreeNodePtr p, int* lastDispl) {

        /* p's category should be C_FORMAL_PARAMETERS */

        TreeNodePtr invertedList = invertList(p->comps[0]), next;

        SymbEntryPtr formalParameterList = NULL, aux;

        while (invertedList->categ != C_EMPTY) {

                SymbEntryPtr formalParameter;
                Passage pass_method;

                switch (invertedList->categ) {

                        case C_EXPRESSION_PARAMETER:

                                pass_method = P_VALUE;
                                if (invertedList->comps[2]->categ == C_VAR_MECHANISM)
                                        pass_method = P_VARIABLE;

                                TypeDescrPtr paramType = getType(invertedList->comps[1]->str);

                                TreeNodePtr invertedIdentList = invertList(invertedList->comps[0]->comps[0]);

                                while (invertedIdentList->categ != C_EMPTY) {

                                        formalParameter = newSymbEntry(S_PARAMETER, invertedIdentList->str);
                                        formalParameter->descr.paramDescr = (ParameterDescPtr) malloc (sizeof (ParameterDesc));
                                        formalParameter->descr.paramDescr->pass = pass_method;
                                        formalParameter->descr.paramDescr->type = paramType;


                                        if (pass_method == P_VALUE)
                                                *lastDispl = *lastDispl - paramType->size;
                                        else
                                                *lastDispl = *lastDispl - 1;

                                        if (formalParameterList == NULL) {
                                                formalParameterList = formalParameter;
                                                aux = formalParameter;
                                        }
                                        else {
                                                aux->next = formalParameter;
                                                aux = formalParameter;
                                        }

                                        invertedIdentList = invertedIdentList->next;
                                }

                                break;

                        case C_FUNCTION_PARAMETER:

                                formalParameter = newSymbEntry(S_PARAMETER, invertedList->comps[1]->str);
                                formalParameter->descr.paramDescr = (ParameterDescPtr) malloc (sizeof (ParameterDesc));
                                formalParameter->descr.paramDescr->pass = P_VALUE;

                                TypeDescrPtr functionType = (TypeDescrPtr) malloc (sizeof (TypeDescr));
                                functionType->constr = T_FUNCTION;
                                functionType->size = 3;

                                if (invertedList->comps[0]->categ == C_EMPTY)
                                       functionType->descr.FunctionType.result = NULL;
                                else  
                                       functionType->descr.FunctionType.result = getType(invertedList->comps[0]->str);

                                int displ = -4, i;
                                SymbEntryPtr functionFormals = processFormals (invertedList->comps[2], &displ);

                                functionType->descr.FunctionType.params = functionFormals;

                                formalParameter->descr.paramDescr->type = functionType;

                                *lastDispl = *lastDispl - functionType->size;

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

                next = invertedList->next;
                freeNode (invertedList);
                invertedList = next;
        }

        freeNode (invertedList);

        aux = formalParameterList;
        int i = 0;
        while (aux != NULL) {
                aux->descr.paramDescr->displ = *lastDispl + i;

                if (aux->descr.paramDescr->pass == P_VALUE)
                        i += aux->descr.paramDescr->type->size;
                else 
                        i++;

                aux = aux->next;              
        }

        return formalParameterList;
}

/** Code generation functions **/

void genCode0 (char* instr, char* comment) {
        printf ("%10s%s\n", instr, comment);
}

void genCode1 (char* instr, int param, char* comment) {
        printf ("%10s   %d%s\n", instr, param, comment);
}

void genCode2 (char* instr, int param1, int param2, char* comment) {
        printf ("%10s   %d,%d%s\n", instr, param1, param2, comment);
}

void genCode3 (char* instr, int param1, int param2, int param3, char* comment) {
        printf ("%10s   %d,%d,%d%s\n", instr, param1, param2, param3, comment);
}

void genLabel1 (char* instr, int label, char* comment) {
        printf ("%10s   L%d%s\n", instr, label, comment);
}

void genLabel2 (char* instr, int label, int param, char* comment) {
        printf ("%10s   L%d,%d%s\n", instr, label, param, comment);
}

void genLabel3 (char* instr, int label, int param1, int param2, char* comment) {
        printf ("%10s   L%d,%d,%d%s\n", instr, label, param1, param2, comment);
}

void genCodeLabel(int label, char* comment) {
        char lbl[6];
        sprintf (lbl, "L%d:", label);
        printf ("%-6sNOOP%13s\n", lbl, comment);
}

void genCodeLabel1(int label, char* instr, int param1, char* comment) {
        char lbl[6];
        sprintf (lbl, "L%d:", label);
        printf ("%-6s%s   %d         %s\n", lbl, instr, param1, comment);
}

void genCodeLabel2(int label, char* instr, int param1, int param2, char* comment) {
        char lbl[6];
        sprintf (lbl, "L%d:", label);
        printf ("%-6s%s   %d,%d        %s\n", lbl, instr, param1, param2, comment);
}
