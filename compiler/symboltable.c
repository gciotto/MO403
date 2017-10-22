#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "symboltable.h"

SymbEntryPtr symbolTable, savedSymbolTableTop;
int level, label;

void dumpSymbolTable () {

        SymbEntryPtr nextEntry = symbolTable;
        while (nextEntry != NULL) {

                printf("%d, %s\n", nextEntry->categ, nextEntry->ident);
                nextEntry = nextEntry->next;
        }
}

void initSymbolTable() {

        level = -1;
        label = 0;
        symbolTable = NULL;

        /**** Generates initial contents of the symbolTable ****/

        /* Generates default variable types integer and boolean */
        TypeDescrPtr integer, boolean;

        integer = (TypeDescrPtr) malloc (sizeof (TypeDescr));
        integer->constr = T_PREDEF;
        integer->size = 1;
        INTEGER = integer;

        boolean = (TypeDescrPtr) malloc (sizeof (TypeDescr));
        boolean->constr = T_PREDEF;
        boolean->size = 1;
        BOOLEAN = boolean;

        SymbEntryPtr integerEntry = newSymbEntry(S_TYPE, "integer"),
                     booleanEntry = newSymbEntry(S_TYPE, "boolean");

        integerEntry->descr.typeDescr = integer;
        booleanEntry->descr.typeDescr = boolean;

        insertSymbolTable (integerEntry);
        insertSymbolTable (booleanEntry);

        /* Generates false and true constants */
        SymbEntryPtr falseEntry = newSymbEntry(S_CONST, "false"),
                     trueEntry  = newSymbEntry(S_CONST, "true");

        falseEntry->descr.constantDescr = (ConstantDescPtr) malloc (sizeof (ConstantDesc));
        falseEntry->descr.constantDescr->value = 0;
        falseEntry->descr.constantDescr->type  = boolean;
        FALSE = falseEntry;

        trueEntry->descr.constantDescr = (ConstantDescPtr) malloc (sizeof (ConstantDesc));
        trueEntry->descr.constantDescr->value = 1;
        trueEntry->descr.constantDescr->type  = boolean;
        TRUE = trueEntry;

        insertSymbolTable (falseEntry);
        insertSymbolTable (trueEntry);

        /* Generates read and write pseudo-functions */

        SymbEntryPtr writeEntry = newSymbEntry(S_FUNCTION, "write"),
                     readEntry  = newSymbEntry(S_FUNCTION, "read");

        writeEntry->descr.functionDescr = (FunctionDescPtr) malloc (sizeof (FunctionDesc));
        writeEntry->descr.functionDescr->displ = -5; /* Parameter and -4 from function call */
        writeEntry->descr.functionDescr->result = NULL;

        SymbEntryPtr writeParameterEntry = newSymbEntry(S_PARAMETER, "");
        writeParameterEntry->descr.paramDescr = (ParameterDescPtr) malloc (sizeof (ParameterDesc));
        writeParameterEntry->descr.paramDescr->displ = -5;
        writeParameterEntry->descr.paramDescr->pass = P_VALUE;
        writeParameterEntry->descr.paramDescr->type = integer;

        writeEntry->descr.functionDescr->params = writeParameterEntry;

        readEntry->descr.functionDescr = (FunctionDescPtr) malloc (sizeof (FunctionDesc));
        readEntry->descr.functionDescr->displ = -5; /* Return and -4 from function call */
        readEntry->descr.functionDescr->result = NULL;
        readEntry->descr.functionDescr->params = writeParameterEntry;

        insertSymbolTable (writeEntry);
        insertSymbolTable (readEntry);

        WRITE_FUNCTION = writeEntry;
        READ_FUNCTION = readEntry;

}

void freeSymbolTable() {
        freeSymbolTableFrom (symbolTable);
}

void freeSymbolTableFrom (SymbEntryPtr entry) {

        if (entry == NULL)
                return;

        SymbEntryPtr aux;

        while (entry != NULL) {

                free (entry->ident);

                switch (symbolTable->categ) {

                        case S_CONST:
                                free (entry->descr.constantDescr);
                                break;
                        case S_VARIABLE:
                                free (entry->descr.variableDescr);
                                break;
                        case S_PARAMETER:
                                free (entry->descr.paramDescr);
                                break;
                        case S_FUNCTION:
                                free (entry->descr.functionDescr);
                                break;
                        case S_LABEL:
                                free (entry->descr.labelDescr);
                                break;
                        case S_TYPE:
                                
                                if (entry->descr.typeDescr->constr == T_ARRAY)
                                        free (entry->descr.typeDescr->descr.ArrayType.element);
                                else if (entry->descr.typeDescr->constr == T_FUNCTION)
                                        free (entry->descr.typeDescr->descr.FunctionType.result);
                                
                                free (entry->descr.typeDescr);
                                break;
                }

                aux = entry->next;
                free (entry);
                entry = aux;
        }
}

SymbEntryPtr newSymbEntry(SymbCateg entryCateg, char* id) {

        SymbEntryPtr entry = (SymbEntryPtr) malloc (sizeof (SymbEntry));

        entry->ident = (char*) malloc ((strlen(id) + 1) * sizeof (char));
        strcpy (entry->ident, id);

        entry->categ = entryCateg;
        entry->level = level;
        entry->next = NULL;

        return entry;
}

void insertSymbolTable (SymbEntryPtr newEntry) {

        if (symbolTable == NULL) {
                symbolTable = newEntry;
                return;
        }

        SymbEntryPtr nextEntry = symbolTable;
        while (nextEntry->next != NULL)
                nextEntry = nextEntry->next;

        nextEntry->next = newEntry;
}

void saveSymbTable(SymbEntryPtr entryList) {

        savedSymbolTableTop = entryList;
}

void setSavedState(SymbEntryPtr entry) {
        savedSymbolTableTop = entry;
}

SymbEntryPtr getSavedState() {

        return savedSymbolTableTop;
}

void loadFormalsSymbolTable(SymbEntryPtr entryList) {

        if (entryList == NULL)
                return;

        while (entryList->next != NULL) {
                insertSymbolTable(entryList);
                entryList = entryList->next;
        }

}

void restoreSymbTable(SymbEntryPtr entryList) {
        entryList->next = NULL;
}

void incrCurrentLevel() {
        level++;
}

void decrCurrentLevel() {
        level--;
}

int getCurrentLevel() {
        return level;
}

TypeDescrPtr getType(char *id) {

        SymbEntryPtr nextEntry = symbolTable;
        while (nextEntry != NULL) {

                if (nextEntry->categ == S_TYPE && strcmp (id, nextEntry->ident) == 0)
                        return nextEntry->descr.typeDescr;

                nextEntry = nextEntry->next;
        }

        return NULL;        
}

SymbEntryPtr getFunction(char* id) {

        SymbEntryPtr nextEntry = symbolTable;
        while (nextEntry != NULL) {

                if (nextEntry->categ == S_FUNCTION && strcmp (id, nextEntry->ident) == 0)
                        return nextEntry;

                nextEntry = nextEntry->next;
        }

        return NULL; 

}

SymbEntryPtr getVariable(char* id) {

        SymbEntryPtr nextEntry = symbolTable;
        while (nextEntry != NULL) {

                if (nextEntry->categ == S_VARIABLE && strcmp (id, nextEntry->ident) == 0)
                        return nextEntry;

                nextEntry = nextEntry->next;
        }

        return NULL; 
}

SymbEntryPtr searchId(char* id){

        SymbEntryPtr nextEntry = symbolTable, result = NULL;;
        while (nextEntry != NULL) {

                if (strcmp (id, nextEntry->ident) == 0)
                        result = nextEntry;

                nextEntry = nextEntry->next;
        }

        return result; 
}

TypeDescrPtr multiDimensionalType (TreeNodePtr p, TypeDescrPtr baseType) {

        int dimension = strtol(p->str, NULL, 10);

        TypeDescrPtr arrayType = (TypeDescrPtr) malloc (sizeof (TypeDescr));
        arrayType->constr = T_ARRAY;
        arrayType->descr.ArrayType.dimen = dimension;

        if (p->next->categ == C_EMPTY) {            
                arrayType->descr.ArrayType.element = baseType;
                arrayType->size = dimension * baseType->size;
        }
        else {
                arrayType->descr.ArrayType.element = multiDimensionalType (p->next, baseType);
                arrayType->size = dimension * arrayType->descr.ArrayType.element->size;
        }

        return arrayType;
}

TypeDescrPtr determineType(TreeNodePtr p) {

        TypeDescrPtr baseType = getType(p->str);

        if (p->next->categ == C_EMPTY) /* Variable's type is not an array */
                return baseType;

        /* Multi-dimensional variables */
        TypeDescrPtr arrayType =  multiDimensionalType (p->next, baseType);

        return arrayType;
}

int nextLabel() {
        label++;
        return label;
}

int compatibleType (TypeDescrPtr t1, TypeDescrPtr t2) {

        if (t1 == t2)
                return 1;

        if (t1->size != t2->size || t1->constr != t2->constr)
                return 0;

        if (t1->constr == t2->constr) {

                int compatible = 0;

                switch (t1->constr) {
                        case T_ARRAY:
                                if (t1->descr.ArrayType.dimen != t2->descr.ArrayType.dimen)
                                        return 0;

                                return compatibleType (t1->descr.ArrayType.element, t2->descr.ArrayType.element);

                        case T_FUNCTION:
                                                                
                                if (!compatibleType (t1->descr.FunctionType.result, t2->descr.FunctionType.result))
                                        return 0;

                                SymbEntryPtr paramT1 = t1->descr.FunctionType.params, 
                                             paramT2 = t2->descr.FunctionType.params;

                                while (paramT1 != NULL && paramT2 != NULL) {

                                        if (!compatibleType (paramT1->descr.paramDescr->type, paramT2->descr.paramDescr->type))
                                                return 0;

                                        paramT1 = paramT1->next;
                                        paramT2 = paramT2->next;
                                }

                                if (paramT1 != NULL || paramT2 != NULL) 
                                        return 0;

                                return 1;
                }
        }

        return 0;
}
