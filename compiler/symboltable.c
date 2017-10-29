/**
* symboltable.c
* Provides functions to handle and manage a symbol table
*
* Gustavo CIOTTO PINTON
* October 2017 - MO403 - Implementation of Programming Languages 
**/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "symboltable.h"

/* Internal global variables */
SymbEntryPtr symbolTable, savedSymbolTableTop;
int level, label;

/* Prints the current symbol table's state out */
void dumpSymbolTable () {

        SymbEntryPtr nextEntry = symbolTable;
        while (nextEntry != NULL) {

                printf("%d, %s\n", nextEntry->categ, nextEntry->ident);
                nextEntry = nextEntry->next;
        }
}

/* Inits the a symbol table instance with the default types, contants and functions */
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

/* Frees allocated memory for the entire symbol table */
void freeSymbolTable() {
        freeSymbolTableFrom (symbolTable);
}

/* Frees allocated memory for all entries added after the 'entry' parameter */
void freeSymbolTableFrom (SymbEntryPtr entry) {

        if (entry == NULL)
                return;

        SymbEntryPtr aux;

        while (entry != NULL) {

                /* Frees allocated space for the identification field */
                free (entry->ident);

                /* Frees allocated space for the specific descriptor */
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

/* Creates and returns a new symbol table entry with the category and identification given 
   as parameters */
SymbEntryPtr newSymbEntry(SymbCateg entryCateg, char* id) {

        SymbEntryPtr entry = (SymbEntryPtr) malloc (sizeof (SymbEntry));

        entry->ident = (char*) malloc ((strlen(id) + 1) * sizeof (char));
        strcpy (entry->ident, id);

        entry->categ = entryCateg;
        entry->level = level;
        entry->next = NULL;

        return entry;
}

/* Pushes a new entry into the symbol table */
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

/* Saves a state of the stack */
void saveSymbTable(SymbEntryPtr entryList) {
        savedSymbolTableTop = entryList;
}

/* Gets the saved state of the stack */
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

/* Restores symbol table's stack top to the entryList. */
void restoreSymbTable(SymbEntryPtr entryList) {
        entryList->next = NULL;
}

/* Increases current function level */
void incrCurrentLevel() {
        level++;
}

/* Decreases current function level */
void decrCurrentLevel() {
        level--;
}

/* Returns current function level */
int getCurrentLevel() {
        return level;
}

/* Searches for a S_TYPE entry in the symbol table */
TypeDescrPtr getType(char *id) {

        SymbEntryPtr nextEntry = symbolTable;
        while (nextEntry != NULL) {

                if (nextEntry->categ == S_TYPE && strcmp (id, nextEntry->ident) == 0)
                        return nextEntry->descr.typeDescr;

                nextEntry = nextEntry->next;
        }

        return NULL;        
}

/* Searches for a S_FUNCTION entry in the symbol table */
SymbEntryPtr getFunction(char* id) {

        SymbEntryPtr nextEntry = symbolTable;
        while (nextEntry != NULL) {

                if (nextEntry->categ == S_FUNCTION && strcmp (id, nextEntry->ident) == 0)
                        return nextEntry;

                nextEntry = nextEntry->next;
        }

        return NULL; 

}

/* Searches for a S_VARIABLE entry in the symbol table */
SymbEntryPtr getVariable(char* id) {

        SymbEntryPtr nextEntry = symbolTable;
        while (nextEntry != NULL) {

                if (nextEntry->categ == S_VARIABLE && strcmp (id, nextEntry->ident) == 0)
                        return nextEntry;

                nextEntry = nextEntry->next;
        }

        return NULL; 
}

/* Given an id string, searches for an entry in the symbol table */
SymbEntryPtr searchId(char* id){

        SymbEntryPtr nextEntry = symbolTable, result = NULL;;
        while (nextEntry != NULL) {

                if (strcmp (id, nextEntry->ident) == 0)
                        result = nextEntry;

                nextEntry = nextEntry->next;
        }

        return result; 
}

/* Returns a new multi-dimensional type, given a base type and a tree node pointer */
TypeDescrPtr multiDimensionalType (TreeNodePtr p, TypeDescrPtr baseType) {

        int dimension = strtol(p->str, NULL, 10);

        /* Allocates new type */
        TypeDescrPtr arrayType = (TypeDescrPtr) malloc (sizeof (TypeDescr));
        arrayType->constr = T_ARRAY;
        arrayType->descr.ArrayType.dimen = dimension;

        if (p->next->categ == C_EMPTY) {
                /* Gets out of the recursion */
                arrayType->descr.ArrayType.element = baseType;
                arrayType->size = dimension * baseType->size;
        }
        else {
                /* Determines the type of the current level by calling multiDimensionalType for the next node */
                arrayType->descr.ArrayType.element = multiDimensionalType (p->next, baseType);
                arrayType->size = dimension * arrayType->descr.ArrayType.element->size;
        }

        freeNode (p);

        return arrayType;
}

/* Gets the type of the given tree node p */
TypeDescrPtr determineType(TreeNodePtr p) {

        TypeDescrPtr baseType = getType(p->str);

        if (p->next->categ == C_EMPTY) /* Variable's type is not an array */
                return baseType;

        /* Multi-dimensional variables */
        TypeDescrPtr arrayType =  multiDimensionalType (p->next, baseType);

        return arrayType;
}

/* Increases the label count and returns it */
int nextLabel() {
        label++;
        return label;
}

/* Computes if type t1 is compatible with the type t2 */
int compatibleType (TypeDescrPtr t1, TypeDescrPtr t2) {

        if (t1 == t2)
                return 1;

        if (t1->size != t2->size || t1->constr != t2->constr)
                return 0;

        if (t1->constr == t2->constr) {

                int compatible = 0;

                switch (t1->constr) {
                        /* In the case of an array, we compare all the dimensions and type of each 'level' */
                        case T_ARRAY:
                                if (t1->descr.ArrayType.dimen != t2->descr.ArrayType.dimen)
                                        return 0;

                                return compatibleType (t1->descr.ArrayType.element, t2->descr.ArrayType.element);

                        /* In the case of a function, we compare all params types and return type */
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
