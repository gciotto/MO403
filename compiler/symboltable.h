/**
* symboltable.h
* Function prototypes to handle everything related to the compiler's symbol table.
*
* Gustavo CIOTTO PINTON
* October 2017 - MO403 - Implementation of Programming Languages 
**/

#ifndef _SYMBOLTABLE_H_
#define _SYMBOLTABLE_H_

#include "tree.h"

/* Enumeration Declaration */

typedef enum {
        S_CONST = 1,
        S_VARIABLE,
        S_PARAMETER,
        S_FUNCTION,
        S_LABEL,
        S_TYPE
} SymbCateg;

typedef enum { P_VALUE = 1, P_VARIABLE } Passage;

typedef enum {
        T_PREDEF = 1,
        T_ARRAY,
        T_FUNCTION
} TypeConstr;

/* Forward declaration of ParameterDesc struct */
struct ParameterDesc;
struct _symbEntry;

/* Types descriptor */
typedef struct TypeDescr {
        TypeConstr constr;
        int size;
        union { // depends on constr
                struct {
                        struct TypeDescr *element;
                        int dimen;
                } ArrayType;

                struct {
                        struct TypeDescr *result;
                        /* struct ParameterDesc *params; */
                        struct _symbEntry *params;
                } FunctionType;
        } descr;
} TypeDescr, *TypeDescrPtr;

/* Descriptor definitions */
typedef struct {
        int value;
        TypeDescrPtr type;
} ConstantDesc, *ConstantDescPtr;

typedef struct {
        int displ;
        TypeDescrPtr type;
} VariableDesc, *VariableDescPtr;

typedef struct ParameterDesc {
        int displ;
        TypeDescrPtr type;
        Passage pass;
} ParameterDesc, *ParameterDescPtr;

typedef struct {
        int displ, entLabel, retLabel;
        TypeDescrPtr result;
        struct _symbEntry *params;
} FunctionDesc, *FunctionDescPtr;

typedef struct {
        char* label;
        int defined;
} LabelDesc, *LabelDescPtr;

/* Symbol table entry structure */
typedef struct _symbEntry {
        SymbCateg categ;
        char *ident;
        int level;
        struct _symbEntry *next;
        union { // depends on categ
                ConstantDescPtr constantDescr;
                VariableDescPtr variableDescr;
                ParameterDescPtr paramDescr;
                FunctionDescPtr functionDescr;
                LabelDescPtr labelDescr;
                TypeDescrPtr typeDescr;
        } descr;
} SymbEntry, *SymbEntryPtr;

/* Primitive types, constants and functions supported by the SL  */
SymbEntryPtr WRITE_FUNCTION, READ_FUNCTION, FALSE, TRUE;
TypeDescrPtr INTEGER, BOOLEAN;

/* Function prototypes */
void dumpSymbolTable();
void initSymbolTable();
void freeSymbolTableFrom(SymbEntryPtr entry);
void freeSymbolTable();
SymbEntryPtr newSymbEntry(SymbCateg entryCateg, char* id);
void insertSymbolTable (SymbEntryPtr newEntry);
void saveSymbTable(SymbEntryPtr entryList);
SymbEntryPtr getSavedState();
void loadFormalsSymbolTable(SymbEntryPtr entryList);
void restoreSymbTable(SymbEntryPtr entryList);
void incrCurrentLevel();
void decrCurrentLevel();
int getCurrentLevel();
TypeDescrPtr getType(char *id);
SymbEntryPtr getFunction(char* id);
SymbEntryPtr getVariable(char* id);
SymbEntryPtr searchId(char* id);
TypeDescrPtr multiDimensionalType (TreeNodePtr p, TypeDescrPtr baseType);
TypeDescrPtr determineType(TreeNodePtr p);
int nextLabel();
int compatibleType (TypeDescrPtr t1, TypeDescrPtr t2);

#endif
