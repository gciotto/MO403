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
        int displ;
        TypeDescrPtr result;
        /* ParameterDescPtr params; */
        struct _symbEntry *params;
} FunctionDesc, *FunctionDescPtr;

typedef struct {
        char* label;
        int defined;
} LabelDesc, *LabelDescPtr;

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

SymbEntryPtr WRITE_FUNCTION, READ_FUNCTION;
TypeDescrPtr INTEGER, BOOLEAN;

void dumpSymbolTable();
void initSymbolTable();
SymbEntryPtr searchSte(char* id);
SymbEntryPtr newSymbEntry(SymbCateg entryCateg, char* id);
void insertSymbolTable (SymbEntryPtr newEntry);
void saveSymbTable();
void loadFormalsSymbolTable(SymbEntryPtr entryList);
void restoreSymbTable();
void incrCurrentLevel();
void decrCurrentLevel();
int getCurrentLevel();
TypeDescrPtr getType(TreeNodePtr p);
SymbEntryPtr getFunction(char* id);
SymbEntryPtr getVariable(char* id);
TypeDescrPtr determineType(TreeNodePtr p);
int nextLabel();

#endif
