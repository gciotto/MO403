#ifndef TREE_H
#define TREE_H

/**
* Declaration of the auxiliary functions needed to build a program's derivation tree.
*
* Gustavo Ciotto Pinton - September 2017
**/


#define MAX_COMPS 16
#define MAX_STACK_SIZE 64

typedef enum {

        C_PROGRAM = 1,
        C_FUNCTION,
        C_BLOCK,
        C_LABELS,
        C_TYPES,
        C_IDENTIFIER_TYPE,
        C_VARIABLES,
        C_IDENTIFIER_LIST_TYPE,
        C_FUNCTIONS,
        C_BODY,
        C_TYPE,
        C_TYPE_BRACE,
        C_FORMAL_PARAMETERS,
        C_EXPRESSION_PARAMETER,
        C_FUNCTION_PARAMETER,
        C_STATEMENT,
        C_VARIABLE,
        C_ASSIGN,
        C_FUNCTION_CALL_STATEMENT,
        C_GOTO,
        C_RETURN,
        C_COMPOUND,
        C_IF,
        C_WHILE,
        C_EXPRESSION,
        C_RELATIONAL_OPERATOR_SIMPLE_EXPRESSION,
        C_SIMPLE_EXPRESSION,
        C_ADDITIVE_OPERATOR_TERM,
        C_TERM,
        C_MULTICATIVE_OPERATOR_FACTOR,
        C_OP_SYMBOL,
        C_FACTOR,
        C_FUNCTION_CALL,
        C_IDENTIFIER_LIST,
        C_EXPRESSION_LIST,
        C_INTEGER,
        C_IDENT,
        C_OPERATOR,
        C_EMPTY,

} Categ;

typedef struct _treeNode {
        Categ categ;
        struct _treeNode *next; // list
        struct _treeNode *comps[MAX_COMPS];
        char *str; // IDENT, INTEGER or operator

} TreeNode, *TreeNodePtr;

TreeNodePtr getTree();
void counts(TreeNodePtr tree, int *functions, int *funcalls, int *whiles, int *ifs, int *bin);

void printTree(TreeNodePtr tree, int tab_count);

void genNode(Categ cat, int n);
void genNode3(Categ cat, int n, char *s);

void genIdent(char *tok_val);
void genInteger(char *tok_val);
void genEmpty();
void genOpSymbol(char *symbol);

void insertTopList();

void dumpNode(TreeNodePtr node);
void dumpTree(TreeNodePtr tree);

#endif
