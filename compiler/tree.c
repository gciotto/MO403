/**
* Auxiliary functions needed to build a program's derivation tree.
*
* Gustavo Ciotto Pinton - September 2017
**/


#include "tree.h"
#include <stdlib.h>
#include <stdio.h>

/* Tree's stack memory allocation */
TreeNodePtr stack[MAX_STACK_SIZE];

/* Stack's top index */
int top = 0;

/* Compiler's tree can be retrieved by returning the first element in the stack */
TreeNodePtr getTree() {
        return stack[0];
}

void dumpNode(TreeNodePtr node, int tab_count) {

        int i;

        if (node == NULL)
                return;

        // Prints tabs according to the tree's height.
        for (i = 0; i < tab_count; i++)
                printf("\t");

        // Prints the node's string if it's not null.
        if (node->str)
                printf ("Category = %d, String = %s\n", node->categ, node->str);
        else
                printf ("Category = %d\n", node->categ);

}

/* Recursively prints the tree given as parameter. Tab_count is the number of tabs which will be printed 
   before each node. It corresponds to the respective node's heigth. */
void dumpTree(TreeNodePtr tree, int tab_count) {

        int i;

        if (tree == NULL)
                return;

        dumpNode(tree, tab_count);
        
        i = 0;
        // Prints all the tree's leaves.
        while (tree->comps[i] && i < MAX_COMPS)
                dumpTree(tree->comps[i++], tab_count + 1);

        // Prints next in the list.
        dumpTree(tree->next, tab_count);

}

/* Counts the number of declared functions, function call, while and if statementas and binary operations 
   present in the program. Given the recursive nature of a tree, this function is also recursive. */
void countRecursive(TreeNodePtr tree, int *functions, int *funcalls, int *whiles, int *ifs, int *bin) {

        if (tree == NULL)
                return;

        switch (tree->categ) {
                
                case C_FUNCTION: (*functions)++;
                                 break;

                case C_FUNCTION_CALL: (*funcalls)++;
                                      break;

                case C_WHILE: (*whiles)++;
                              break;

                case C_IF: (*ifs)++;
                           break;

                case C_RELATIONAL_OPERATOR_SIMPLE_EXPRESSION:
                case C_ADDITIVE_OPERATOR_TERM:
                case C_MULTICATIVE_OPERATOR_FACTOR: (*bin)++;
                                                    break;
        }

        int i = 0;
        // Counts all this node's leaves.
        while (tree->comps[i] && i < MAX_COMPS) {
                countRecursive(tree->comps[i++], functions, funcalls, whiles, ifs, bin);
        }

        // Counts the next element in the list.
        countRecursive(tree->next, functions, funcalls, whiles, ifs, bin);

}

/* This function is needed by the test program.  */
void counts(TreeNodePtr tree, int *functions, int *funcalls, int *whiles, int *ifs, int *bin) {

        *functions = *funcalls = *whiles = *ifs = *bin = 0;

        countRecursive(tree, functions, funcalls, whiles, ifs, bin);
}

/* Generates a tree node with n leaves, retrieved from the n top elements in the stack, a given category and a string.
   The new node is placed in the top of the stack in the end of this function.
   Provided by Tomasz Kowaltowski. */
void genNode3(Categ cat, int n, char *s) {

        TreeNodePtr np = (TreeNodePtr) malloc(sizeof(TreeNode));
        np->categ = cat;
        np->next = NULL;
        np->str = s;

        for (int i = 0; i < n; i++)
                np->comps[i] = stack[top - n + i];

        top -= (n - 1);
        stack[top - 1] = np;

}

/* Generates a node with a null string. */
void genNode(Categ cat, int n) {
        genNode3(cat, n, NULL);
}

/* Generates a leaf node describing an identifier. */
void genIdent(char *tok_val) {
        genNode3(C_IDENT, 0, tok_val);
}

/* Generates a leaf node describing an integer number. */
void genInteger(char *tok_val) {
        genNode3(C_INTEGER, 0, tok_val);
}

/* Generates a leaf node describing an empty character. */
void genEmpty() {
        genNode3(C_EMPTY, 0, NULL);
}

/* Generates a leaf node describing a unary/binary operator. */
void genOpSymbol(char *symbol) {
        genNode3(C_OP_SYMBOL, 0, symbol);
}

/* Removes an element from the top of the stack and inserts it into the subsequent element's linked list. 
   Provided by Tomasz Kowaltowski. */
void insertTopList() {
        // Reversed list!

        TreeNodePtr t = stack[--top];
        TreeNodePtr s = stack[top-1];

        t->next = s;

        stack[top-1] = t;
}

/* Reclaims a node's allocated memory. */
void freeNode(TreeNodePtr node) {

        if (node == NULL)
                return;

        if (node->str)
                free(node->str);

        node->str = NULL;

        free (node);
}


/* Reclaims the memory allocated for the given tree. */
void freeTree(TreeNodePtr tree) {

        if (tree == NULL)
                return;

        int i = 0;

        while (tree->comps[i] && i < MAX_COMPS)
                freeTree(tree->comps[i++]);

        freeTree(tree->next);

        freeNode(tree);
}

int getStackHeight() {
        return top;
}

TreeNodePtr invertList (TreeNodePtr p) {

        TreeNodePtr actual, next;

        actual = p;
        next = p->next;

        actual->next = NULL;

        while (next != NULL)  {
                TreeNodePtr aux = next->next;
                next->next = actual;
                actual = next;
                next = aux;
        }

        return actual;       
}
