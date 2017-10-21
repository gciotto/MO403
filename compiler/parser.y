%{

/**
* Parser descriptor file for the Simple Language compiler.
* 
* MO403 - Implementation of Programming Languages
*
* Gustavo Ciotto Pinton - September 2017
*
**/

#include "tree.h"

/* Declarations to avoid annoying warnings */
/* FLEX functions */
int yylex (void); 
int yyerror(char *s);

/* String retrieved from FLEX */
char *token_value;

%}


%token EQUAL
%token DIFFERENT
%token LESS
%token LESS_OR_EQUAL
%token GREATER
%token GREATER_OR_EQUAL
%token PLUS
%token MINUS
%token OR
%token MULTIPLY
%token DIV
%token AND
%token CLOSE_BRACE
%token CLOSE_BRACKET
%token CLOSE_PAREN
%token COLON
%token COMMA
%token ELSE
%token END_OF_FILE
%token FUNCTIONS
%token GOTO
%token IDENTIFIER
%token ASSIGN
%token IF
%token INTEGER
%token LABELS
%token NOT
%token OPEN_BRACE
%token OPEN_BRACKET
%token OPEN_PAREN
%token RETURN
%token SEMI_COLON
%token TYPES
%token VAR
%token VARS
%token VOID
%token WHILE
%token UNFINISHED_COMMENT
%token LEXICAL_ERROR

%left PLUS MINUS
%left MULTIPLY DIV
%left UNARY

%%

program
        : function END_OF_FILE                                                  { genNode(C_PROGRAM, 1); return 0;}
        ;

function
        : function_header identifier formal_parameters block                    { genNode(C_FUNCTION, 4); }
        ;

function_header
        : VOID                                                                  { genEmpty(); }
        | identifier
        ;

block
        : block_labels block_types block_variables block_functions body         { genNode(C_BLOCK, 5); }
        ;

block_labels
        : labels
        | empty
        ;

block_types
        : types
        | empty
        ;

block_variables
        : variables
        | empty
        ;

block_functions
        : functions
        | empty
        ;

labels
        : LABELS identifier_list SEMI_COLON                                     { genNode(C_LABELS, 1); }
        ;

types
        : TYPES identifier ASSIGN type SEMI_COLON                               { genNode(C_IDENTIFIER_TYPE, 2); }
          identifier_assign_type                                                { genNode(C_TYPES, 1); }
        ;

identifier_assign_type 
        : identifier ASSIGN type SEMI_COLON                                     { genNode(C_IDENTIFIER_TYPE, 2); insertTopList(); }
          identifier_assign_type
        | empty                                                                 { insertTopList(); } 
        ;

variables
        : VARS identifier_list COLON type SEMI_COLON                            { genNode(C_IDENTIFIER_LIST_TYPE, 2); }
          identifier_list_type                                                  { genNode(C_VARIABLES, 1); }
        ;

identifier_list_type
        : identifier_list COLON type SEMI_COLON                                 { genNode(C_IDENTIFIER_LIST_TYPE, 2); insertTopList(); }
          identifier_list_type
        | empty                                                                 { insertTopList(); } 
        ;

functions
        : FUNCTIONS function function_empty                                     { genNode(C_FUNCTIONS, 1); }
        ;

function_empty
        : function                                                              { insertTopList(); }
          function_empty
        | empty                                                                 { insertTopList(); }
        ;

body
        : OPEN_BRACE body_statement
          CLOSE_BRACE                                                           { genNode(C_BODY, 1); }
        ;

body_statement
        : body_statement
          statement                                                             { insertTopList(); }
        | empty                                                                 
        ;

type
        : identifier brackets_list                                              { genNode(C_TYPE, 1); }
        ;

brackets_list
        : OPEN_BRACKET integer CLOSE_BRACKET                                    { genNode (C_TYPE_BRACE, 1); insertTopList(); }
          brackets_list
        | empty                                                                 { insertTopList(); }
        ;

formal_parameters
        : OPEN_PAREN formal_parameter semicolon_formal_parameters CLOSE_PAREN   { genNode(C_FORMAL_PARAMETERS, 1); }    
        | OPEN_PAREN CLOSE_PAREN                                                { genEmpty(); genNode(C_FORMAL_PARAMETERS, 1); }
        ;

semicolon_formal_parameters
        : SEMI_COLON formal_parameter                                           { insertTopList(); }
          semicolon_formal_parameters                                           
        | empty                                                                 { insertTopList(); }
        ;

formal_parameter
        : expression_parameter
        | function_parameter
        ;

expression_parameter
        : VAR identifier_list COLON identifier                                  { genNode(C_VAR_MECHANISM, 0); genNode(C_EXPRESSION_PARAMETER, 3);}
        | identifier_list COLON identifier                                      { genEmpty(); genNode(C_EXPRESSION_PARAMETER, 3);}
        ;

function_parameter
        : function_parameter_header identifier formal_parameters                { genNode(C_FUNCTION_PARAMETER, 3); }
        ;

function_parameter_header
        : identifier
        | VOID                                                                  { genEmpty(); }
        ;

statement
        : identifier COLON unlabeled_statement                                  { genNode(C_STATEMENT, 2);}
        | unlabeled_statement                                                   { genNode(C_STATEMENT, 1);}
        | compound                                                              { genNode(C_STATEMENT, 1);}
        ;


variable
        : identifier                                                            { genNode(C_VARIABLE, 1); }
          variable_array
        ;

variable_array
        : OPEN_BRACKET expression CLOSE_BRACKET                                 { insertTopList(); }
          variable_array
        | empty                                                                 { insertTopList(); }
        ;

unlabeled_statement
        : assignment 
        | function_call_statement
        | goto
        | return
        | conditional
        | repetitive
        | empty_statement
        ;

assignment
        : variable ASSIGN expression SEMI_COLON                                 { genNode(C_ASSIGN, 2); }
        ;

function_call_statement
        : function_call SEMI_COLON                                              { genNode (C_FUNCTION_CALL_STATEMENT, 1); }
        ;

goto
        : GOTO identifier SEMI_COLON                                            { genNode (C_GOTO, 1); }
        ;

return
        : RETURN return_alternative SEMI_COLON                                  { genNode (C_RETURN, 1); }
        ;

return_alternative
        : empty
        | expression
        ;

compound
        : OPEN_BRACE unlabeled_statement compound_unlabeled_statement CLOSE_BRACE       { genNode(C_COMPOUND, 1); }
        ;

compound_unlabeled_statement
        : unlabeled_statement                                                   { insertTopList(); }
          compound_unlabeled_statement
        | empty                                                                 { insertTopList(); }
        ;


conditional
        : IF OPEN_PAREN expression CLOSE_PAREN compound conditional_else        { genNode (C_IF, 3); }
        ;

conditional_else
        : empty
        | ELSE compound
        ;

repetitive
        : WHILE OPEN_PAREN expression CLOSE_PAREN compound                      { genNode (C_WHILE, 2); }
        ;

empty_statement
        : SEMI_COLON                                                            { genEmpty(); }
        ;

expression
        : simple_expression relational_operator_simple_expression               { genNode(C_EXPRESSION, 2); }
        ;

relational_operator_simple_expression
        : relational_operator simple_expression                         { genNode(C_RELATIONAL_OPERATOR_SIMPLE_EXPRESSION, 2); }
        | empty
        ;

relational_operator
        : EQUAL                                                         { genOpSymbol("=="); }
        | DIFFERENT                                                     { genOpSymbol("!="); }
        | LESS                                                          { genOpSymbol("<"); }
        | LESS_OR_EQUAL                                                 { genOpSymbol("<="); }
        | GREATER                                                       { genOpSymbol(">"); }
        | GREATER_OR_EQUAL                                              { genOpSymbol(">="); }
        ;

simple_expression
        : PLUS  { genOpSymbol("+"); } term additive_operator_term %prec UNARY     { genNode(C_SIMPLE_EXPRESSION, 2); }
        | MINUS { genOpSymbol("-"); } term additive_operator_term %prec UNARY     { genNode(C_SIMPLE_EXPRESSION, 2); }
        | empty term additive_operator_term                                       { genNode(C_SIMPLE_EXPRESSION, 2); }
        ;

additive_operator_term
        : additive_operator term                                        { genNode(C_ADDITIVE_OPERATOR_TERM, 2); insertTopList(); }
          additive_operator_term
        | empty                                                         { insertTopList(); }
        ;

additive_operator
        : PLUS                                                          { genOpSymbol("+"); }
        | MINUS                                                         { genOpSymbol("-"); }
        | OR                                                            { genOpSymbol("||"); }
        ;

term
        : factor multiplicative_operator_factor                         { genNode(C_TERM, 1); }
        ;

multiplicative_operator_factor
        : multiplicative_operator factor                                { genNode(C_MULTICATIVE_OPERATOR_FACTOR, 2); insertTopList(); }
          multiplicative_operator_factor                                
        | empty                                                         { insertTopList(); }
        ;

multiplicative_operator
        : MULTIPLY                                                      { genOpSymbol("*"); }
        | DIV                                                           { genOpSymbol("/"); }
        | AND                                                           { genOpSymbol("&&"); }
        ;

factor
        : variable                                                      { genNode (C_FACTOR, 1); }
        | integer                                                       { genNode (C_FACTOR, 1); }
        | function_call                                                 { genNode (C_FACTOR, 1); }
        | OPEN_PAREN expression CLOSE_PAREN                             { genNode (C_FACTOR, 1); }
        | NOT factor                                                    { genNode (C_FACTOR, 1); }
        ;

function_call
        : identifier OPEN_PAREN expression_list CLOSE_PAREN             { genNode(C_FUNCTION_CALL, 2);}
        ;

identifier_list
        : identifier comma_identifier                                   { genNode(C_IDENTIFIER_LIST, 1); }
        ;

comma_identifier
        : COMMA identifier                                              { insertTopList(); }
          comma_identifier 
        | empty                                                         { insertTopList(); }                                                      
        ;

expression_list
        : expression comma_expression                                   { genNode(C_EXPRESSION_LIST, 1); }                         
        | empty                                                         { genNode(C_EXPRESSION_LIST, 1); }
        ;

comma_expression
        : COMMA expression                                              { insertTopList(); }
          comma_expression 
        | empty                                                         { insertTopList(); }
        ;

empty
        :                                                               { genEmpty(); }
        ;

integer
        : INTEGER                                                       { genInteger(token_value);}
        ;

identifier
        : IDENTIFIER                                                    { genIdent(token_value); }
        ;

%%


