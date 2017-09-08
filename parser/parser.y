%{

/**
* Parser descriptor file for the Simple Language compiler.
* 
* MO403 - Implementation of Programming Languages
*
* Gustavo Ciotto Pinton
*
**/

/* Declarations to avoid annoying warnings */
/* FLEX functions */
int yylex (void); 
int yyerror(char *s);

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
%left UNARY
%left MULTIPLY DIV

%%

program
        : function END_OF_FILE {return 0;}
        ;

function
        : function_header function_continuation
        ;

function_header
        : VOID
        | identifier
        ;

function_continuation
        : identifier formal_parameters block
        ;

block
        : block_labels block_types block_variables block_functions body
        ;

block_labels
        : labels
        | 
        ;

block_types
        : types
        | 
        ;

block_variables
        : variables
        | 
        ;

block_functions
        : functions
        | 
        ;

labels
        : LABELS identifier_list SEMI_COLON
        ;

types
        : TYPES identifier ASSIGN type SEMI_COLON identifier_assign_type
        ;

identifier_assign_type 
        : identifier ASSIGN type SEMI_COLON identifier_assign_type
        | 
        ;

variables
        : VARS identifier_list COLON type SEMI_COLON identifier_list_type
        ;

identifier_list_type
        : identifier_list COLON type SEMI_COLON identifier_list_type
        | 
        ;

functions
        : FUNCTIONS function function_empty
        ;

function_empty
        : function function_empty
        | 
        ;

body
        : OPEN_BRACE body_statement CLOSE_BRACE
        ;

body_statement
        : statement body_statement
        | 
        ;

type
        : identifier type_brackets
        ;

type_brackets
        : OPEN_BRACKET integer CLOSE_BRACKET type_brackets
        | 
        ;

formal_parameters
        : OPEN_PAREN formal_parameter semicolon_formal_parameters CLOSE_PAREN
        | OPEN_PAREN CLOSE_PAREN
        ;

semicolon_formal_parameters
        : SEMI_COLON formal_parameter semicolon_formal_parameters
        | 
        ;

formal_parameter
        : expression_parameter
        | function_parameter
        ;

expression_parameter
        : VAR identifier_list COLON identifier
        | identifier_list COLON identifier
        ;

function_parameter
        : function_parameter_header identifier formal_parameters
        ;

function_parameter_header
        : identifier
        | VOID
        ;

statement
        : identifier COLON unlabeled_statement
        | unlabeled_statement
        | compound
        ;

variable
        : identifier variable_array
        ;

variable_array
        : OPEN_BRACKET expression CLOSE_BRACKET variable_array
        | 
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
        : variable ASSIGN expression SEMI_COLON
        ;

function_call_statement
        : function_call SEMI_COLON
        ;

goto
        : GOTO identifier SEMI_COLON
        ;

return
        : RETURN SEMI_COLON
        | RETURN expression SEMI_COLON
        ;

compound
        : OPEN_BRACE unlabeled_statement compound_unlabeled_statement CLOSE_BRACE
        ;

compound_unlabeled_statement
        : unlabeled_statement compound_unlabeled_statement
        | 
        ;


conditional
        : IF OPEN_PAREN expression CLOSE_PAREN compound
        | IF OPEN_PAREN expression CLOSE_PAREN compound ELSE compound
        ;

repetitive
        : WHILE OPEN_PAREN expression CLOSE_PAREN compound
        ;

empty_statement
        : SEMI_COLON
        ;

expression
        : simple_expression relational_operator_simple_expression
        ;

relational_operator_simple_expression
        : relational_operator simple_expression
        | 
        ;

relational_operator
        : EQUAL
        | DIFFERENT
        | LESS
        | LESS_OR_EQUAL
        | GREATER
        | GREATER_OR_EQUAL
        ;

simple_expression
        : PLUS term additive_operator_term %prec UNARY
        | MINUS term additive_operator_term %prec UNARY
        | term additive_operator_term
        ;

additive_operator_term
        : additive_operator term additive_operator_term
        | 
        ;

additive_operator
        : PLUS
        | MINUS
        | OR
        ;

term
        : factor multiplicative_operator_factor
        ;

multiplicative_operator_factor
        : multiplicative_operator factor multiplicative_operator_factor
        | 
        ;

multiplicative_operator
        : MULTIPLY
        | DIV
        | AND
        ;

factor
        : variable
        | integer
        | function_call
        | OPEN_PAREN expression CLOSE_PAREN
        | NOT factor
        ;

function_call
        : identifier OPEN_PAREN expression_list CLOSE_PAREN
        ;

identifier_list
        : identifier comma_identifier
        ;

comma_identifier
        : COMMA identifier comma_identifier
        | 
        ;

expression_list
        : expression comma_expression
        | 
        ;

comma_expression
        : COMMA expression comma_expression
        | 
        ;

integer
        : INTEGER
        ;

identifier
        : IDENTIFIER
        ;

%%


