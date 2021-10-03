#ifndef HEADER_H_INCLUDED
#define HEADER_H_INCLUDED

/******************************************************************************************************************************************
    All enumeration literals
       TokenType : Specify the type of the token scanner returns
	   DataType  : The data type of the declared variable
	   StmtType  : Indicate one statement in AcDc program is print or assignment statement.
	   ValueType : The node types of the expression tree that represents the expression on the right hand side of the assignment statement.
	               Identifier, IntConst, FloatConst must be the leaf nodes ex: a, b, c , 1.5 , 3.
				   PlusNode, MinusNode, MulNode, DivNode are the operations in AcDc. They must be the internal nodes.
                   Note that IntToFloatConvertNode to represent the type coercion may appear after finishing type checking. 			  
	   Operation : Specify all arithematic expression, including +, - , *, / and type coercion.
*******************************************************************************************************************************************/

typedef enum TokenType { FloatDeclaration, IntegerDeclaration, PrintOp, AssignmentOp, PlusOp, MinusOp,
             MulOp, DivOp, Alphabet, IntValue, FloatValue, EOFsymbol, NullToken} TokenType;// GeniusPudding
typedef enum DataType { Int, Float, Notype }DataType;
typedef enum StmtType { Print, Assignment } StmtType;
typedef enum ValueType { Identifier, IntConst, FloatConst, PlusNode, MinusNode, MulNode, DivNode, IntToFloatConvertNode } ValueType;
typedef enum Operation { Plus, Minus, Mul, Div, Assign, IntToFloatConvert } Operation;


/**************************************************************************************** 
   All structures to facilitate the processes of 
   scanning, parsing, AST, type-checking, building the symbol table, and code generation.
*****************************************************************************************/


/* For scanner */
typedef struct Token{
    TokenType type;
    char tok[1025];
}Token;


// GeniusPudding, shiuld we run scanner once to get a token list?
typedef struct TokenListNode{
    Token tn;
    struct TokenListNode *lastNode;
    struct TokenListNode *nextNode;
}TokenListNode;

/*** The following are nodes of the AST. ***/

/* For decl production or say one declaration statement */
typedef struct Declaration{
    DataType type;
    char name[65]; // GeniusPudding
}Declaration;

/* 
    For decls production or say all declarations. (
	You can view it as the subtree for decls in AST,
	or just view it as the linked list that stores 
	all declarations. ) 
*/
typedef struct Declarations{
    Declaration first;
    struct Declarations *rest;
}Declarations;

/* For the nodes of the expression on the right hand side of one assignment statement */
typedef struct Value{// TODO: support id with at most 64 chars
    ValueType type;
    union{
        char id[65];                   /* if the node represent the access of the identifier */
        Operation op;              /* store +, -, *, /, =, type_convert */ 
        int ivalue;                /* for integer constant in the expression */
        float fvalue;              /* for float constant */
    }val;
    struct Value *nextInProduct; // GeniusPudding, for the Product(non-terminal) 
}Value;//Question: op matches ValueType, then why we need op???


/* 
   The data structure of the expression tree.
   Recall how to deal with expression by tree 
   in data structure course.   
*/
typedef struct Expression{
    Value v;
    struct Expression *leftOperand;
    struct Expression *rightOperand;
    DataType type;
}Expression;


/* For one assignment statement */
typedef struct AssignmentStatement{
    char id[65];
    Expression *expr;
    DataType type;      /* For type checking to store the type of all expression on the right. */
}AssignmentStatement;


/* For stmt production or say one statement*/
typedef struct Statement{
    StmtType type;
    union{
        char variable[65];              /* print statement */
        AssignmentStatement assign;
    }stmt;
}Statement;

/* For stmts production or say all productions */
typedef struct Statements{
    struct Statement first;
    struct Statements *rest;
}Statements;

/* For the root of the AST. */
typedef struct Program{
    Declarations *declarations;
    Statements *statements;
}Program;

/* For building the symbol table */
typedef struct SymbolTable{
    DataType table[26];
    char name[26][65];
    int count;
} SymbolTable;


Token getNumericToken( FILE *source, char c );
Token scanner( FILE *source );
Declaration makeDeclarationNode( Token declare_type, Token identifier );
Declarations *makeDeclarationTree( Declaration decl, Declarations *decls );
Declaration parseDeclaration( FILE *source, Token token );
Declarations *parseDeclarations( FILE *source );
Expression *parseValue( FILE *source );
Expression *parseExpressionTail( FILE *source, Expression *lvalue );
Expression *parseExpression( FILE *source, Expression *lvalue );
Statement makeAssignmentNode( char *id, Expression *v, Expression *expr_tail );
Statement makePrintNode( char *id );
Statements *makeStatementTree( Statement stmt, Statements *stmts );
Statement parseStatement( FILE *source, Token token );
Statements *parseStatements( FILE * source );
Program parser( FILE *source );
void InitializeTable( SymbolTable *table );
void add_table( SymbolTable *table, char *c, DataType t );
SymbolTable build( Program program );
void convertType( Expression * old, DataType type );
DataType generalize( Expression *left, Expression *right );
char lookup_id( SymbolTable *table,const char* n ); // partner
DataType lookup_table( SymbolTable *table, char *c );
void checkexpression( Expression * expr, SymbolTable * table );
void checkstmt( Statement *stmt, SymbolTable * table );
void check( Program *program, SymbolTable * table);
void fprint_op( FILE *target, ValueType op );
// void fprint_expr( FILE *target, Expression *expr );
// void gencode( Program prog, FILE * target );
void fprint_expr( FILE *target, Expression *expr, SymbolTable* table );
void gencode( Program prog, FILE * target, SymbolTable* table );


void print_expr( Expression *expr );
void test_parser( FILE *source );
void CheckProductInValue( FILE *source, Expression *value );// GeniusPudding
void fprint_product( FILE *target, Value tailProduct, SymbolTable* table );// GeniusPudding 
void folding_products(Expression *expr);// GeniusPudding 
void folding_sums(Expression *expr);// GeniusPudding 
#endif // HEADER_H_INCLUDED
