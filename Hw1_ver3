#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "header.h"

// GeniusPudding
Token temporaryToken = {.type = NullToken};//assigns value when scanned token unused  
// Add CFGs:
// Stmt -> id assign Prod Expr
// Expr -> plus Prod Expr
//       | minus Prod Expr
// Prod -> Val multiple Prod
//       | Val divide Prod
//       | Val
 
// TODO: check divide-by-zero after constant folding 
// Probable problem: precision


int main( int argc, char *argv[] )
{
    FILE *source, *target;
    Program program;
    SymbolTable symtab;

    if( argc == 3){
        source = fopen(argv[1], "r");
        target = fopen(argv[2], "w");
        if( !source ){
            printf("can't open the source file\n");
            exit(2);
        }
        else if( !target ){
            printf("can't open the target file\n");
            exit(2);
        }
        else{
            program = parser(source);
            fclose(source);
            symtab = build(program);
            check(&program, &symtab); 
            constfolds(program.statements);
            gencode(program, target, &symtab);

        }
    }
    else{
        printf("Usage: %s source_file target_file\n", argv[0]);
    }


    return 0;
}


/********************************************* 
  Scanning 
 *********************************************/
Token getNumericToken( FILE *source, char c )
{
    Token token;
    int i = 0;

    while( isdigit(c) ) {
        token.tok[i++] = c;
        c = (char)fgetc(source);
    }

    if( c != '.' ){
        ungetc(c, source);
        token.tok[i] = '\0';
        token.type = IntValue;
        return token;
    }

    token.tok[i++] = '.';

    c = (char)fgetc(source);
    if( !isdigit(c) ){
        ungetc(c, source);
        printf("Expect a digit : %c\n", c);
        exit(1);
    }

    while( isdigit(c) ){
        token.tok[i++] = c;
        c = (char)fgetc(source);
    }

    ungetc(c, source);
    token.tok[i] = '\0';
    token.type = FloatValue;
    return token;
}

Token getAlphabet( FILE *source, char c ) {
    int i = 0;
    Token token = {.type = Alphabet};
    //printf("%s",(token));
    token.tok[i++] = c;
    while (islower(c = (char)fgetc(source))) {
        token.tok[i++] = c;
    }
    ungetc(c, source);
    token.tok[i] = '\0';
    return token;
}
Token scanner( FILE *source )
{
    char c;
    Token token;

    // GeniusPudding   
    if (temporaryToken.type != NullToken){
        printf("Token temporary is not empty!\n");
        token = temporaryToken;
        temporaryToken.type = NullToken;
        return token;
    }

    while( !feof(source) ){
        c = (char)fgetc(source);

        while( isspace(c) ) c = (char)fgetc(source);

        if( isdigit(c) ) {
            return getNumericToken(source, c);
        }
        token.tok[0] = c;
        token.tok[1] = '\0';
        if( islower(c) ){
            char nc = (char)fgetc(source);
            if( c == 'f' && isspace(nc)){
                //printf("%d",nc);
                token.type = FloatDeclaration;
            }
            else if( c == 'i' && isspace(nc)){
                token.type = IntegerDeclaration;
            }
            else if( c == 'p' && isspace(nc)){
                strcpy(token.tok, "p ");            
                token.type = PrintOp;
            }
            else{
                //printf("%d",(char)ungetc(nc, source));
                ungetc(nc, source);
                token = getAlphabet(source, c);
            }
            return token;
        }

        switch(c){
            case '=':
                token.type = AssignmentOp;
                return token;
            case '+':
                token.type = PlusOp;
                return token;
            case '-':
                token.type = MinusOp;
                return token;
            case '*':
                token.type = MulOp;
                return token;
            case '/':
                token.type = DivOp;
                return token;
            case EOF:
                token.type = EOFsymbol;
                token.tok[0] = '\0';
                return token;
            default:
                printf("Invalid character : %c\n", c);
                exit(1);
        }
    }
    token.tok[0] = '\0';
    token.type = EOFsymbol;   
    return token;
}


/********************************************************
  Parsing
 *********************************************************/
/*
Syntax:

Expr : Value PlusOp  Expr
     | Value MinusOp Expr
     | ExprTail
ExprTail : Value MulOp ExprTail
         | Value DivOp ExprTail
         | Value
*/
Declaration parseDeclaration( FILE *source, Token token )
{
    Token token2;
    switch(token.type){
        case FloatDeclaration:
        case IntegerDeclaration:
            token2 = scanner(source);
            if (strcmp(token2.tok, "f") == 0 ||
                    strcmp(token2.tok, "i") == 0 ||
                    strcmp(token2.tok, "p") == 0) {
                printf("Syntax Error: %s cannot be used as id\n", token2.tok);
                exit(1);
            }
            return makeDeclarationNode( token, token2 );
        default:
            printf("Syntax Error: Expect Declaration %s\n", token.tok);
            exit(1);
    }
}

Declarations *parseDeclarations( FILE *source )
{
    Token token = scanner(source);
    Declaration decl;
    Declarations *decls;
    switch(token.type){
        case FloatDeclaration:
        case IntegerDeclaration:
            decl = parseDeclaration(source, token);
            decls = parseDeclarations(source);
            return makeDeclarationTree( decl, decls );
        case PrintOp:
        case Alphabet:
            for(int i=strlen(token.tok)-1;i>=0;--i)  
              ungetc(token.tok[i], source);
            return NULL;
        case EOFsymbol:
            return NULL;
        default:
            printf("Syntax Error: Expect declarations %s\n", token.tok);
            exit(1);
    }
}

Expression *parseValue( FILE *source )
{
    Token token = scanner(source);
    Expression *value = (Expression *)malloc( sizeof(Expression) );
    value->leftOperand = value->rightOperand = NULL;

    switch(token.type){
        case Alphabet:
            (value->v).type = Identifier;
            strncpy((value->v).val.id, token.tok, 512);
            break;
        case IntValue:
            (value->v).type = IntConst;
            (value->v).val.ivalue = atoi(token.tok);
            break;
        case FloatValue:
            (value->v).type = FloatConst;
            (value->v).val.fvalue = atof(token.tok);
            break;
        default:
            printf("Syntax Error: Expect Identifier or a Number %s\n", token.tok);
            exit(1);
    }
    CheckProductInValue(source, value);// GeniusPudding

    return value;
}

void CheckProductInValue( FILE *source, Expression *value )
{
    // guess next operator 
    Token operatorToken, valueToken;
 
    operatorToken = scanner(source);
    printf("DEBUG: operatorToken.type=%d, operatorToken.tok=%s\n",operatorToken.type,operatorToken.tok);
    Value *operator = (Value *)malloc( sizeof(Value) );// only care about the Value type
    switch(operatorToken.type){
        case MulOp:
        case DivOp:
            
            (value->v).nextInProduct = operator;// all Product content listed in only An Expression: value
 
            Expression *tailValue = parseValue(source);// recursion for tail 
            operator->nextInProduct = &(tailValue->v);
 
            if(operatorToken.type == MulOp){
                operator->type = MulNode;
                (operator->val).op = Mul;
            }else{//DivOp, check divide by 0 here 
                if((operator->nextInProduct->type==IntConst&&(operator->nextInProduct->val).ivalue==0)||(operator->nextInProduct->type==FloatConst&&(operator->nextInProduct->val).fvalue==0.0)){
                    printf("Divide by 0!\n");
                    exit(1);                    
                }
 
                operator->type = DivNode;
                (operator->val).op = Div;
            } 
            break;            
        default:
            temporaryToken = operatorToken;
    }
    return; 
}

Expression *parseExpressionTail( FILE *source, Expression *lvalue )
{
    Token token = scanner(source);
    Expression *expr;
    switch(token.type){
        case PlusOp:
            expr = (Expression *)malloc( sizeof(Expression) );
            (expr->v).type = PlusNode;
            (expr->v).val.op = Plus;
            expr->leftOperand = lvalue;
            expr->rightOperand = parseValue(source);
            return parseExpressionTail(source, expr);
        case MinusOp:
            expr = (Expression *)malloc( sizeof(Expression) );
            (expr->v).type = MinusNode;
            (expr->v).val.op = Minus;
            expr->leftOperand = lvalue;
            expr->rightOperand = parseValue(source);
            return parseExpressionTail(source, expr);
        case Alphabet:
        case PrintOp:
            for(int i=strlen(token.tok)-1;i>=0;--i) 
              ungetc(token.tok[i], source);
            return lvalue;
        case EOFsymbol:
            return lvalue;
        default:
            printf("Syntax Error: Expect a numeric value or an identifier %s\n", token.tok);
            exit(1);
    }
}

Expression *parseExpression( FILE *source, Expression *lvalue )
{
    Token token = scanner(source);
    Expression *expr;
    switch(token.type){
        case PlusOp:
            expr = (Expression *)malloc( sizeof(Expression) );
            (expr->v).type = PlusNode;
            (expr->v).val.op = Plus;
            expr->leftOperand = lvalue;
            expr->rightOperand = parseValue(source);
            return parseExpressionTail(source, expr);
        case MinusOp:
            expr = (Expression *)malloc( sizeof(Expression) );
            (expr->v).type = MinusNode;
            (expr->v).val.op = Minus;
            expr->leftOperand = lvalue;
            expr->rightOperand = parseValue(source);
            return parseExpressionTail(source, expr);
        case Alphabet:
        case PrintOp:
            for(int i=strlen(token.tok)-1;i>=0;--i) 
              ungetc(token.tok[i], source);
            return NULL;
        case EOFsymbol:
            return NULL;
        default:
            printf("Syntax Error: Expect a numeric value or an identifier %s\n", token.tok);
            exit(1);
    }
}

Statement parseStatement( FILE *source, Token token )
{
    Token next_token;
    Expression *value, *expr;
    printf("parseStatement token.type:%d, token.tok:%s\n",(int)token.type,token.tok);

    switch(token.type){
        case Alphabet:
            next_token = scanner(source);
            printf("parseStatement next_token.type:%d, next_token.tok:%s\n",(int)next_token.type,next_token.tok);
            if(next_token.type == AssignmentOp){
                value = parseValue(source);
                expr = parseExpression(source, value);
                if (expr != NULL)
                    printf("value:%d,expr:%d\n",(int)(value->v).type, (int)(expr->v).type);
                else
                    printf("value:%d\n",(int)(value->v).type);
                return makeAssignmentNode(token.tok, value, expr);
            }
            else{
                printf("Syntax Error: Expect an assignment op %s\n", next_token.tok);
                exit(1);
            }
        case PrintOp:
            next_token = scanner(source);
            if(next_token.type == Alphabet)
                return makePrintNode(next_token.tok);
            else{
                printf("Syntax Error: Expect an identifier %s\n", next_token.tok);
                exit(1);
            }
            break;
        default:
            printf("Syntax Error: Expect a statement %s\n", token.tok);
            exit(1);
    }
}

Statements *parseStatements( FILE * source )
{

    Token token = scanner(source);
    Statement stmt;
    Statements *stmts;

    switch(token.type){
        case Alphabet:
        case PrintOp:
            stmt = parseStatement(source, token);
            stmts = parseStatements(source);
            return makeStatementTree(stmt , stmts);
        case EOFsymbol:
            return NULL;
        default:
            printf("Syntax Error: Expect statements %s\n", token.tok);
            exit(1);
    }
}


/*********************************************************************
  Build AST
 **********************************************************************/
Declaration makeDeclarationNode( Token declare_type, Token identifier )
{
    Declaration tree_node;

    switch(declare_type.type){
        case FloatDeclaration:
            tree_node.type = Float;
            break;
        case IntegerDeclaration:
            tree_node.type = Int;
            break;
        default:
            break;
    }
    strncpy(tree_node.name, identifier.tok, 512);

    return tree_node;
}

Declarations *makeDeclarationTree(Declaration decl, Declarations *decls )
{
    Declarations *new_tree = (Declarations *)malloc( sizeof(Declarations) );
    new_tree->first = decl;
    new_tree->rest = decls;

    return new_tree;
}


Statement makeAssignmentNode(const char* id, Expression *v, Expression *expr_tail )
{
    Statement stmt;
    AssignmentStatement assign;

    stmt.type = Assignment;
    strncpy(assign.id, id, 512);
    if(expr_tail == NULL)
        assign.expr = v;
    else
        assign.expr = expr_tail;
    stmt.stmt.assign = assign;

    return stmt;
}

Statement makePrintNode(const char* id )
{
    Statement stmt;
    stmt.type = Print;
    strncpy(stmt.stmt.variable, id, 512);

    return stmt;
}

Statements *makeStatementTree( Statement stmt, Statements *stmts )
{
    Statements *new_tree = (Statements *)malloc( sizeof(Statements) );
    new_tree->first = stmt;
    new_tree->rest = stmts;

    return new_tree;
}


/* parser */
Program parser( FILE *source )
{
    Program program;

    program.declarations = parseDeclarations(source);
    program.statements = parseStatements(source);

    return program;
}


/********************************************************
  Build symbol table
 *********************************************************/
void InitializeTable( SymbolTable *table )
{
    int i;
    for(i = 0 ; i < 26; i++) {
        table->table[i] = Notype;
        table->name[i][0] = '\0';
    }
    table->count = 0;
}

void add_table( SymbolTable *table, char *s, DataType t )
{
    int i;
    for (i = 0; i < table->count; i++) {
        if(strcmp(s, table->name[i]) == 0) {
            printf("Error : id %s has been declared\n", s);
        }
    }
    table->count++;
    strcpy(table->name[i], s);
    table->table[i] = t;
}

SymbolTable build( Program program )
{
    SymbolTable table;
    Declarations *decls = program.declarations;
    Declaration current;

    InitializeTable(&table);

    while(decls !=NULL){
        current = decls->first;
        add_table(&table, current.name, current.type);
        decls = decls->rest;
    }

    return table;
}
/*constant folding*/
void constfold(Expression *node)
{
    if(!node) return;
    Expression *lnode=node->leftOperand;
    Expression *rnode=node->rightOperand;
    //DataType type = generalize(lnode, rnode);

    while((node->leftOperand != NULL)&&(node->rightOperand != NULL))
    {
        constfold(lnode);
        constfold(rnode);
        if((node->v.type==PlusNode))
        {
            if(((lnode->v.type) == IntConst)&&((rnode->v.type) == IntConst))
            {
                node->v.val.ivalue = lnode->v.val.ivalue + rnode->v.val.ivalue;
                printf("%s:", "fold the node: ");
                printf("%d",lnode->v.val.ivalue);
                printf("+");
                printf("%d=",rnode->v.val.ivalue);
                printf("%d\n",node->v.val.ivalue);
                node->v.type = IntConst;
                
            }
            else if(((lnode->v.type) == FloatConst)&&((rnode->v.type) == FloatConst))
            {
                node->v.val.fvalue = lnode->v.val.fvalue + rnode->v.val.fvalue;
                printf("%s:", "fold the node: ");
                printf("%f",lnode->v.val.fvalue);
                printf("+");
                printf("%f=",rnode->v.val.fvalue);
                printf("%f\n",node->v.val.fvalue);
                node->v.type = FloatConst;
            }
            free(lnode);
            free(rnode);
            node->leftOperand = NULL;
            node->rightOperand = NULL;
        }
        else if(node->v.type==MinusNode)
        {
            if(((lnode->v.type) == IntConst)&&((rnode->v.type) == IntConst))
            {
                node->v.val.ivalue = lnode->v.val.ivalue - rnode->v.val.ivalue;
                printf("%s:", "fold the node: ");
                printf("%d",lnode->v.val.ivalue);
                printf("-");
                printf("%d=",rnode->v.val.ivalue);
                printf("%d\n",node->v.val.ivalue);
                node->v.type = IntConst;
            }
            else if(((lnode->v.type) == FloatConst)&&((rnode->v.type) == FloatConst))
            {
                node->v.val.fvalue = lnode->v.val.fvalue - rnode->v.val.fvalue;
                printf("%s:", "fold the node: ");
                printf("%f",lnode->v.val.fvalue);
                printf("-");
                printf("%f=",rnode->v.val.fvalue);
                printf("%f\n",node->v.val.fvalue);
                node->v.type = FloatConst;
            }
            free(lnode);
            free(rnode);
            node->leftOperand = NULL;
            node->rightOperand = NULL;
        }
        else if(node->v.type==MulNode)
        {
            if(((lnode->v.type) == IntConst)&&((rnode->v.type) == IntConst))
            {
                node->v.val.ivalue = lnode->v.val.ivalue * rnode->v.val.ivalue;
                printf("%s:", "fold the node: ");
                printf("%d",lnode->v.val.ivalue);
                printf("*");
                printf("%d=",rnode->v.val.ivalue);
                printf("%d\n",node->v.val.ivalue);
                node->v.type = IntConst;
            }
            else if(((lnode->v.type) == FloatConst)&&((rnode->v.type) == FloatConst))
            {
                node->v.val.fvalue = lnode->v.val.fvalue * rnode->v.val.fvalue;
                printf("%s:", "fold the node: ");
                printf("%f",lnode->v.val.fvalue);
                printf("*");
                printf("%f=",rnode->v.val.fvalue);
                printf("%f\n",node->v.val.fvalue);
                node->v.type = FloatConst;
            }
            free(lnode);
            free(rnode);
            node->leftOperand = NULL;
            node->rightOperand = NULL;
        }
        else if(node->v.type==DivNode)
        {
            if(((lnode->v.type) == IntConst)&&((rnode->v.type) == IntConst))
            {
                node->v.val.ivalue = lnode->v.val.ivalue / rnode->v.val.ivalue;
                printf("%s:", "fold the node: ");
                printf("%d",lnode->v.val.ivalue);
                printf("/");
                printf("%d=",rnode->v.val.ivalue);
                printf("%d\n",node->v.val.ivalue);
                node->v.type = IntConst;
            }
            else if(((lnode->v.type) == FloatConst)&&((rnode->v.type) == FloatConst))
            {
                node->v.val.fvalue = lnode->v.val.fvalue / rnode->v.val.fvalue;
                printf("%s:", "fold the node: ");
                printf("%f",lnode->v.val.fvalue);
                printf("/");
                printf("%f=",rnode->v.val.fvalue);
                printf("%f\n",node->v.val.fvalue);
                node->v.type = FloatConst;
            }
            free(lnode);
            free(rnode);
            node->leftOperand = NULL;
            node->rightOperand = NULL;
        }
        else if(node->v.type==Identifier)
        {
            printf("%s","8");
        }
    } 
}

void constfolds(Statements *node)
{
    while(node)
    { 
        constfold(node->first.stmt.assign.expr);
        node = node->rest;
    }
}
/********************************************************************
  Type checking
 *********************************************************************/

void convertType( Expression * old, DataType type )
{
    if(old->type == Float && type == Int){
        printf("error : can't convert float to integer\n");
        return;
    }
    if(old->type == Int && type == Float){
        Expression *tmp = (Expression *)malloc( sizeof(Expression) );
        if(old->v.type == Identifier)
            printf("convert to float %s \n",old->v.val.id);
        else
            printf("convert to float %d \n", old->v.val.ivalue);
        tmp->v = old->v;
        tmp->leftOperand = old->leftOperand;
        tmp->rightOperand = old->rightOperand;
        tmp->type = old->type;

        Value v;
        v.type = IntToFloatConvertNode;
        v.val.op = IntToFloatConvert;
        old->v = v;
        old->type = Float;
        old->leftOperand = tmp;
        old->rightOperand = NULL;
    }
}

DataType generalize( Expression *left, Expression *right )
{
    if(left->type == Float || right->type == Float){
        printf("generalize : float\n");
        return Float;
    }
    printf("generalize : int\n");
    return Int;
}

DataType lookup_table( SymbolTable *table,const char *s )
{
    for (int i = 0; i < table->count; i++) {
        if(strcmp(s, table->name[i]) == 0) {
            return table->table[i];
        }
    }
    printf("Error : identifier %s is not declared\n", s);
    return Notype;
   
}
char lookup_id( SymbolTable *table,const char* n )
{
    int i;
    for (i=0;i<26;++i)
      if (strcmp(table->name[i], n) == 0)
        break;
    return i;
}

void checkexpression( Expression * expr, SymbolTable * table )
{
    const char* c;
    if(expr->leftOperand == NULL && expr->rightOperand == NULL){
        switch(expr->v.type){
            case Identifier:
                c = expr->v.val.id;
                printf("identifier : %s\n",c);
                expr->type = lookup_table(table, c);
                break;
            case IntConst:
                printf("constant : int\n");
                expr->type = Int;
                break;
            case FloatConst:
                printf("constant : float\n");
                expr->type = Float;
                break;
                //case PlusNode: case MinusNode: case MulNode: case DivNode:
            default:
                break;
        }
    }
    else{
        Expression *left = expr->leftOperand;
        Expression *right = expr->rightOperand;

        checkexpression(left, table);
        checkexpression(right, table);

        DataType type = generalize(left, right);
        convertType(left, type);//left->type = type;//converto
        convertType(right, type);//right->type = type;//converto
        expr->type = type;
    }
}

void checkstmt( Statement *stmt, SymbolTable * table )
{
    if(stmt->type == Assignment){
        AssignmentStatement assign = stmt->stmt.assign;
        printf("assignment : %s \n",assign.id);
        checkexpression(assign.expr, table);
        stmt->stmt.assign.type = lookup_table(table, assign.id);
        if (assign.expr->type == Float && stmt->stmt.assign.type == Int) {
            printf("error : can't convert float to integer\n");
        } else {
            convertType(assign.expr, stmt->stmt.assign.type);
        }
    }
    else if (stmt->type == Print){
        printf("print : %s \n",stmt->stmt.variable);
        lookup_table(table, stmt->stmt.variable);
    }
    else printf("error : statement error\n");//error
}

void check( Program *program, SymbolTable * table )
{
    Statements *stmts = program->statements;
    while(stmts != NULL){
        checkstmt(&stmts->first,table);
        stmts = stmts->rest;
    }
}


/***********************************************************************
  Code generation
 ************************************************************************/
void fprint_op( FILE *target, ValueType op )
{
    switch(op){
        case MinusNode:
            fprintf(target,"-\n");
            break;
        case PlusNode:
            fprintf(target,"+\n");
            break;
        // GeniusPudding
        case MulNode:
            fprintf(target,"*\n");
            break;
        case DivNode:
            fprintf(target,"/\n");
            break;
        default:
            fprintf(target,"Error in fprintf_op ValueType = %d\n",op);
            break;
    }
}
 
void fprint_expr( FILE *target, Expression *expr, SymbolTable* table)
{
 
    if(expr->leftOperand == NULL){
        switch( (expr->v).type ){
            case Identifier:
                fprintf(target,"l%c\n", lookup_id(table, (expr->v).val.id));
                break;
            case IntConst:
                if (expr->v.val.ivalue < 0)
                    fprintf(target,"0\n%d\n-",-(expr->v).val.ivalue);
                else
                    fprintf(target,"%d\n",(expr->v).val.ivalue);
                break;
            case FloatConst:
                if (expr->v.val.fvalue < 0)
                    fprintf(target,"0.0\n%f\n-", -(expr->v).val.fvalue);
                else
                    fprintf(target,"%f\n", (expr->v).val.fvalue);
                break;
            default:
                fprintf(target,"Error In fprint_left_expr. (expr->v).type=%d\n",(expr->v).type);
                break;
        }
         // GeniusPudding
        fprint_product(target, (expr->v));
    }
    else{
        fprint_expr(target, expr->leftOperand, table);
        if(expr->rightOperand == NULL){
            fprintf(target,"5k\n");
        }
        else{
            //fprint_right_expr(expr->rightOperand);
            fprint_expr(target, expr->rightOperand, table);
            fprint_op(target, (expr->v).type);
        }
    }
}

 
// GeniusPudding
void fprint_product( FILE *target, Value tailProduct)
{
    if(tailProduct.nextInProduct!=NULL && (tailProduct.nextInProduct->type==MulNode||tailProduct.nextInProduct->type==DivNode)){
        Value *operator = tailProduct.nextInProduct;
        tailProduct = *(operator->nextInProduct);//next value
        switch( tailProduct.type ){
            case Identifier:
                fprintf(target,"l%s\n",tailProduct.val.id);
                break;
            case IntConst:
                fprintf(target,"%d\n",tailProduct.val.ivalue);
                break;
            case FloatConst:
                fprintf(target,"%f\n", tailProduct.val.fvalue);
                break;
            default:
                fprintf(target,"Error In fprint_left_expr. (expr->v).type=%d\n",tailProduct.type);
                break;
        }
        fprint_op(target, operator->type);
        fprint_product(target, tailProduct); // recursive to gen stack language
        
    }
    return;
} 
 
 
void gencode(Program prog, FILE * target, SymbolTable* table)
{
    Statements *stmts = prog.statements;
    Statement stmt;
 
    while(stmts != NULL){
        stmt = stmts->first;
        switch(stmt.type){
            case Print:
                fprintf(target,"l%c\n", lookup_id(table, stmt.stmt.variable));
                fprintf(target,"p\n");
                break;
            case Assignment:
                fprint_expr(target, stmt.stmt.assign.expr, table);
                /*
                   if(stmt.stmt.assign.type == Int){
                   fprintf(target,"0 k\n");
                   }
                   else if(stmt.stmt.assign.type == Float){
                   fprintf(target,"5 k\n");
                   }*/
                fprintf(target,"s%c\n",lookup_id(table, stmt.stmt.assign.id));
                fprintf(target,"0 k\n");
                break;
        }
        stmts=stmts->rest;
    }
 
}
/***************************************
  For our debug,
  you can omit them.
 ****************************************/
void print_expr(Expression *expr)
{
    if(expr == NULL)
        return;
    else{
        print_expr(expr->leftOperand);
        switch((expr->v).type){
            case Identifier:
                printf("%s ", (expr->v).val.id);
                break;
            case IntConst:
                printf("%d ", (expr->v).val.ivalue);
                break;
            case FloatConst:
                printf("%f ", (expr->v).val.fvalue);
                break;
            case PlusNode:
                printf("+ ");
                break;
            case MinusNode:
                printf("- ");
                break;
            case MulNode:
                printf("* ");
                break;
            case DivNode:
                printf("/ ");
                break;
            case IntToFloatConvertNode:
                printf("(float) ");
                break;
            default:
                printf("error %d\n", expr->v.type);
                break;
        }
        print_expr(expr->rightOperand);
    }
}

void test_parser( FILE *source )
{
    Declarations *decls;
    Statements *stmts;
    Declaration decl;
    Statement stmt;
    Program program = parser(source);

    decls = program.declarations;

    while(decls != NULL){
        decl = decls->first;
        if(decl.type == Int)
            printf("i ");
        if(decl.type == Float)
            printf("f ");
        printf("%s ",decl.name);
        decls = decls->rest;
    }

    stmts = program.statements;

    while(stmts != NULL){
        stmt = stmts->first;
        if(stmt.type == Print){
            printf("p %s ", stmt.stmt.variable);
        }

        if(stmt.type == Assignment){
            printf("%s = ", stmt.stmt.assign.id);
            print_expr(stmt.stmt.assign.expr);
        }
        stmts = stmts->rest;
    }

}
