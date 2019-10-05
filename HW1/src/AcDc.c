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

Token scanner( FILE *source )// TODO: support variable name that not exceed 64 characters
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

        if( isdigit(c) )
            return getNumericToken(source, c);

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
            strncpy((value->v).val.id, token.tok, 65);//(value->v).val.id = token.tok[0];// TODO: wont be only 1 char
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

// GeniusPudding
void CheckProductInValue( FILE *source, Expression *value )
{
    (value->v).nextInProduct = NULL;
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
                return makeAssignmentNode(token.tok, value, expr);// TODO: wont be only 1 char
            }
            else{
                printf("Syntax Error: Expect an assignment op %s\n", next_token.tok);//Syntax Error description has Syntax Error?
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
    strncpy(tree_node.name, identifier.tok, 65);// tree_node.name = identifier.tok[0];

    return tree_node;
}

Declarations *makeDeclarationTree( Declaration decl, Declarations *decls )
{
    Declarations *new_tree = (Declarations *)malloc( sizeof(Declarations) );
    new_tree->first = decl;
    new_tree->rest = decls;

    return new_tree;
}


Statement makeAssignmentNode( char *id, Expression *v, Expression *expr_tail )// TODO: wont be only 1 char
{
    Statement stmt;
    AssignmentStatement assign;

    stmt.type = Assignment;
    strncpy(assign.id, id, 65);// assign.id = id;
    if(expr_tail == NULL)
        assign.expr = v;
    else
        assign.expr = expr_tail;
    stmt.stmt.assign = assign;

    return stmt;
}

Statement makePrintNode( char *id )
{
    Statement stmt;
    stmt.type = Print;
    strncpy(stmt.stmt.variable, id, 65);// stmt.stmt.variable = id;

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

    for(i = 0 ; i < 26; i++){
        table->table[i] = Notype;
        for (int j = 0; j < 65; ++j){
            table->name[i][j] = '\0';
        }
    }
    table->count = 0;

}

void add_table( SymbolTable *table, char *s, DataType t )
{
    // int index = (int)(c - 'a');

    // if(table->table[index] != Notype)
    //     printf("Error : id %c has been declared\n", c);//error
    // table->table[index] = t;
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
        printf("add id name:%s\n",current.name );
        add_table(&table, current.name, current.type);
        decls = decls->rest;
    }

    return table;
}


/********************************************************************
  Type checking
 *********************************************************************/

void convertType( Expression * old, DataType type )
{
    if(old->type == Float && type == Int){
        printf("error : can't convert float to integer in convertType\n");
        return;
    }
    if(old->type == Int && type == Float){
        Expression *tmp = (Expression *)malloc( sizeof(Expression) );
        if(old->v.type == Identifier)
            printf("convert to float %c \n",old->v.val.id);
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
        old->type = Int;
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

DataType lookup_table( SymbolTable *table, char *s )// TODO: support variable length of identifier's name
{
    // int id = c-'a';
    // if( table->table[id] != Int && table->table[id] != Float)
    //     printf("Error : identifier %c is not declared\n", c);//error
    // return table->table[id];
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
    // printf("looking n:%s\n",n );
    for (i=0;i<26;++i){
        // printf("table->name[%d]:%s\n",i,table->name[i] );
        if (strcmp(table->name[i], n) == 0)
            break;        
    }


    // printf("lookup_id:%d,char:%c\n",i,i+'a' );
    return i+'a';
}

void checkexpression( Expression * expr, SymbolTable * table )
{
    char *c;
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

        // GeniusPudding, fold products here
        folding_products(expr, table);

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

        // GeniusPudding, fold sums here after the product folded in leaf nodes
        folding_sums(expr);

    }
}

void folding_products(Expression *expr, SymbolTable *table)//expr belongs to value here, mind the int/float type
{
    if((expr->v).type!=Identifier && (expr->v).nextInProduct!=NULL){
        int iProduct = 1;
        float fProduct = 1.0;
        int productType = 0;// 0:Int, 1:Float
        if((expr->v).type==FloatConst){
            productType = 1;
            fProduct = (expr->v).val.fvalue;
            printf("%f\n",(expr->v).val.fvalue);
        }else{
            iProduct = (expr->v).val.ivalue;
            printf("%d\n",(expr->v).val.ivalue);
        }

        // traverse all the values in the product
        int continuous = 1;
        Value *currentOperator = (expr->v).nextInProduct;
        Value *currentValue = currentOperator->nextInProduct;
        while(continuous){
            printf("currentOperator->type:%d\n",currentOperator->type );
            printf("currentValue->type:%d\n",currentValue->type );
            switch(currentValue->type){
                case Identifier:
                    printf("Got Identifier:%s\n",lookup_id(table, (currentValue->val).id));
                    continuous = 0;
                    break;
                case IntConst:
                    if(productType==0){
                        if(currentOperator->type==MulNode){
                            iProduct *= (currentValue->val).ivalue;
                        }else if(currentOperator->type==DivNode){
                            iProduct /= (currentValue->val).ivalue;
                        }
                        printf("%d\n", iProduct);
                    }else{
                        if(currentOperator->type==MulNode){
                            fProduct *= (float)(currentValue->val).ivalue;
                        }else if(currentOperator->type==DivNode){
                            fProduct /= (float)(currentValue->val).ivalue;
                        }      
                        printf("%f\n", fProduct);                 
                    } 
                    if(currentValue->nextInProduct!=NULL){
                        currentOperator = currentValue->nextInProduct;  
                        free(currentValue);
                        currentValue = currentOperator->nextInProduct;     
                        free(currentOperator);                   
                    }else{
                        continuous = 0;
                    }

                    printf("currentOperator->type:%d\n",currentOperator->type );
                    printf("currentValue->type:%d\n",currentValue->type );
                    break;
                case FloatConst:    
                    if(productType==0){
                        productType = 1;
                        fProduct = (float)iProduct;
                        if(currentOperator->type==MulNode){
                            fProduct *= (currentValue->val).fvalue;
                        }else if(currentOperator->type==DivNode){
                            fProduct /= (currentValue->val).fvalue;
                        }
                        printf("%f\n", fProduct);    
                        
                    }else{
                        if(currentOperator->type==MulNode){
                            fProduct *= (currentValue->val).fvalue;
                        }else if(currentOperator->type==DivNode){
                            fProduct /= (currentValue->val).fvalue;
                        }                      
                        printf("%f\n", fProduct);    
                    }
                    if(currentValue->nextInProduct!=NULL){
                        currentOperator = currentValue->nextInProduct;  
                        free(currentValue);
                        currentValue = currentOperator->nextInProduct;     
                        free(currentOperator);                            
                    }else{
                        continuous = 0;
                    }

                    printf("currentOperator->type:%d\n",currentOperator->type );
                    printf("currentValue->type:%d\n",currentValue->type );                               
                    break;
                default:
                    break;
            }

        };

        //set the merge node
        if(currentValue->type==Identifier){//coefficient followed by Identifier     
            (expr->v).nextInProduct = currentOperator;
            printf("currentOperator before Identifier:%d\n",currentOperator->type );
        }else{// all merged as a constant
            (expr->v).type = productType + 1;
            (expr->v).nextInProduct = NULL;
        }
        if(productType==0){
            (expr->v).val.ivalue = iProduct; 
        }else if(productType==1){
            (expr->v).val.fvalue = fProduct;
        }
        
    }
}

void folding_sums(Expression *expr)
{
    Expression *left = expr->leftOperand;
    Expression *right = expr->rightOperand;
    // printf("(expr->v).type:%d\n",(expr->v).type );
    // printf("(right->v).nextInProduct:%p\n",(right->v).nextInProduct );
    //check if need constant-folding
    if(((left->v).type==IntConst||(left->v).type==FloatConst)&&(left->v).nextInProduct==NULL&&((right->v).type==IntConst||(right->v).type==FloatConst)&&(right->v).nextInProduct==NULL ){
        printf("Two constant leaf!\n");
        int iSum = 0;
        float fSum = 0.0;
        int sumType = 1;
        if((left->v).type==IntConst&&(right->v).type==IntConst){
            sumType = 0;
            // printf("(left->v).val.ivalue:%d,(right->v).val.ivalue:%d\n",(left->v).val.ivalue,(right->v).val.ivalue );
            iSum = (left->v).val.ivalue + (right->v).val.ivalue;
        }else if((left->v).type==IntConst&&(right->v).type==FloatConst){
            // printf("(float)(left->v).val.ivalue:%f,(right->v).val.fvalue:%f\n",(float)(left->v).val.ivalue,(right->v).val.fvalue );
            fSum = (float)(left->v).val.ivalue + (right->v).val.fvalue; 
        }else if((left->v).type==FloatConst&&(right->v).type==IntConst){
            // printf("(left->v).val.fvalue:%f,(float)(right->v).val.ivalue:%f\n",(left->v).val.fvalue,(float)(right->v).val.ivalue );    
            fSum = (left->v).val.fvalue + (float)(right->v).val.ivalue; 
        }else if((left->v).type==FloatConst&&(right->v).type==FloatConst){
            // printf("f (left->v).val.fvalue:%f,(right->v).val.fvalue:%f\n",(left->v).val.fvalue,(right->v).val.fvalue );
            fSum = (left->v).val.fvalue + (right->v).val.fvalue; 
        }

        free(left);
        free(right);
        //assign values to expr
        expr->leftOperand = expr->rightOperand = NULL;
        (expr->v).nextInProduct = NULL;
        (expr->v).type = sumType + 1;
        if(sumType==0){
            (expr->v).val.ivalue = iSum; 
        }else if(sumType==1){
            (expr->v).val.fvalue = fSum;
        }
        // printf("iSum:%d,fSum:%f\n",iSum,fSum );
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
            printf("error : can't convert float to integer in checkstmt\n");
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
    printf("In fprint_op, type=%d\n",op);
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

void fprint_expr( FILE *target, Expression *expr, SymbolTable* table)// TODO: fprint Values in Products
{
    // printf("fprinting:%d\n",(expr->v).type);
    if(expr->leftOperand == NULL){
        switch( (expr->v).type ){
            case Identifier:
                fprintf(target,"l%c\n", lookup_id(table, (expr->v).val.id));// fprintf(target,"l%c\n",(expr->v).val.id);
                break;
            case IntConst:
                if (expr->v.val.ivalue < 0)
                    fprintf(target,"0\n%d\n-",-(expr->v).val.ivalue);
                else
                    fprintf(target,"%d\n",(expr->v).val.ivalue);
                // fprintf(target,"%d\n",(expr->v).val.ivalue);
                break;
            case FloatConst:
                if (expr->v.val.fvalue < 0)
                    fprintf(target,"0.0\n%f\n-", -(expr->v).val.fvalue);
                else
                    fprintf(target,"%f\n", (expr->v).val.fvalue);
                // fprintf(target,"%f\n", (expr->v).val.fvalue);
                break;
            default:
                fprintf(target,"Error In fprint_left_expr. (expr->v).type=%d in fprint_expr\n",(expr->v).type);
                break;
        }

        // GeniusPudding
        fprint_product(target, (expr->v), table);
    }
    else{
        fprint_expr(target, expr->leftOperand, table);
        if(expr->rightOperand == NULL){
            fprintf(target,"5k\n");
        }
        else{
            //	fprint_right_expr(expr->rightOperand);
            fprint_expr(target, expr->rightOperand, table);
            fprint_op(target, (expr->v).type);
        }
    }
}

// GeniusPudding
void fprint_product( FILE *target, Value tailProduct, SymbolTable *table)
{
    printf("In fprint_product tailProduct.type:%d\n",tailProduct.type);
    if(tailProduct.nextInProduct!=NULL && (tailProduct.nextInProduct->type==MulNode||tailProduct.nextInProduct->type==DivNode)){
        Value *operator = tailProduct.nextInProduct;
        tailProduct = *(operator->nextInProduct);//next value
        switch( tailProduct.type ){
            case Identifier:
                fprintf(target,"l%c\n", lookup_id(table, tailProduct.val.id));//   ,tailProduct.val.id);
                break;
            case IntConst:
                fprintf(target,"%d\n",tailProduct.val.ivalue);
                break;
            case FloatConst:
                fprintf(target,"%f\n", tailProduct.val.fvalue);
                break;
            default:
                fprintf(target,"Error In fprint_left_expr. (expr->v).type=%d in fprint_product\n",tailProduct.type);
                break;
        }
        printf("operator->type:%d\n",operator->type );
        fprint_op(target, operator->type);
        fprint_product(target, tailProduct, table); // recursive to gen stack language
        
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
                fprintf(target,"l%c\n", lookup_id(table, stmt.stmt.variable));//,stmt.stmt.variable);
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
                fprintf(target,"s%c\n",lookup_id(table, stmt.stmt.assign.id));//,stmt.stmt.assign.id);
                // fprintf(target,"0 k\n");
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
                printf("%c ", (expr->v).val.id);
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
                printf("error ");
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
