#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "header.h"
#include "symbolTable.h"
// This file is for reference only, you are not required to follow the implementation. //
// You only need to check for errors stated in the hw4 document. //
int g_anyErrorOccur = 0;

DATA_TYPE getBiggerType(DATA_TYPE dataType1, DATA_TYPE dataType2);
void processProgramNode(AST_NODE *programNode);
void processDeclarationNode(AST_NODE* declarationNode);
void declareIdList(AST_NODE* typeNode, SymbolAttributeKind isVariableOrTypeAttribute, int ignoreArrayFirstDimSize);
void declareFunction(AST_NODE* returnTypeNode);
void processDeclDimList(AST_NODE* variableDeclDimList, TypeDescriptor* typeDescriptor, int ignoreFirstDimSize);
void processTypeNode(AST_NODE* typeNode);
void processBlockNode(AST_NODE* blockNode);
void processStmtNode(AST_NODE* stmtNode);
void processGeneralNode(AST_NODE *node);
void checkAssignOrExpr(AST_NODE* assignOrExprRelatedNode);
void checkWhileStmt(AST_NODE* whileNode);
void checkForStmt(AST_NODE* forNode);
void checkAssignmentStmt(AST_NODE* assignmentNode);
void checkIfStmt(AST_NODE* ifNode);
void checkWriteFunction(AST_NODE* functionCallNode);
void checkFunctionCall(AST_NODE* functionCallNode);
void processExprRelatedNode(AST_NODE* exprRelatedNode);
void checkParameterPassing(Parameter* formalParameter, AST_NODE* actualParameter);
void checkReturnStmt(AST_NODE* returnNode);
void processExprNode(AST_NODE* exprNode);
void processVariableLValue(AST_NODE* idNode);
void processVariableRValue(AST_NODE* idNode);
void processConstValueNode(AST_NODE* constValueNode);
void getExprOrConstValue(AST_NODE* exprOrConstNode, int* iValue, float* fValue);
void evaluateExprValue(AST_NODE* exprNode);

void AST_traverse(AST_NODE* AST_node);
void processIdentifierNode(AST_NODE *IdNode);

typedef enum ErrorMsgKind
{
    SYMBOL_IS_NOT_TYPE,
    SYMBOL_REDECLARE,
    SYMBOL_UNDECLARED,
    NOT_FUNCTION_NAME,
    TRY_TO_INIT_ARRAY,
    EXCESSIVE_ARRAY_DIM_DECLARATION,
    RETURN_ARRAY,
    VOID_VARIABLE,
    TYPEDEF_VOID_ARRAY,
    PARAMETER_TYPE_UNMATCH,
    TOO_FEW_ARGUMENTS,
    TOO_MANY_ARGUMENTS,
    RETURN_TYPE_UNMATCH,
    INCOMPATIBLE_ARRAY_DIMENSION,
    NOT_ASSIGNABLE,
    NOT_ARRAY,
    IS_TYPE_NOT_VARIABLE,
    IS_FUNCTION_NOT_VARIABLE,
    STRING_OPERATION,
    ARRAY_SIZE_NOT_INT,
    ARRAY_SIZE_NEGATIVE,
    ARRAY_SUBSCRIPT_NOT_INT,
    PASS_ARRAY_TO_SCALAR,
    PASS_SCALAR_TO_ARRAY
} ErrorMsgKind;
int ignoreDim = 0;

void AST_traverse(AST_NODE* AST_node){
    for(AST_NODE* child = AST_node->child; child != NULL; child = child->rightSibling){
        switch(child->nodeType){
            case PROGRAM_NODE:
                processProgramNode(child);
                break;
            case DECLARATION_NODE:
                processDeclarationNode(child);
                break;                
            case IDENTIFIER_NODE:
                processIdentifierNode(child);
                break;
            case PARAM_LIST_NODE:
                AST_traverse(child);
                break;
            case NUL_NODE:

                break;
            case BLOCK_NODE:
                processBlockNode(child);
                break;
            case VARIABLE_DECL_LIST_NODE:
                AST_traverse(child);
                break;
            case STMT_LIST_NODE:
                AST_traverse(child);
                break;
            case STMT_NODE:
                processStmtNode(child);
                break;
            case EXPR_NODE:
                processExprNode(child);
                break;
            case CONST_VALUE_NODE:
                processConstValueNode(child);
                break;
            case NONEMPTY_ASSIGN_EXPR_LIST_NODE:
                AST_traverse(child);
                break;
            case NONEMPTY_RELOP_EXPR_LIST_NODE:
                AST_traverse(child);
                break;
            default:
                break;
        }

    }
}



void printErrorMsgSpecial(AST_NODE* node1, char* name2, ErrorMsgKind errorMsgKind)
{
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node1->linenumber);
    /*
    switch(errorMsgKind)
    {
    default:
        printf("Unhandled case in void printErrorMsg(AST_NODE* node, ERROR_MSG_KIND* errorMsgKind)\n");
        break;
    }
    */
}


void printErrorMsg(AST_NODE* node, ErrorMsgKind errorMsgKind)
{
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node->linenumber);
    /*
    switch(errorMsgKind)
    {
        printf("Unhandled case in void printErrorMsg(AST_NODE* node, ERROR_MSG_KIND* errorMsgKind)\n");
        break;
    }
    */
}


void semanticAnalysis(AST_NODE *root)
{
    processProgramNode(root);
}


DATA_TYPE getBiggerType(DATA_TYPE dataType1, DATA_TYPE dataType2)
{
    if(dataType1 == FLOAT_TYPE || dataType2 == FLOAT_TYPE) {
        return FLOAT_TYPE;
    } else {
        return INT_TYPE;
    }
}

void processIdentifierNode(AST_NODE *IdNode)
{
    AST_traverse(IdNode); // For traversing array's indices nodes or the init value when declaring variables 
    SymbolTableEntry* ptr = retrieveSymbol(IdNode->semantic_value.identifierSemanticValue.identifierName);
    idNode->semantic_value.identifierSemanticValue.symbolTableEntry = ptr; 

    if(ptr == NULL){//Not yet declared, may be first declaring or undeclared using
        if(IdNode->parent->nodeType == DECLARATION_NODE){//First declaring  
            printf("Won't execute to here! Already handled this case in function:processDeclarationNode\n");
        }else{
            printErrorMsg(idNode, SYMBOL_UNDECLARED);
        }
    }else if(ptr->attribute->attributeKind == TYPE_ATTRIBUTE){
        printErrorMsg(idNode, IS_TYPE_NOT_VARIABLE);
    }else if(ptr->attribute->attributeKind == FUNCTION_SIGNATURE){ //For function ID
        idNode->dataType = ptr->attribute->attr.functionSignature->returnType;
    }else if(ptr->attribute->attr.typeDescriptor->kind == SCALAR_TYPE_DESCRIPTOR){//For common variable ID
        idNode->dataType = ptr->attribute->attr.typeDescriptor->properties.dataType;
        idNode->semantic_value.identifierSemanticValue.kind = NORMAL_ID;
    }else if(ptr->attribute->attr.typeDescriptor->kind == ARRAY_TYPE_DESCRIPTOR){//For array ID
        idNode->dataType = ptr->attribute->attr.typeDescriptor->properties.arrayProperties.elementType;
        idNode->semantic_value.identifierSemanticValue.kind = ARRAY_ID;

        //Need to check the indices' datatype
        int arrayDim = 0;
        for(AST_NODE* i = idNode->child ; i != NULL ; i = i->rightSibling){
            if(i->dataType != INT_TYPE){
                printErrorMsg(idNode, ARRAY_SUBSCRIPT_NOT_INT);
                return; //Question: Just return? 
            }
            ++arrayDim;
        }

        if(ignoreDim == 0){//Normally match
            if(arrayDim != ptr->attribute->attr.typeDescriptor->properties.arrayProperties.dimension){// Check the dimension     
                printErrorMsg(idNode, INCOMPATIBLE_ARRAY_DIMENSION);
            }
            else{
                idNode->semantic_value.identifierSemanticValue.kind = NORMAL_ID;
            }
        }else{//Function array parameter passing
            if(arrayDim == ptr->attribute->attr.typeDescriptor->properties.arrayProperties.dimension){
                //Passing scalar
                idNode->semantic_value.identifierSemanticValue.kind = NORMAL_ID;
            }
            else if(arrayDim < ptr->attribute->attr.typeDescriptor->properties.arrayProperties.dimension){
                //Probable in function parameters, and the parameter is also an array with smaller dimension than idNode
                idNode->semantic_value.identifierSemanticValue.kind = ARRAY_ID;
            }
            else{
                printErrorMsg(idNode, INCOMPATIBLE_ARRAY_DIMENSION);
            }
        }   

    }   

}

void processProgramNode(AST_NODE *programNode)
{
    AST_traverse(programNode);
}

void processDeclarationNode(AST_NODE* declarationNode)
{
    switch(declNode->semantic_value.declSemanticValue.kind){
        case VARIABLE_DECL:
            declareIdList(declNode->child, VARIABLE_ATTRIBUTE, 0);//declNode->child is the type node
            break;
        case TYPE_DECL:
            declareIdList(declNode->child, TYPE_ATTRIBUTE, 0);
            break;
        case FUNCTION_DECL:
            declareFunction(declNode->child);
            break;
        case FUNCTION_PARAMETER_DECL:
            declareIdList(declNode->child, VARIABLE_ATTRIBUTE, 1);
            break;
        default:
            break;
    }

}


void processTypeNode(AST_NODE* idNodeAsType)
{
    char *typeName = idNodeAsType->semantic_value.identifierSemanticValue.identifierName;
    SymbolTableEntry *sym = retrieveSymbol(typeName);
    idNodeAsType->semantic_value.identifierSemanticValue.symbolTableEntry = sym;
    if (sym == NULL) {
        printErrorMsg(idNodeAsType, SYMBOL_UNDECLARED);
        // Set to void to prevent segfault, for user defined types 
        idNodeAsType->semantic_value.identifierSemanticValue.symbolTableEntry =  retrieveSymbol("void");
    } else {// Common C-- types
        if (sym->attribute->attributeKind != TYPE_ATTRIBUTE) { // Question: What kind of case??
            printErrorMsg(idNodeAsType, SYMBOL_IS_NOT_TYPE);
        } else {
            idNodeAsType->semantic_value.identifierSemanticValue.symbolTableEntry = sym;
        }
    }
}


void declareIdList(AST_NODE* typeNode, SymbolAttributeKind isVariableOrTypeAttribute, int ignoreArrayFirstDimSize)
{
    
    processTypeNode(typeNode);
    SymbolTableEntry* sym = typeNode->semantic_value.identifierSemanticValue.symbolTableEntry;
    DATA_TYPE type = sym->attribute->attr.typeDescriptor->properties.dataType;
    AST_NODE* Identifier = typeNode->rightSibling;

    for(;Identifier!=NULL; Identifier = Identifier->rightSibling){
        
        TypeDescriptor* typeDesc = malloc(sizeof(TypeDescriptor));
        if (Identifier->semantic_value.identifierSemanticValue.kind == ARRAY_ID) {
            processDeclDimList(Identifier->child, typeDesc, ignoreArrayFirstDimSize);//Check if the array's indices is legal (int const)
            typeDesc->properties.arrayProperties.elementType = type;
            // TODO: check TRY_TO_INIT_ARRAY
        } else {
            typeDesc->kind = SCALAR_TYPE_DESCRIPTOR;
            typeDesc->properties.dataType = type;
            if (Identifier->semantic_value.identifierSemanticValue.kind == WITH_INIT_ID){// Declaring variable with initial value
                AST_traverse(Identifier->child);
            }
        }        

        SymbolAttribute *symAttr = malloc(sizeof(SymbolAttribute));
        symAttr->attributeKind = isVariableOrTypeAttribute;
        symAttr->attr.typeDescriptor = typeDesc;
        char* idName = Identifier->semantic_value.identifierSemanticValue.identifierName;

        if (!declaredLocally(idName)) {//Check if locally defined
            Identifier->semantic_value.identifierSemanticValue.symbolTableEntry = enterSymbol(idName, symAttr);//Question: What is the real usage of enterSymbol? 
        } else {
            printErrorMsg(Identifier, SYMBOL_REDECLARE); //Question: How about the program behavior when redefining a variable?
            // ignore, use predefined symbol ?
        }
    }
}

void checkAssignOrExpr(AST_NODE* assignOrExprRelatedNode)
{
}

void checkWhileStmt(AST_NODE* whileNode)
{
    AST_traverse(whileNode);
}


void checkForStmt(AST_NODE* forNode)
{
    AST_traverse(forNode);
}


void checkAssignmentStmt(AST_NODE* assignmentNode)
{
}


void checkIfStmt(AST_NODE* ifNode)
{
    AST_traverse(ifNode);
}

void checkWriteFunction(AST_NODE* functionCallNode)
{
}

void checkFunctionCall(AST_NODE* functionCallNode)
{
    ignoreDim = 1;
    traverse(functionCallNode);
    ignoreDim = 0;
}

void checkParameterPassing(Parameter* formalParameter, AST_NODE* actualParameter)
{
}


void processExprRelatedNode(AST_NODE* exprRelatedNode)
{
}

void getExprOrConstValue(AST_NODE* exprOrConstNode, int* iValue, float* fValue)
{
}

void evaluateExprValue(AST_NODE* exprNode)
{
}


void processExprNode(AST_NODE* exprNode)
{
}


void processVariableLValue(AST_NODE* idNode)
{
}

void processVariableRValue(AST_NODE* idNode)
{
}


void processConstValueNode(AST_NODE* constValueNode)
{
}


void checkReturnStmt(AST_NODE* returnNode)
{
}


void processBlockNode(AST_NODE* blockNode)
{
    AST_traverse(blockNode);
}


void processStmtNode(AST_NODE* stmtNode)
{
}


void processGeneralNode(AST_NODE *node)
{
}

void processDeclDimList(AST_NODE* idNode, TypeDescriptor* typeDescriptor, int ignoreFirstDimSize)
{


}


void declareFunction(AST_NODE* returnTypeNode)
{   

    processTypeNode(returnTypeNode);
    SymbolTableEntry* sym = returnTypeNode->semantic_value.identifierSemanticValue.symbolTableEntry;
    DATA_TYPE type = sym->attribute->attr.typeDescriptor->properties.dataType;

    AST_NODE* Identifier = typeNode->rightSibling;
    AST_NODE* paramListNode = Identifier->rightSibling;
    AST_NODE* blockNode = paramListNode->rightSibling;

    //Declare function ID
    SymbolAttribute *Attr = (SymbolAttribute*)malloc(sizeof(SymbolAttribute*));
    *Attr = (SymbolAttribute){(SymbolAttributeKind)FUNCTION_SIGNATURE, (void*)malloc(sizeof(FunctionSignature*))};
    *(Attr->attr.functionSignature) = (FunctionSignature){0, NULL, returnType};
    
    SymbolTableEntry* ptr = retrieveSymbol(Identifier->semantic_value.identifierSemanticValue.identifierName);
    if(ptr != NULL){// This ID name already declared 
        Identifier->linenumber = returnTypeNode->linenumber;
        printErrorMsg(Identifier, SYMBOL_REDECLARE);
        Identifier->semantic_value.identifierSemanticValue.symbolTableEntry = ptr;
    }else{// Declared new ID
        Identifier->semantic_value.identifierSemanticValue.symbolTableEntry = enterSymbol(Identifier->semantic_value.identifierSemanticValue.identifierName, Attr);
    }

    //Function parameters' scope
    openScope();
    AST_traverse(paramListNode);
    Parameter* last = NULL;
    for(AST_NODE* paramDeclNode = paramListNode->child ; paramDeclNode != NULL ; paramDeclNode = paramDeclNode->rightSibling)
    {
        Parameter* tmp = (Parameter*)malloc(sizeof(Parameter*));
        tmp->type = retrieveSymbol((paramDeclNode->child->rightSibling)->semantic_value.identifierSemanticValue.identifierName)->attribute->attr.typeDescriptor;
        tmp->parameterName = (paramDeclNode->child->rightSibling)->semantic_value.identifierSemanticValue.identifierName;
        ++Attr->attr.functionSignature->parametersCount;
        tmp->next = NULL;
        if(last == NULL)
            Attr->attr.functionSignature->parameterList = tmp;
        else
            last->next = tmp;
        last = tmp;
    }
    AST_traverse(blockNode);
    closeScope();

}
