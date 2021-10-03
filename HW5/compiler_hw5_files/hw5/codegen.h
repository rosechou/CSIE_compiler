#ifndef __CODEGEN_H__
#define __CODEGEN_H__

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include "header.h"
#include "symbolTable.h"

#define node_id_entry(node) node->semantic_value.identifierSemanticValue.symbolTableEntry
#define node_id_name(node) node->semantic_value.identifierSemanticValue.identifierName
#define node_id_kind(node) node->semantic_value.identifierSemanticValue.kind
#define node_decl_kind(node) node->semantic_value.declSemanticValue.kind
#define node_stmt_kind(node) node->semantic_value.stmtSemanticValue.kind
#define node_expr_kind(node) node->semantic_value.exprSemanticValue.kind
#define sym_typedesc(sym) sym->attribute->attr.typeDescriptor
#define const_ival(node) node->semantic_value.const1->const_u.intval
#define const_fval(node) node->semantic_value.const1->const_u.fval
#define const_sval(node) node->semantic_value.const1->const_u.sc
#define is_const_eval(node) node->semantic_value.exprSemanticValue.isConstEval
#define const_type(node) node->semantic_value.const1->const_type
#define expr_bin_op(node) node->semantic_value.exprSemanticValue.op.binaryOp
#define expr_uni_op(node) node->semantic_value.exprSemanticValue.op.unaryOp
#define expr_const_eval(node) node->semantic_value.exprSemanticValue.constEvalValue

void gen_head(char *name);
void gen_prologue(char *name);
void gen_epilogue(char *name);
int get_reg();
void free_reg(int reg);
int get_float_reg();
void gen_Alignment();
void gen_offset_data(int reg, int offset);
void codegen(AST_NODE *root);
void genProgram(AST_NODE *root);
void genGlobalVarDecl(AST_NODE *varDeclListNode);
void genFunctionDecl(AST_NODE *funcDeclNode);
void genLocalVarDecl(AST_NODE *DeclListNode);
void genStmtList(AST_NODE *stmtListNode);
void genStmt(AST_NODE *stmtNode);
void genBlock(AST_NODE *blockNode);
void genWhileStmt(AST_NODE *whileNode);
void genIfStmt(AST_NODE *ifNode);
void genForStmt(AST_NODE *forNode);
void genAssignStmt(AST_NODE *assignNode);
void genFuncCall(AST_NODE *funcNode);
void genReturnStmt(AST_NODE *returnNode);
int genExprRelated(AST_NODE *exprRNode);
int genExpr(AST_NODE *exprNode);
void genWrite(AST_NODE *node);
void genIntBinaryOp(AST_NODE *exprNode, int reg1, int reg2, char *op);
int genFloatBinaryOp(AST_NODE *exprNode, int reg1, int reg2, char *op);

#endif