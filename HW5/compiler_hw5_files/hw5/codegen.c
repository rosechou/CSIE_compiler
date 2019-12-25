#include "codegen.h"
#include "header.h"
#include "symbolTable.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//TODO: modify all register names, binary/unary ops
//question: framesize needed? .skip needed? meaning of .align? fcvt.w.s&fcvt.s.w? different of float&int instruction


FILE *output;
int tmp_regs[7],  float_tmp_regs[8], g_cnt, g_offset, sizeLocalVar;

void gen_head(char *name){
	fprintf(output, ".text\n");
	fprintf(output, "_start_%s:\n", name);
}

void gen_prologue(char *name){
	//open stack frame space:
	fprintf(output, "sd ra,0(sp)\n");
	fprintf(output, "sd fp,-8(sp)\n");
	fprintf(output, "add fp, sp, -8\n");
	fprintf(output, "add sp, sp, -16\n");
    fprintf(output, "la ra, _frameSize_%s\n", name);
    fprintf(output, "lw ra,0(ra)\n");
    fprintf(output, "sub sp,sp,ra\n");

    // callee save:
	int offset = 8;
	for(int i = 0; i <= 6; ++i){	
		fprintf(output, "sd t%d,%d(sp)\n", i, offset);
		offset += 8;
	}
	for(int i = 2; i <= 11; ++i){
		fprintf(output, "sd s%d,%d(sp)\n", i, offset);
		offset += 8;
	}
	fprintf(output, "sd fp,%d(sp)\n", offset);
	offset += 8;
	for(int i = 0; i <= 7; ++i){
		fprintf(output, "fsw ft%d,%d(sp)\n", i, offset);
		offset += 4;
	}

	//what this?
	// fprintf(output, ".data\n");
	// fprintf(output, "_AR_%d: .word %d\n", g_cnt, offset);
	// gen_Alignment();
	// fprintf(output, ".text\n");
	// fprintf(output, "lw t1, _AR_%d\n", g_cnt++);
	// fprintf(output, "sub sp, sp, w19\n");
	g_offset = 0;
}

void gen_epilogue(char *name){
	fprintf(output, "_end_%s:\n", name);
    // callee load:
	int offset = 8;
	for(int i = 0; i <= 6; ++i){	
		fprintf(output, "ld t%d,%d(sp)\n", i, offset);
		offset += 8;
	}
	for(int i = 2; i <= 11; ++i){
		fprintf(output, "ld s%d,%d(sp)\n", i, offset);
		offset += 8;
	}
	fprintf(output, "ld fp,%d(sp)\n", offset);
	offset += 8;
	for(int i = 0; i <= 7; ++i){
		fprintf(output, "flw ft%d,%d(sp)\n", i, offset);
		offset += 4;
	}

	//close stack frame space:
	fprintf(output, "ld ra,8(fp)\n");
	fprintf(output, "mv sp,fp\n");
	fprintf(output, "add sp,sp,8\n");
	fprintf(output, "ld fp,0(fp)\n");
	fprintf(output, "jr ra\n");
	fprintf(output, ".data\n");
	fprintf(output, "_frameSize_%s: .word %d\n", name, offset+sizeLocalVar);

}



void genGlobalVarDecl(AST_NODE *DeclListNode){
	fprintf(output, ".data\n");
	for (AST_NODE *declNode = DeclListNode->child ; declNode != NULL; declNode = declNode->rightSibling)
	{
		if( node_decl_kind(declNode) == VARIABLE_DECL)
		{
			for (AST_NODE *idNode = declNode->child->rightSibling ; idNode != NULL; idNode = idNode->rightSibling)
			{
				char name[512];
				sprintf(name, "_%s", node_id_name(idNode));
				TypeDescriptor *TD = sym_typedesc(node_id_entry(idNode));
				DATA_TYPE type = TD->properties.dataType;

				if(TD->kind != SCALAR_TYPE_DESCRIPTOR){
					int size = 4;
					for(int i = 0 ; i < TD->properties.arrayProperties.dimension ; i++)
						size *= TD->properties.arrayProperties.sizeInEachDimension[i];
					fprintf(output, "%s: .skip %d\n", name, size);
				}
				else{// NORMAL_ID || WITH_INIT_ID
					if(type == INT_TYPE){
						int value = idNode->child != NULL ? const_ival(idNode->child) : 0;
						fprintf(output, "%s: .word %d\n", name, value);
					}
					else if (type == FLOAT_TYPE){
						double value = idNode->child != NULL ? const_fval(idNode->child) : 0.0;
						fprintf(output, "%s: .word %d\n", name, 0);
					}
				}
			}
		}
	}
	gen_Alignment();
}

void genFunctionDecl(AST_NODE *funcDeclNode){
	AST_NODE *typeNode, *idNode, *paramListNode, *blockNode;
	typeNode = funcDeclNode->child;
	idNode = typeNode->rightSibling;
	paramListNode = idNode->rightSibling;
	blockNode = paramListNode->rightSibling;
	
	gen_head(node_id_name(idNode));
	gen_prologue(node_id_name(idNode));
	genBlock(blockNode);
	gen_epilogue(node_id_name(idNode));
}

void genLocalVarDecl(AST_NODE *DeclListNode){//any array in HW5?
	int _offset = g_offset;
	for(AST_NODE *declNode = DeclListNode->child ; declNode != NULL ; declNode = declNode->rightSibling){
		if( node_decl_kind(declNode) == VARIABLE_DECL){
			int offset = 0;
			for (AST_NODE *idNode = declNode->child->rightSibling ; idNode != NULL; idNode = idNode->rightSibling){
				SymbolTableEntry *entry = node_id_entry(idNode);
				if(sym_typedesc(entry)->kind == SCALAR_TYPE_DESCRIPTOR)
					offset += 4;
				else{
					int size = 4;
					for(int i = 0 ; i < sym_typedesc(entry)->properties.arrayProperties.dimension ; i++)
						size *= sym_typedesc(entry)->properties.arrayProperties.sizeInEachDimension[i];
					offset += size;
				}
				entry->offset = g_offset + 184 + offset;
			}
			g_offset += offset;
			sizeLocalVar = offset;
		}
	}

	//print framesize after the epilogue
	// fprintf(output, ".data\n");
	// fprintf(output, "_int_const_%d: .word %d\n", g_cnt, g_offset - _offset);
	// gen_Alignment();
	// fprintf(output, ".text\n");
	// int reg = get_reg();
	// fprintf(output, "lw t%d, _int_const_%d\n", reg, g_cnt++);
	// free_reg(reg);
	// fprintf(output, "sub sp, sp, t%d\n", reg);
}

void genBlock(AST_NODE *blockNode){
	// int id = g_cnt++;
	// int reg1 = get_reg(), reg2 = get_reg();
	// free_reg(reg1), free_reg(reg2);

	// fprintf(output, ".data\n");
	// fprintf(output, "_SP_%d: .skip 8\n", id);
	// fprintf(output, ".text\n");
	// fprintf(output, "la t%d,_SP_%d\n", reg1, id);
	// fprintf(output, "mv t%d,sp\n", reg2);
	// fprintf(output, "str t%d,0(t%d)\n", reg2, reg1);
	for(AST_NODE *node = blockNode->child ; node != NULL ; node = node->rightSibling){
		if(node->nodeType == VARIABLE_DECL_LIST_NODE)
			genLocalVarDecl(node);
		else if(node->nodeType == STMT_LIST_NODE)
			genStmtList(node);
	}
	// reg1 = get_reg(), reg2 = get_reg();
	// free_reg(reg1), free_reg(reg2);
	// fprintf(output, "la t%d,_SP_%d\n", reg1, id);
	// fprintf(output, "lw t%d,0(t%d)\n", reg2, reg1);
	// fprintf(output, "mv sp,t%d\n", reg2);
}

void genStmtList(AST_NODE *stmtListNode){
	for(AST_NODE *stmtNode = stmtListNode->child ; stmtNode != NULL ; stmtNode = stmtNode->rightSibling)
		genStmt(stmtNode);
}

void genWhileStmt(AST_NODE *whileNode){
	int cnt = g_cnt++;
	AST_NODE *condition = whileNode->child, *stmtNode = condition->rightSibling;
	fprintf(output, "_whileLabel_%d:\n", cnt);
	if(condition->nodeType == STMT_NODE &&  node_stmt_kind(condition) == ASSIGN_STMT){
		genAssignStmt(condition);
		condition = condition->child;
	}
	int reg = genExprRelated(condition);
	free_reg(reg);
	if(condition->dataType == FLOAT_TYPE){
		int reg2 = get_reg();
		free_reg(reg2);
		fprintf(output, "fcvt.w.s t%d, ft%d\n", reg2, reg-7);
		fprintf(output, "beqz t%d, _whileExitLabel_%d\n", reg2, cnt);
	}else{
		fprintf(output, "beqz t%d, _whileExitLabel_%d\n", reg, cnt);
	}
	genStmt(stmtNode);
	fprintf(output, "j _whileLabel_%d\n", cnt);
	fprintf(output, "_whileExitLabel_%d:\n", cnt);
}

void genIfStmt(AST_NODE *ifNode){
	AST_NODE *condition = ifNode->child, *stmtNode = condition->rightSibling, *elseNode = stmtNode->rightSibling;
	int cnt = g_cnt++;
	// fprintf(output, "_IF_%d:\n", cnt);
	if(condition->nodeType == STMT_NODE &&  node_stmt_kind(condition) == ASSIGN_STMT){
		genAssignStmt(condition);
		condition = condition->child;
	}
	int reg = genExprRelated(condition);
	free_reg(reg);

	if(condition->dataType == FLOAT_TYPE){
		int reg2 = get_reg();
		free_reg(reg2);
		fprintf(output, "fcvt.w.s t%d, ft%d\n", reg2, reg-7);
		fprintf(output, "beqz t%d, _elseLabel_%d\n", reg2, cnt);
	}else{
		fprintf(output, "beqz t%d, _elseLabel_%d\n", reg, cnt);
	}
	genStmt(stmtNode);
	fprintf(output, "j _ifExitLabel_%d\n", cnt);
	fprintf(output, "_elseLabel_%d:\n", cnt);
	genStmt(elseNode);
	fprintf(output, "_ifExitLabel_%d:\n", cnt);
}

void genForStmt(AST_NODE *forNode){
	//TODO
	return;
}


void genAssignStmt(AST_NODE *assignNode){
	AST_NODE *idNode = assignNode->child, *rightNode = idNode->rightSibling;
	int resultReg = genExprRelated(rightNode);
	SymbolTableEntry *entry = node_id_entry(idNode);
	TypeDescriptor *TD = sym_typedesc(entry);

	if(node_id_kind(idNode) == NORMAL_ID){
		idNode->dataType = TD->properties.dataType;
		int reg = get_reg();
		free_reg(reg);
		if(entry->nestingLevel == 0)//global
			fprintf(output, "la t%d,_%s\n", reg, node_id_name(idNode));
		else{
			gen_offset_data(reg, entry->offset);
			fprintf(output, "sub t%d, fp, t%d\n", reg, reg);
		}
		if(idNode->dataType == INT_TYPE){
			if(rightNode->dataType == FLOAT_TYPE){
				int resultReg2 = get_reg();
				free_reg(resultReg2);
				fprintf(output, "fcvt.w.s t%d, ft%d\n", resultReg2, resultReg-7);
				fprintf(output, "sw t%d,0(t%d)\n", resultReg2, reg);
			}else{
				fprintf(output, "sw t%d,0(t%d)\n", resultReg, reg);
			}
			
		}else{
			if(rightNode->dataType == INT_TYPE){
				int resultReg2 = get_reg();
				free_reg(resultReg2);
				fprintf(output, "fcvt.s.w t%d, t%d\n", resultReg2, resultReg);
				fprintf(output, "sw t%d,0(t%d)\n", resultReg2, reg);
			}else{
				fprintf(output, "sw t%d,0(t%d)\n", resultReg, reg);	
			}
			
		}
	}
	else{//ARRAY
		idNode->dataType = sym_typedesc(entry)->properties.arrayProperties.elementType;
		int dim = 0;
		int reg = get_reg();
		fprintf(output, "addi t%d,t%d,0\n", reg, reg);
		for(AST_NODE *dimListNode = idNode->child ; dimListNode != NULL ; dimListNode = dimListNode->rightSibling){
			int idxreg = genExprRelated(dimListNode);
			int reg2 = get_reg();
			gen_offset_data(reg2, sym_typedesc(entry)->properties.arrayProperties.sizeInEachDimension[dim]);
			fprintf(output, "mulw t%d, t%d, t%d\n", reg, reg, reg2);
			fprintf(output, "slli t%d, t%d, 2\n", idxreg, idxreg);
			fprintf(output, "add t%d, t%d, t%d\n", reg, reg, idxreg);
			free_reg(idxreg);
			free_reg(reg2);
			++dim;//multi-dimension part
		}
		int offset = entry->offset;
		int reg2 = get_reg();

		if(entry->nestingLevel == 0)//global
			fprintf(output, "la t%d,_%s\n", reg2, node_id_name(idNode));
		else{
			gen_offset_data(reg2, entry->offset);
			fprintf(output, "sub t%d, fp, t%d\n", reg2, reg2);
		}
		fprintf(output, "add t%d, t%d, t%d\n", reg, reg, reg2);
		if(idNode->dataType == INT_TYPE){
			if(rightNode->dataType == FLOAT_TYPE){
				int resultReg2 = get_reg();
				free_reg(resultReg2);
				fprintf(output, "fcvt.w.s t%d, ft%d\n", resultReg2, resultReg-7);
				fprintf(output, "sw t%d,0(t%d)\n", resultReg2, reg);
			}else{
				fprintf(output, "sw t%d,0(t%d)\n", resultReg, reg);
			}
			
		}
		else{
			if(rightNode->dataType == INT_TYPE){
				int resultReg2 = get_reg();
				free_reg(resultReg2);
				fprintf(output, "fcvt.s.w t%d, t%d\n", resultReg2, resultReg);
				fprintf(output, "sw t%d,0(t%d)\n", resultReg2, reg);
			}else{
				fprintf(output, "sw t%d,0(t%d)\n", resultReg, reg);	
			}
			
		}


		free_reg(reg);
		free_reg(reg2);
	}
	free_reg(resultReg);
	assignNode->dataType = (idNode->dataType == FLOAT_TYPE || rightNode->dataType == FLOAT_TYPE) ? FLOAT_TYPE : INT_TYPE;
}

void genFuncCall(AST_NODE *funcNode){
	AST_NODE *idNode = funcNode->child;
	if(strcmp(node_id_name(idNode), "write") == 0){
		genWrite(idNode);
		return;
	}
	//Parameterless procedure calls
	if(strcmp(node_id_name(idNode), "read") == 0)
		fprintf(output, "jal _read_int\n");
	else if(strcmp(node_id_name(idNode), "fread") == 0)
		fprintf(output, "jal _read_float\n");
	else
		fprintf(output, "jal _start_%s\n", node_id_name(idNode));
	funcNode->dataType = node_id_entry(idNode)->attribute->attr.functionSignature->returnType;
}

void genReturnStmt(AST_NODE *returnNode){
	AST_NODE *node;
	for(node = returnNode->parent ; node != NULL ; node = node->parent){
		if(node->nodeType == DECLARATION_NODE){
			if(node_decl_kind(node) == FUNCTION_DECL)
				returnNode->dataType = node->child->dataType;
			break;
		}
	}
	int reg = genExprRelated(returnNode->child);
	if(returnNode->dataType == INT_TYPE){
		if(returnNode->child->dataType == INT_TYPE){
			fprintf(output, "mv a0,t%d\n", reg);
		}else{
			fprintf(output, "fcvt.w.s a0,ft%d\n", reg-7);
		}
	}
	else if(returnNode->dataType == FLOAT_TYPE){
		if(returnNode->child->dataType == INT_TYPE){
			fprintf(output, "mv a0,t%d\n", reg);
			fprintf(output, "fcvt.s.w a0, a0\n");
		}else{
			fprintf(output, "fmv.x.w a0,ft%d\n", reg-7);
		}
	}
	fprintf(output, "j _end_%s\n", node->child->rightSibling->semantic_value.identifierSemanticValue.identifierName);
	free_reg(reg);
}

void genStmt(AST_NODE *stmtNode){
	if(stmtNode->nodeType == NUL_NODE)
		return;
	else if(stmtNode->nodeType == BLOCK_NODE)
		genBlock(stmtNode);
	else{
		switch( node_stmt_kind(stmtNode)){
			case WHILE_STMT:
				genWhileStmt(stmtNode);
				break;
			case FOR_STMT:
				genForStmt(stmtNode);
				break;
			case ASSIGN_STMT:
				genAssignStmt(stmtNode);
				break;
			case IF_STMT:
				genIfStmt(stmtNode);
				break;
			case FUNCTION_CALL_STMT:
				genFuncCall(stmtNode);
				break;
			case RETURN_STMT:
				genReturnStmt(stmtNode);
				break;
		}
	}
}

int genExprRelated(AST_NODE *exprRelatedNode){//TODO: Support return float tmp register index
	int reg = get_reg();
	SymbolTableEntry *entry = node_id_entry(exprRelatedNode);
	switch(exprRelatedNode->nodeType){
		case EXPR_NODE:
			free_reg(reg);
			reg = genExpr(exprRelatedNode);
			return reg;
		case STMT_NODE:
			free_reg(reg);
			genFuncCall(exprRelatedNode);
			
			if(exprRelatedNode->dataType == INT_TYPE){
				reg = get_reg();
				fprintf(output, "mv t%d,a0\n", reg);				
			}else if(exprRelatedNode->dataType == FLOAT_TYPE){
				reg = get_float_reg();
				fprintf(output, "fmv.w.x ft%d,a0\n", reg-7);
			}
			return reg;
		case IDENTIFIER_NODE:
			
			if(node_id_kind(exprRelatedNode) == NORMAL_ID){
				exprRelatedNode->dataType = sym_typedesc(entry)->properties.dataType;
				if(entry->nestingLevel == 0){
					if(exprRelatedNode->dataType == INT_TYPE){
						fprintf(output, "lw t%d,_%s\n", reg, exprRelatedNode->semantic_value.identifierSemanticValue.identifierName);
					}else{
						free_reg(reg);
						reg = get_float_reg();
						fprintf(output, "flw ft%d,_%s\n", reg, exprRelatedNode->semantic_value.identifierSemanticValue.identifierName);
					}
				}else{
					gen_offset_data(reg, entry->offset);
					fprintf(output, "sub t%d, fp, t%d\n", reg, reg);
					if(exprRelatedNode->dataType == INT_TYPE){
						fprintf(output, "lw t%d,0(t%d)\n", reg, reg);
					}else{
						free_reg(reg);
						int reg2 = get_float_reg();						
						fprintf(output, "flw ft%d,0(t%d)\n", reg2-7, reg);
						return reg2;
					}
				}
			}else{//ARRAY_ID
				exprRelatedNode->dataType = sym_typedesc(entry)->properties.arrayProperties.elementType;
				int dim = 0; 
				fprintf(output, "addi t%d,x0,0\n", reg); 
				for(AST_NODE *dimListNode = exprRelatedNode->child ; dimListNode != NULL ; dimListNode = dimListNode->rightSibling){
					int reg2 = get_reg();
					gen_offset_data(reg2, sym_typedesc(entry)->properties.arrayProperties.sizeInEachDimension[dim]);
					fprintf(output, "mulw t%d, t%d, t%d\n", reg, reg, reg2);
					free_reg(reg2);
					reg2 = genExprRelated(dimListNode);
					fprintf(output, "slli t%d, t%d, 2\n", reg2, reg2);
					fprintf(output, "add t%d, t%d, t%d\n", reg, reg, reg2);
					free_reg(reg2);
					++dim;
				}
				int reg2 = get_reg();
				if(entry->nestingLevel == 0){
					fprintf(output, "la t%d, _%s\n", reg2, exprRelatedNode->semantic_value.identifierSemanticValue.identifierName);
				}else{
					gen_offset_data(reg2, entry->offset);
					fprintf(output, "sub t%d, fp, t%d\n", reg2, reg2);
				}
				free_reg(reg2);
				fprintf(output, "add t%d, t%d, t%d\n", reg, reg, reg2);
				if(exprRelatedNode->dataType == INT_TYPE){
					fprintf(output, "lw t%d,0(t%d)\n", reg, reg);
				}else{
					free_reg(reg);
					int reg2 = get_float_reg();						
					fprintf(output, "flw ft%d,0(t%d)\n", reg2-7, reg);
					return reg2;
				}
			}
			break;
		case CONST_VALUE_NODE:
			if(const_type(exprRelatedNode) == INTEGERC){
				exprRelatedNode->dataType = INT_TYPE;
				expr_const_eval(exprRelatedNode).iValue = const_ival(exprRelatedNode);
				fprintf(output, ".data\n");
				fprintf(output, "_int_const_%d: .word %d\n", g_cnt, const_ival(exprRelatedNode));
				gen_Alignment();
				fprintf(output, ".text\n");
				fprintf(output, "lw t%d, _int_const_%d\n", reg, g_cnt++);
			}
			else if(const_type(exprRelatedNode) == FLOATC){
				exprRelatedNode->dataType = FLOAT_TYPE;
				free_reg(reg);
				reg = get_float_reg();
				expr_const_eval(exprRelatedNode).fValue = const_fval(exprRelatedNode);
				fprintf(output, ".data\n");
				fprintf(output, "_float_const_%d: .float %lf\n", g_cnt, const_fval(exprRelatedNode));
				gen_Alignment();
				fprintf(output, ".text\n");
				fprintf(output, "flw ft%d, _float_const_%d\n", reg-7, g_cnt++);
			}
			else if(const_type(exprRelatedNode) == STRINGC){
				exprRelatedNode->dataType = CONST_STRING_TYPE;
				fprintf(output, ".data\n");
				fprintf(output, "_string_const_%d: .ascii %s\n", g_cnt, const_sval(exprRelatedNode));
				gen_Alignment();
				fprintf(output, ".text\n");
				fprintf(output, "la t%d, _string_const_%d\n", reg, g_cnt++);
			}
			break;
	}
	return reg;
}

void genIntBinaryOp(AST_NODE *exprNode, int reg1, int reg2, char *op){//BEQ/BNE/BLT/BGE
	exprNode->dataType = INT_TYPE;
	fprintf(output, "%s t%d, t%d, 12\n", op, reg1, reg2);//_binaryOpLabel_%d 
	fprintf(output, "addi t%d,x0,0\n", reg1);
	fprintf(output, "j _END_binaryOp_%d\n", g_cnt);
	fprintf(output, "_binaryOpLabel_%d:\n", g_cnt);
	fprintf(output, "addi t%d,x0,1\n", reg1);
	fprintf(output, "_END_binaryOp_%d:\n", g_cnt++);
}

int genFloatBinaryOp(AST_NODE *exprNode, int reg1, int reg2, char *op){//FEQ.S/FLT.S/FLE.S
	exprNode->dataType = INT_TYPE;
	int rd_reg = get_reg();
	fprintf(output, "%s t%d, ft%d, ft%d\n", op,rd_reg, reg1-7, reg2-7);
	free_reg(reg1);
	return rd_reg;
}

int genExpr(AST_NODE *exprNode){
	if(node_expr_kind(exprNode) == BINARY_OPERATION){
		AST_NODE *leftNode = exprNode->child, *rightNode = leftNode->rightSibling;
		exprNode->dataType = (leftNode->dataType == FLOAT_TYPE || rightNode->dataType == FLOAT_TYPE) ? FLOAT_TYPE : INT_TYPE;
		if(is_const_eval(exprNode)){
			int reg = get_reg();
			fprintf(output, ".data\n");
			if(exprNode->dataType == INT_TYPE){
				fprintf(output, "_int_const_%d: .word %d\n", g_cnt, expr_const_eval(exprNode).iValue);
				gen_Alignment();
				fprintf(output, ".text\n");
				fprintf(output, "lw t%d, _int_const_%d\n", reg, g_cnt++);
			}else{
				free_reg(reg);
				reg = get_float_reg();
				fprintf(output, "_float_const_%d: .float %lf\n", g_cnt, expr_const_eval(exprNode).fValue);
				gen_Alignment();
				fprintf(output, ".text\n");
				fprintf(output, "flw ft%d, _float_const_%d\n", reg-7, g_cnt++);
			}
			return reg;
		}else{
			int reg1 = genExprRelated(leftNode);
			free_reg(reg1);
			if(leftNode->dataType == INT_TYPE){				
				fprintf(output, "sw t%d,0(sp)\n", reg1);
			}else{
				fprintf(output, "fsw ft%d,0(sp)\n", reg1-7);
			}
			fprintf(output, "addi sp, sp, -8\n");

			int reg2 = genExprRelated(rightNode);
			fprintf(output, "addi sp, sp, 8\n");
			if(leftNode->dataType == INT_TYPE){
				reg1 = get_reg();
				fprintf(output, "lw t%d,0(sp)\n", reg1);
			}else{
				reg1 = get_float_reg();
				fprintf(output, "flw ft%d,0(sp)\n", reg1-7);
			}
			if(leftNode->dataType == INT_TYPE && rightNode->dataType == INT_TYPE){
				exprNode->dataType = INT_TYPE;
				int swap_tmp;
				switch(expr_bin_op(exprNode)){
					case BINARY_OP_ADD:
						fprintf(output, "addw t%d, t%d, t%d\n", reg1, reg1, reg2);
						break;
					case BINARY_OP_SUB:
						fprintf(output, "subw t%d, t%d, t%d\n", reg1, reg1, reg2);
						break;
					case BINARY_OP_MUL:
						fprintf(output, "mulw t%d, t%d, t%d\n", reg1, reg1, reg2);
						break;
					case BINARY_OP_DIV:
						fprintf(output, "div t%d, t%d, t%d\n", reg1, reg1, reg2);
						break;
					case BINARY_OP_EQ:
						genIntBinaryOp(exprNode, reg1, reg2, "beq");
						break;
					case BINARY_OP_GE:
						genIntBinaryOp(exprNode, reg1, reg2, "bge");
						break;
					case BINARY_OP_LE://risc-v not support ble
						swap_tmp = reg1;
						reg1 = reg2;
						reg2 = swap_tmp;
						genIntBinaryOp(exprNode, reg1, reg2, "bge");					
						break;
					case BINARY_OP_NE:
						genIntBinaryOp(exprNode, reg1, reg2, "bne");
						break;
					case BINARY_OP_GT://risc-v not support bgt
						swap_tmp = reg1;
						reg1 = reg2;
						reg2 = swap_tmp;
						genIntBinaryOp(exprNode, reg1, reg2, "blt");
						break;
					case BINARY_OP_LT:
						genIntBinaryOp(exprNode, reg1, reg2, "blt");
						break;
					case BINARY_OP_AND:
						fprintf(output, "and %d, %d, %d", reg1, reg1, reg2);
						break;
					case BINARY_OP_OR:
						fprintf(output, "or %d, %d, %d", reg1, reg1, reg2);
						break;	
				}
				free_reg(reg2);
				return reg1;
			}else{//float
				exprNode->dataType = FLOAT_TYPE;
				if(leftNode->dataType == INT_TYPE){
					fprintf(output, "fcvt.s.w t%d, t%d\n", reg1, reg1);
					free_reg(reg1);
					int tmp = reg1;
					reg1 = get_float_reg();
					fprintf(output, "fmv.w.x ft%d t%d\n", reg1-7, tmp);
				}
				if(rightNode->dataType == INT_TYPE){
					fprintf(output, "fcvt.s.w t%d, t%d\n", reg2, reg2);
					free_reg(reg2);
					int tmp = reg2;
					reg2 = get_float_reg();
					fprintf(output, "fmv.w.x ft%d t%d\n", reg2-7, tmp);					
				}
				

				switch(expr_bin_op(exprNode)){
					case BINARY_OP_ADD:
						fprintf(output, "fadd.s ft%d, ft%d, ft%d\n", reg1-7, reg1-7, reg2-7);
						break;
					case BINARY_OP_SUB:
						fprintf(output, "fsub.s ft%d, ft%d, ft%d\n", reg1-7, reg1-7, reg2-7);
						break;
					case BINARY_OP_MUL:
						fprintf(output, "fmul.s ft%d, ft%d, ft%d\n", reg1-7, reg1-7, reg2-7);
						break;
					case BINARY_OP_DIV:
						fprintf(output, "fdiv.s ft%d, ft%d, ft%d\n", reg1-7, reg1-7, reg2-7);
						break;
					case BINARY_OP_EQ://FEQ.S/FLT.S/FLE.S
						reg1 = genFloatBinaryOp(exprNode, reg1, reg2, "feq.s");
						break;
					case BINARY_OP_GE:
						reg1 = genFloatBinaryOp(exprNode, reg1, reg2, "bge");
						break;
					case BINARY_OP_LE:
						reg1 = genFloatBinaryOp(exprNode, reg1, reg2, "ble");
						break;
					case BINARY_OP_NE:
						reg1 = genFloatBinaryOp(exprNode, reg1, reg2, "bne");
						break;
					case BINARY_OP_GT:
						reg1 = genFloatBinaryOp(exprNode, reg1, reg2, "bgt");
						break;
					case BINARY_OP_LT:
						reg1 = genFloatBinaryOp(exprNode, reg1, reg2, "blt");
						break;
					case BINARY_OP_AND:
						fprintf(output, "and %d, %d, %d", reg1, reg1, reg2);//what does it mean?
						break;
					case BINARY_OP_OR:
						fprintf(output, "or %d, %d, %d", reg1, reg1, reg2);
						break;

				}
				free_reg(reg2);
				return reg1;
			}
		}
	}
	else{//Unary
		AST_NODE* operand = exprNode->child;
		exprNode->dataType = operand->dataType;
		if(is_const_eval(exprNode)){
			fprintf(output, ".data\n");
			if(exprNode->dataType == INT_TYPE){
				fprintf(output, "_int_const_%d: .word %d\n", g_cnt, expr_const_eval(exprNode).iValue);
			}else{
				fprintf(output, "_float_const_%d: .float %lf\n", g_cnt, expr_const_eval(exprNode).fValue);
			}
			gen_Alignment();
			fprintf(output, ".text\n");
			int reg;
			if(exprNode->dataType == INT_TYPE){
				reg = get_reg();
				fprintf(output, "lw t%d, _int_const_%d\n", reg, g_cnt++);
			}else{
				reg = get_float_reg();
				fprintf(output, "flw ft%d, _float_const_%d\n", reg-7, g_cnt++);
			}
			return reg;
		}else{
			int reg = genExprRelated(operand);
			if(operand->dataType == INT_TYPE){
				if(expr_uni_op(exprNode) == UNARY_OP_NEGATIVE){
					fprintf(output, "subw t%d,x0,t%d", reg, reg); //neg = sub rd, x0, rs
				}else if(expr_uni_op(exprNode) == UNARY_OP_LOGICAL_NEGATION){
					fprintf(output, "beqz t%d,_unaryOpLabel_%d\n", reg, g_cnt);
					fprintf(output, "addi t%d,x0,1\n", reg);
					fprintf(output, "j _END_unaryOp_%d\n", g_cnt);
					fprintf(output, "_unaryOpLabel_%d:\n", g_cnt);
					fprintf(output, "addi t%d,x0,1\n", reg);
					fprintf(output, "_END_unaryOp_%d:\n", g_cnt++);
				}
			}else{
				exprNode->dataType = FLOAT_TYPE;
				if(expr_uni_op(exprNode) == UNARY_OP_NEGATIVE){
					fprintf(output, "fsub.s ft%d,x0,ft%d", reg-7, reg-7);
				}else if(expr_uni_op(exprNode) == UNARY_OP_LOGICAL_NEGATION){
					int tmp = get_reg();
					free_reg(reg);
					fprintf(output, "fcvt.w.s t%d, t%d\n", tmp, reg-7);
					reg = tmp;
					fprintf(output, "beqz t%d,_unaryOpLabel_%d\n", reg, g_cnt);
					fprintf(output, "addi t%d,x0,1\n", reg);
					fprintf(output, "j _END_unaryOp_%d\n", g_cnt);
					fprintf(output, "_unaryOpLabel_%d:\n", g_cnt);
					fprintf(output, "addi t%d,x0,1\n", reg);
					fprintf(output, "_END_unaryOp_%d:\n", g_cnt++);
				}
			}
			return reg;
		}
	}
}


void genWrite(AST_NODE *node){
	AST_NODE *paramListNode = node->rightSibling, *paramNode = paramListNode->child;
	int reg = genExprRelated(paramNode);
	if(paramNode->dataType == INT_TYPE){
		fprintf(output, "mv a0, t%d\n", reg);
		fprintf(output, "jal _write_int\n");
	}
	else if(paramNode->dataType == FLOAT_TYPE){
		fprintf(output, "fmv.x.w a0, ft%d\n", reg-7); 
		fprintf(output, "jal _write_float\n");
	}
	else if(paramNode->dataType == CONST_STRING_TYPE){
		fprintf(output, "mv a0,t%d\n", reg);
		fprintf(output, "jal _write_str\n");
		
	}
	free_reg(reg);
}

void genProgram(AST_NODE *root){
	for (AST_NODE *decl = root->child ; decl != NULL; decl = decl->rightSibling){
		if (decl->nodeType == VARIABLE_DECL_LIST_NODE)
			genGlobalVarDecl(decl);
		else
			genFunctionDecl(decl);
	}
}

void codegen(AST_NODE *root){
	output = fopen("output.s", "w");
	genProgram(root);
	fclose(output);
}

int get_reg(){
	for(int i = 0; i <= 6 ; i++){
		if(!tmp_regs[i]){
			tmp_regs[i] = 1;
			return i;
		}
	}
	printf("out of register!!!!\n");
	exit(0);
}

void free_reg(int reg){
	if(reg<7){
		tmp_regs[reg] = 0;
	}else{
		float_tmp_regs[reg-7] = 0;
	}
	
}

int get_float_reg(){
	for(int i = 0; i <= 7 ; i++){
		if(!float_tmp_regs[i]){
			float_tmp_regs[i] = 1;
			return i + 7;
		}
	}
	printf("out of float register!!!!\n");
	exit(0);
}


void gen_Alignment(){
	fprintf(output, ".align 3\n");
}


void gen_offset_data(int reg, int offset){//TODO: what this?
	fprintf(output, ".data\n");
	fprintf(output, "_const_offset_%d: .word %d\n", g_cnt, offset);
	gen_Alignment();
	fprintf(output, ".text\n");
	fprintf(output, "lw t%d, _const_offset_%d\n", reg, g_cnt++);
	
}