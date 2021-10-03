%option noyywrap 
%{
#include <stdio.h>
#include "header.h" 
int linenumber;
symtab * lookup();
symtab * ptr;
void     insertID();
void 	 printSymTab();
void 	 SymbolFreq();
/* You need to define for all tokens in C--, here are some examples */
/*rose: define c-- Symbols and operators and Separators*/

#define RETURN  1
#define TYPEOF  2
#define IF      3
#define ELSE    4
#define INT		10
#define FLOAT 		11
#define OP_ASSIGN 	12 
#define OP_OR 		13 
#define MK_LPAREN 	14 
#define MK_RPAREN 	15 
#define FOR         16
#define VOID        17
#define WHILE       18
#define OP_PLUS     19
#define OP_MINUS    20
#define OP_DIV      21
#define OP_MUL      22
#define OP_LT       23  /*less than*/       
#define OP_MT       24  /*more than*/
#define OP_LE       25  /*less equal*/
#define OP_ME       26 
#define OP_NE       27  
#define OP_EQ       28 
#define OP_AND      29
#define OP_NOT      30
#define MK_LBRACE   31
#define MK_RBRACE   32
#define MK_LBRAKT   33
#define MK_RBRAKT   34
#define SEMICO      35
#define COMMA       36
#define DOT         37
#define HIGHLIGHT   38
#define ERROR 		100 

%}

letter   [A-Za-z]
digit    [0-9]
ID	 {letter}({letter}|{digit}|"_")*
WS	 [ \t]+
Int_constant {digit}+

/* You need to define the following RE's */
Float_constant {digit}+e(\+|-)?{digit}+|{digit}+(\.{digit}+)(e(\+|-)?{digit}+)?|({digit}+)?(\.{digit}+)(e(\+|-)?{digit}+)?
String_constant \"[^\"\n]*\"
comment \/\*(\*+[^\/]|[^\*])*\*\/
HIGHLIGHT \"[^\"\n]*\"

OP_assign        "="
OP_or   "||"

/* Other operators appear here */ 
/*rose: define c-- Symbols and operators and Separators, same above*/
OP_PLUS     "+"
OP_MINUS    "-"
OP_DIV      "/"
OP_MUL      "*"
OP_LT       "<"       
OP_MT       ">"
OP_LE       "<="
OP_ME       ">="
OP_NE       "!=" 
OP_EQ       "=="
OP_AND      "&&"
OP_NOT      "!"

newline  "\n"

DL_lparen "("
DL_rparen ")"
DL_lbrace "{"
DL_rbrace "}"
DL_lbrakt "["
DL_rbrakt "]"
DL_comma  ","
DL_semicol ";"
DL_dot  "."
INT "int"
FLOAT "float"
RETURN "return"
TYPEOF "typeof"
IF "if"
ELSE "else"
FOR "for"
VOID "void"
WHILE "while"
/* Other separators appear here */

error    .

%%
{INT}           {/* return INT; */ }
{FLOAT}         { /* return FLOAT; */}
{RETURN}		{/* return RETURN;*/}
{TYPEOF}		{/* return TYPEOF;*/}
{IF}			{/* return IF;*/}
{ELSE}			{/* return ELSE;*/}
{VOID}			{/* return VOID;*/}
{WHILE}			{/* return WHILE;*/}
{HIGHLIGHT}		{/* return HIGHLIGHT;*/}

{WS}            {}
{Int_constant}         {/*return INT; */}
{Float_constant}       {/*return FLOAT;*/}
{ID}            { 
			ptr = lookup(yytext);
			if (ptr == NULL)
			     insertID(yytext);	
			else 
			     ptr->counter++;
		}

{OP_assign}     {/* return OP_ASSIGN; */} 
{OP_or}         {/* return OP_OR; */}
{OP_PLUS}       {/* return OP_PLUS; */}
{OP_MINUS}      {/* return OP_MINUS; */}
{OP_DIV}        {/* return OP_DIV; */}
{OP_MUL}        {/* return OP_MUL; */}
{OP_LT}         {/* return OP_LT; */}       
{OP_MT}         {/* return OP_MT; */}
{OP_LE}         {/* return OP_LE; */}
{OP_ME}         {/* return OP_ME; */}
{OP_NE}         {/* return OP_NE; */}
{OP_EQ}         {/* return OP_EQ; */}
{OP_AND}        {/* return OP_AND; */}
{OP_NOT}        {/* return OP_NOT; */}


{DL_lparen}     {/* return MK_LPAREN; */}
{DL_rparen}     {/* return MK_RPAREN; */}
{DL_lbrace}     {/* return MK_LBRACE; */}
{DL_rbrace}     {/* return MK_RBRACE; */}
{DL_lbrakt}     {/* return MK_LBRAKT; */}
{DL_rbrakt}     {/* return MK_RBRAKT; */}
{DL_comma}      {}
{DL_semicol}    {}
{DL_dot}        {}
{newline}       {linenumber += 1;}
{comment}       {puts(yytext); } /*print text token*/
{error}         {
			printf("ERR line %d %s\n",linenumber, yytext); 
			return ERROR; 
		}

%%

int main(int argc, char **argv)
{
 argc--; ++argv;
   if (argc > 0)
      yyin = fopen(argv[0], "r");
   else
      yyin = stdin;
   yylex();
   printf("\n");
   SymbolFreq();
}

