#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<ctype.h>
#include<math.h>
#include"header.h"

#define TABLE_SIZE	512

symtab * hash_table[TABLE_SIZE];
extern int linenumber;


int HASH(char * str){
	int idx=0;
	while(*str){
		idx = idx << 1;
		idx+=*str;
		str++;
	}	
	return (idx & (TABLE_SIZE-1));
}

/*returns the symbol table entry if found else NULL*/

symtab * lookup(char *name){
	int hash_key;
	symtab* symptr;
	if(!name)
		return NULL;
	hash_key=HASH(name);
	symptr=hash_table[hash_key];

	while(symptr){
		if(!(strcmp(name,symptr->lexeme)))
			return symptr;
		symptr=symptr->front;
	}
	return NULL;
}


void insertID(char *name){
	int hash_key;
	symtab* ptr;
	symtab* symptr=(symtab*)malloc(sizeof(symtab));	
	
	hash_key=HASH(name);
	ptr=hash_table[hash_key];
	
	if(ptr==NULL){
		/*first entry for this hash_key*/
		hash_table[hash_key]=symptr;
		symptr->front=NULL;
		symptr->back=symptr;
	}
	else{
		symptr->front=ptr;
		ptr->back=symptr;
		symptr->back=symptr;
		hash_table[hash_key]=symptr;	
	}


	strcpy(symptr->lexeme,name);
	symptr->line=linenumber;
	symptr->counter=1;
	
}

void printSym(symtab* ptr) 
{
	    printf(" Name = %s \n", ptr->lexeme);
	    printf(" References = %d \n", ptr->counter);
}

void printSymTab()
{
    int i;
    printf("----- Symbol Table ---------\n");
    for (i=0; i<TABLE_SIZE; i++)
    {
        symtab* symptr;
		symptr = hash_table[i];
		while (symptr != NULL)
		{
            printf("====>  index = %d \n", i);
	    	printSym(symptr);
	    	symptr=symptr->front;
		}
    }

 }

int compare(const void* a,const void* b)
{
	return strcmp((*(symtab**)a)->lexeme,(*(symtab**)b)->lexeme);
}
void SymbolFreq()
{
	symtab* symptr;
	symtab** symtable;
	int symcount=0;
	printf("Frequency of identifiers:\n");
	/*for(int i=0;i<TABLE_SIZE;i++)
	{
		symptr = hash_table[i];
		while(symptr!=NULL)
		{
			symcount++;
			symptr=symptr->front;
		}
	}*/
	//printf("%d\n", symcount);
	symtable = (symtab**)malloc(symcount*sizeof(symtab*));

	for(int i=0,j=0;i<TABLE_SIZE;i++)
	{
		symptr = hash_table[i];
		//printf("a");
    	while (symptr != NULL)
    	{
    		symcount++;
        	symtable[j] = symptr;
        	j++;  
        	symptr=symptr->front;
        	//printf("b\n");
  		}
  		//printf("%d %d\n",i,j);
	}
	//printf("%d\n", symcount);
	qsort(symtable , symcount , sizeof(symtab*), compare);
 	for (int i=0; i<symcount; i++)
 	{
    	printf("%-10s %d\n", symtable[i]->lexeme, symtable[i]->counter);
 	}
  	free(symtable);
}

