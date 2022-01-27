%{

#include<stdio.h>
#include<stdlib.h>
#include<string.h>

typedef enum {false,true} bool;

typedef struct T3AC
{
	char* code;
	char* var;
	char* falselab;
	char* truelab;
	int count;
	char* begin;
	char* after;
} T3AC;

typedef struct node
{
	struct node *left;
	struct node *right;
	char *token;
	char* type;
	char* value;

	T3AC* TAC;
}node;

typedef struct Args
{
	char* type;
	struct Args* next;
}Args;

typedef struct VarInfo
{
	char* name;
	struct VarInfo* next;
}VarInfo;



typedef struct stack
{
	int CurrentScope;
	VarInfo* List;
	struct stack* PreviousScope;
} stack;



typedef struct Declaration
{
	int scopeid;
	char* type;
	Args* args;
	int* Args_Count;
	struct Declaration* next;
}Declaration;

typedef struct LinkedList
{
	char* name;
	Declaration* Decl;
	struct LinkedList* next;
} LinkedList;

typedef struct HashTableElement
{
		int key;
		LinkedList* chain;
}HashTableElement;

typedef struct HashTable
{
	int cellsTaken;
	HashTableElement* hashtable;
	int tableSize;
	int hashFunction;
	int numOfElements;
} HashTable;

typedef struct Project
{
	int scope_Count;
	HashTable* ht;
	stack* Scope;
	node* tree;
} Project;



int Redclare = 0;
int IfFlag = 0;
int ElseFlag = 0;
int MainFlag = 0;
int TacCount = 0;
int i = 0;
int LabelCount = 0;
node *mknode(node *left, node *right, char *token);
char* ReturnType(HashTable *ht ,char* id);
void StartProject(node* root);
int T3ac(node* root);
T3AC* mk3ac();
int CheckExp(node* root, Args* args ,HashTable* ht);
int ReturnNumBytes(char* type);
char* freshVar();
char* freshLabel();
char* genValues(node* root);
void gen(node* root);
void printTypeTree(node *tree, int tab);
int ReturnNumOfArgs(node* root);
int CheckFuncTypes(node* root, char* name, Args* args, HashTable* ht);
int CheckFuncArgs(HashTable* ht, node* root, char* name);
char* TypeGiving(HashTable *ht, node* root);
Project* Make_Project(node* root);
HashTable* initTable(int tableSize);
stack* PushScopeToHead(Project *proj, stack* scope);
stack* FreeStacks(HashTable *ht, stack* head);
void DelVarFromSymbolTable(HashTable* ht, char* name);
LinkedList** place(HashTable* ht, char* str) ;
stack* DeleteStackElement(HashTable *ht, stack* head);
int pow(int x, int y);
bool checkIfIDInHT(HashTable* ht, char* name, int scopeId);
bool CheckCondition(node* root);
int CheckReturnType(node* root, char* type);
Args* DeleteArgsElement(Args* head);
Args* BuildArgsNode(char* type);
Args* addArg(Args* head, Args* new_chain);
int search(HashTable* ht, char* str);
int insert(HashTable* ht, char* name, char* type, int scopeId, int* Args_Count, Args* args);
Declaration* addDecToStart(Declaration* head, char* type, int scopeId, int* Args_Count, Args* args);
void printHashTabe(HashTable* ht);
VarInfo* mkVarInfo(char* name);
int isInList(LinkedList* head, char* value);
LinkedList* addToStart(LinkedList* head, char* new_node, char* type, int scopeId, int* Args_Count, Args *args);
void printDecList(Declaration* head);
Declaration* BuildDecNode(char* type, int scopeId, int* Args_Count, Args* args);
LinkedList* BuildNode(char* name, char* type, int scopeId, int* Args_Count, Args* args);
int search(HashTable* ht, char* str);
LinkedList* getNode(LinkedList* head, char* name);
int improvedHashFunction(char* str);
int hash(char* str, HashTable* ht);
VarInfo* addVarInfoToStart(VarInfo* head, char* id);
void printIdList(VarInfo* idinfo);
void printArgsList(Args* head);
Declaration* DeleteDecElement(Declaration* head);
Declaration* FreeDecList(Declaration* head);
int accumulateStringHashFunction(char* str);
LinkedList* FreeList(LinkedList* head);
Args* FreeArgsList(Args* head);
HashTable* freeHashTable(HashTable* ht);
int constantStringHashFunction(char* str);
LinkedList* DelElement(LinkedList* head, char* value);
void PrintList(LinkedList* head);
VarInfo* mkIdInfo(char* name);
void printTree(node *tree,int tab);
int yylex(void);
void yyerror(char*);
extern char* yytext;
#define YYSTYPE struct node*

%} 


//------------Yacc Declarations--------------------
%start START
%token BOOL_TYPE CHAR_TYPE INT_TYPE REAL_TYPE STR_TYPE
%token INT_POINT CHAR_POINT REAL_POINT
%token BOOL CHAR INT_DEC INT_HEX
%token IF ELSE FOR DO WHILE VAR FUNC VOID RETURN
%token NONE AND DIV ASSIGN EQ G GE L LE POTEH SOGER 
%token MINUS NOT NOTEQ OR PLUS MUL ADDR POINT POTEH_BLOCK SOGER_BLOCK ADD SUB
%token ID COLON SEMICOLON REAL STRING
%left PLUS MINUS
%left MUL DIV
//------------Yacc Declarations-----End------------


//------------Grammer Rules------------------------
%% 

START: code { StartProject($1); }
;

code: functions { $$=mknode($1,NULL,"CODE"); }
;

functions: function functions {$$=mknode($1,$2,"FUNCTIONS"); } 
	  | function { $$=mknode($1,NULL,"FUNCTION"); }
;

function: VOID prin { $$=mknode($2,NULL,"VOID"); } 
	  | type func { $$=mknode($1,$2,"FUNC"); }
;

func: id func_args { $$=mknode($1,$2,""); }
;

prin: id prin_args { $$=mknode($1,$2,""); }
;

prin_args: POTEH first_args scanner { $$=mknode($2,$3,"ARGS"); }
;

func_args: POTEH first_args returns { $$=mknode($2,$3,"ARGS"); }
;

scanner: block { $$=mknode($1,NULL,""); }
;


//---------body--------------------
body: body_exp body { $$=mknode($1,$2,""); }
    | body_exp {$$=mknode($1,NULL,""); }
    | {$$=mknode(NULL,NULL,""); }
;

body_exp: expression SEMICOLON {$$=mknode($1,NULL,""); }
	| if {$$=mknode($1,NULL,""); }
	| if_else {$$=mknode($1,NULL,""); }
        | while {$$=mknode($1,NULL,""); }
	| functions {$$=mknode($1,NULL,""); }
	| var_exp {$$=mknode($1,NULL,""); }
	| for {$$=mknode($1,NULL,""); }
	| do_while {$$=mknode($1,NULL,""); }
	| string_exp {$$=mknode($1,NULL,""); }
	| POTEH_BLOCK SOGER_BLOCK {$$=mknode(NULL,NULL,""); }
	| block {$$=mknode($1,NULL,""); }
;


//--------body_end-----------------

//--------------args----------------

first_args: second_args SEMICOLON first_args { $$=mknode($1,$3,"MANY_TYPES"); }
            | second_args SOGER { $$=mknode($1,NULL,"ONE_TYPE"); }
;

second_args: type ids { $$=mknode($1,$2,""); }
                    |  { $$=mknode(NULL,NULL,"NONE_ARGS"); }
;

ids: id ',' ids { $$=mknode($1,$3,"MULTI_ARGS"); }
               | id {$$=mknode($1,NULL,"ONE_ARG"); }
;

id: identifier { $$ =  mknode($1, NULL, "ID"); }
;

identifier: ID { $$ =  mknode(NULL, NULL, yytext); }


//-----------type----------
type: BOOL_TYPE { $$ = mknode(NULL, NULL, "BOOL"); }
      | CHAR_TYPE { $$ = mknode(NULL, NULL, "CHAR"); }
      | INT_TYPE { $$ = mknode(NULL, NULL, "INT"); }
      | REAL_TYPE { $$ = mknode(NULL, NULL, "REAL"); }
      | STR_TYPE { $$ = mknode(NULL, NULL, "STRING"); }
      | INT_POINT { $$ = mknode(NULL, NULL, "INT_POINT"); }
      | REAL_POINT { $$ = mknode(NULL, NULL, "REAL_POINT"); }
      | CHAR_POINT { $$ = mknode(NULL, NULL, "CHAR_POINT"); }
;

//---------type_end---------




//----------base_types------

real: real_val { $$ = mknode($1, NULL, "REAL_VAL"); }
      | id { $$ = mknode($1, NULL, "REAL_ID"); }
;

real_val: REAL { $$ = mknode(NULL, NULL, yytext); }

string_exp: string_exp string_var {$$=mknode($1,$2,""); }
	|  string_var {$$=mknode($1,NULL,""); }
;

string_val: STRING { $$ = mknode(NULL, NULL,yytext); }
;

string: string_val { $$ = mknode($1, NULL,"STR_VAL"); }

string_var: STR_TYPE string_loop SEMICOLON {$$=mknode($2,NULL,""); }
;

string_loop: string_format ',' string_loop {$$=mknode($1,$3,""); }
	|    equal ',' string_loop {$$=mknode($1,$3,""); }
	|    string_format {$$=mknode($1,NULL,""); }
	|    equal  {$$=mknode($1,NULL,""); }
	|    {$$=mknode(NULL,NULL,""); }
;	

int: int_dec { $$ = mknode($1, NULL, "INT_DEC"); }
     |int_hex { $$ = mknode($1, NULL, "INT_HEX"); }
     | id { $$ = mknode($1, NULL, "INT_ID"); }
;

int_dec: INT_DEC { $$ = mknode(NULL, NULL, yytext); }
;

int_hex: INT_HEX { $$ = mknode(NULL, NULL, yytext); }
;

char: char_val { $$ = mknode($1, NULL,"CHAR_VAL"); }
      | id { $$ = mknode($1, NULL, "CHAR_ID"); }
;

char_val: CHAR { $$ = mknode(NULL, NULL,yytext); }

var: VAR type ids SEMICOLON { $$ = mknode($2,$3,"VAR");}
     | VAR type var_stat SEMICOLON { $$ = mknode($2,$3,"VAR:");}
;     

var_stat: id ',' var_stat {$$=mknode($1,$3,"VAR_STAT"); }
	| id_r ',' var_stat {$$=mknode($1,$3,"ID=R"); }
	| id_r {$$=mknode($1,NULL,"ID=L"); }
;

id_r: id ASSIGN eq_right {$$=mknode($1,$3,"="); }

var_exp: var_exp var { $$ = mknode($1,$2,""); }
      | var { $$ = mknode($1,NULL,""); }
;


bool: BOOL { $$ = mknode(NULL, NULL,yytext); }
      | id { $$ = mknode($1, NULL, ""); }
;



null: NONE { $$ = mknode(NULL,NULL,"NULL"); }
;


//---------base_types_end----



//----------conditions and loops------------



if: IF POTEH statment SOGER block {$$=mknode($3,$5,"IF"); }
;

if_else: if else { $$=mknode($1,$2,"IF_ELSE"); }
;

else: ELSE block {$$=mknode($2,NULL,"ELSE"); }
;

while: WHILE POTEH statment SOGER block {$$=mknode($3,$5,"WHILE"); }
;

block: POTEH_BLOCK body SOGER_BLOCK { $$=mknode($2,NULL,"BLOCK"); }
;

for: FOR POTEH for_int_condtion_update SOGER block {$$=mknode($3,$5,"FOR");}
;
for_int_condtion_update: for_int_condition SEMICOLON for_update {$$=mknode($1,$3,""); }

for_int_condition : for_init SEMICOLON for_condition  {$$=mknode($1,$3,""); }

for_init: id ASSIGN int{$$=mknode($1,$3,"="); }

for_condition: id for_loop_and_value {$$=mknode($1,$2,"CONDITION"); }
;

for_loop_and_value: for_loop int {$$=mknode($1,$2,"");}
	            | for_loop real {$$=mknode($1,$2,"");}
		    | for_loop id {$$=mknode($1,$2,"");}
;

for_loop: G { $$ = mknode(NULL,NULL,">"); }
	 | GE{ $$ = mknode(NULL,NULL,">="); }
	 | L { $$ = mknode(NULL,NULL,"<"); }
	 | LE  { $$ = mknode(NULL,NULL,"<="); }
	 | NOTEQ  { $$ = mknode(NULL,NULL,"!="); }
	 | EQ { $$ = mknode(NULL,NULL,"=="); }
;

for_update: id ADD{ $$ = mknode($1,NULL,"++"); }
	    |id SUB{ $$ = mknode($1,NULL,"--"); }
	    |id ASSIGN for_add_sub{ $$ = mknode($1,$3,"="); }
;
for_add_sub: id PLUS int{ $$ = mknode($1,$3,"+"); }
	     | id MINUS int{ $$ = mknode($1,$3,"-"); }
	     | id PLUS id{ $$ = mknode($1,$3,"+"); }
	     | id MINUS id{ $$ = mknode($1,$3,"-"); }
;

do_while: DO block while_for_do {$$ = mknode($2,$3,"DO"); }
;

while_for_do: WHILE POTEH statment SOGER SEMICOLON { $$ = mknode($3,NULL,"WHILE_DO"); }
;



//------------expressions-------------

expression: statment {$$=mknode($1,NULL,""); }  
	  | equal {$$=mknode($1,NULL,"EQUAL"); }
	  | id ASSIGN statment {$$=mknode($1,$3,"="); }
	  | type func {$$=mknode($1,NULL,"FUNC"); }
	  | call_func {$$=mknode($1,NULL,""); }
;

call_func: id POTEH call_func_args {$$=mknode($1,$3,"CALL_FUNC"); }
	| id POTEH SOGER {$$=mknode($1,NULL,"CALL_FUNC"); }
;

call_func_args: call_func_values SOGER {$$=mknode($1,NULL,"VALS:"); }
;

call_func_values: func_exp_values {$$=mknode($1,NULL,"FUNC_VALUES"); }
	|  exp {$$=mknode($1,NULL,"FUNC_EXP"); }
;

func_exp_values: value ',' func_exp_values {$$=mknode($1,$3,"VALS_FUNC"); }
	    |    value {$$=mknode($1,NULL,"VAL_FUNC"); }
;	 

equal: id ASSIGN eq_right { $$ = mknode($1,$3,"="); }
      | id ASSIGN pointer { $$ = mknode($1,$3,"="); }
      | pointer ASSIGN eq_right { $$ = mknode($1,$3,"="); }
      | string_format ASSIGN char { $$ = mknode($1,$3,"CHAR_ASS"); }
      | string_format ASSIGN string { $$ = mknode($1,$3,"STR_ASS"); }
;

eq_right: exp { $$ = mknode($1,NULL,"EQ_RIGHT"); } 
	| call_func { $$ = mknode($1,NULL,"ID_=_CF"); }
	| null { $$ = mknode($1,NULL,""); }
	| string { $$ = mknode($1,NULL,"STRING_VALUE"); }
;

statment: bool_compare OR statment {$$=mknode($1,$3,"||"); } 
	 | bool_compare AND statment { $$=mknode($1,$3,"&&"); }
         | bool_compare { $$=mknode($1,NULL,"STAT"); }
;

bool_compare: bool {$$=mknode($1,NULL,""); }
	     | compare {$$=mknode($1,NULL,"COMP"); }
	     | exp {$$=mknode($1,NULL,"EXP"); }
	     | POTEH compare SOGER {$$=mknode($1,NULL,"()"); }
;

compare: exp EQ exp {$$=mknode($1,$3,"=="); }
   	| exp NOTEQ exp {$$=mknode($1,$3,"!="); }
	| NOT bool {$$=mknode($2,NULL,"!"); }
	| NOT exp {$$=mknode($2,NULL,"!"); }
	| exp LE exp {$$=mknode($1,$3,"<=") ;}
	| exp L exp {$$=mknode($1,$3,"<") ; } 
	| exp G exp {$$=mknode($1,$3,">"); }
	| exp GE exp {$$=mknode($1,$3,">="); } 
;

exp: exp PLUS exp {$$=mknode($1,$3,"+"); }
   | exp MINUS exp {$$=mknode($1,$3,"-"); }
   | exp DIV exp {$$=mknode($1,$3,"/"); }
   | exp MUL exp {$$=mknode($1,$3," * "); }
   | POTEH exp SOGER {$$=mknode($2,NULL,"()"); }
   | value {$$=mknode($1,NULL,"VALUE"); }
   | string {$$=mknode($1,NULL,""); }
;

value: int { $$ = mknode($1, NULL, "INT_VALUE"); }
     | real { $$ = mknode($1, NULL, "REAL_VALUE"); }
     | char { $$ = mknode($1, NULL, "CHAR_VALUE"); }
     | bool { $$ = mknode($1, NULL, "BOOL_VALUE"); }
     | string_size { $$ = mknode($1, NULL, ""); }
     | string_format { $$ = mknode($1, NULL, ""); }
     | pointer { $$ = mknode($1, NULL, ""); }
     | addr { $$=mknode($1,NULL,"ADDRESS"); }
     | call_func {$$=mknode($1,NULL,""); }
;

string_size: '|' id '|' {$$=mknode($2,NULL,"| |"); }
;

string_format: id '[' int ']' {$$=mknode($1,$3,"[ ]"); }
 		| id '[' string_index_exp ']' {$$=mknode($1,$3,"[ ]"); }
;
string_index_exp: int_dec str_index_loop { $$ = mknode($1,$2,""); }
;

str_index_loop: PLUS int_dec { $$ = mknode($2,NULL,"+"); }
		| MINUS int_dec{ $$ = mknode($2,NULL,"-"); }
	 	| DIV int_dec{ $$ = mknode($2,NULL,"/"); }
	 	| MUL int_dec{ $$ = mknode($2,NULL," * "); }
;

pointer: POINT POTEH exp SOGER { $$=mknode($3,NULL,"*"); }
	| POINT id {$$=mknode($2,NULL,"*"); }
;

addr: ADDR id {$$=mknode($2,NULL,"&"); }
	| ADDR string_format {$$=mknode($2,NULL,"&"); }
;



//------------returns---------------

returns: POTEH_BLOCK body char_block {$$=mknode($2,$3,"RETURN_CHAR1"); }
       //| POTEH_BLOCK char_block {$$=mknode($2,NULL,"RETURN_CHAR2"); }
       | POTEH_BLOCK body int_block {$$=mknode($2,$3,"RETURN_INT1"); }
       //| POTEH_BLOCK int_block {$$=mknode($2,NULL,"RETURN_INT2"); }
       | POTEH_BLOCK body bool_block {$$=mknode($2,$3,"RETURN_BOOL1"); }
      // | POTEH_BLOCK bool_block {$$=mknode($2,NULL,"RETURN_BOOL2"); }
       | POTEH_BLOCK body real_block {$$=mknode($2,$3,"RETURN_REAL1"); }
       //| POTEH_BLOCK real_block {$$=mknode($2,NULL,"RETURN_REAL2"); }
       | POTEH_BLOCK RETURN call_func SEMICOLON SOGER_BLOCK{$$=mknode($3,NULL,"RETURN_FUNC"); }
;

int_block: RETURN int SEMICOLON SOGER_BLOCK { $$=mknode($2,NULL,"RETURN"); }
	   | RETURN id POTEH int_args SOGER SEMICOLON SOGER_BLOCK { $$=mknode($2,$4,"RETURN"); }
	   | RETURN exp SEMICOLON SOGER_BLOCK { $$=mknode($2,NULL,"RETURN"); }
;

int_args: int ',' int_args {$$=mknode($1,$3,""); }
 	 | int {$$=mknode($1,NULL,""); }
 	 | {$$=mknode(NULL,NULL,"NONE"); }
;


real_block: RETURN real SEMICOLON SOGER_BLOCK { $$=mknode($2,NULL,"RETURN"); }
	   | RETURN id POTEH real_args SOGER SEMICOLON SOGER_BLOCK { $$=mknode($2,$4,"RETURN"); }
;

real_args: real ',' real_args {$$=mknode($1,$3,""); }
 	 | real {$$=mknode($1,NULL,""); }
 	 | {$$=mknode(NULL,NULL,"NONE"); }
;

char_block: RETURN char SEMICOLON SOGER_BLOCK {$$=mknode($2,NULL, "RETURN"); }
	  | RETURN id POTEH char_args SOGER SEMICOLON SOGER_BLOCK {$$=mknode($2,$4,"RETURN"); }
;

char_args: char ',' char_args { $$ = mknode($1, $3, ""); }
	   | char { $$ = mknode($1, NULL, ""); }
	   |  { $$ = mknode(NULL, NULL, "NONE"); }
;

bool_block: RETURN bool SEMICOLON SOGER_BLOCK {$$=mknode($2,NULL, "RETURN"); }
	  | RETURN id POTEH bool_args SOGER SEMICOLON SOGER_BLOCK {$$=mknode($2,$4,"RETURN"); }
;

bool_args: bool {$$=mknode($1,NULL,""); }
;

%% 
//------------Grammer Rules---------End------------


//------------Additional C Code--------------------
#include "lex.yy.c"

int main()
{
	return yyparse();
}

void StartProject(node* root)
{
	Project* Pro = Make_Project(root);
	SyntaxAnalyzer(Pro,root);
	T3AC* T3 = mk3ac();	
	root->TAC = T3;
	T3ac(root);
}


Project* Make_Project(node* root)
{
	Project* newPro = (Project*)malloc(sizeof(Project));
	if(!newPro)
	{
		printf("Error!, Out Of Memory\n");
		return NULL;
	}
	
	newPro->tree = root;
	newPro->scope_Count = 0;
	newPro->Scope = NULL;
	newPro->ht = initTable(1000);
	
	printf("Project has been made\n");
	
	return newPro;
}

int SyntaxAnalyzer(Project* pro ,node* root)
{	
	if(root == NULL)
	{
		return 1;
	}
	
	else
	{
		if(!strcmp(root->token,"") == 0)
		{
			//printf("Token is -------> %s\n" ,root->token);
		}	
	}

	if(strcmp(root->token,"") == 0)
	{
		if(root->left)
		{
			SyntaxAnalyzer(pro,root->left);
		}
	
		if(root->right)
		{
			SyntaxAnalyzer(pro,root->right);
		}
	}
	
	else if(strcmp(root->token, "CODE") == 0)
	{
		stack* s = (stack*)malloc(sizeof(stack));
		PushScopeToHead(pro,s);
		//printf("CODE has been added to scope %d\n", pro->Scope->CurrentScope);
		SyntaxAnalyzer(pro,root->left);
		printHashTabe(pro->ht);
		pro->Scope = DeleteStackElement(pro->ht,pro->Scope);
	}
	
	else if(strcmp(root->token, "FUNCTIONS") == 0)
	{
		SyntaxAnalyzer(pro,root->left);
		SyntaxAnalyzer(pro,root->right);
	}
	
	else if(strcmp(root->token, "FUNCTION") == 0)
	{
		SyntaxAnalyzer(pro,root->left);
	}
	
	else if(strcmp(root->token, "VOID") == 0)
	{
		char* FuncName = root->left->left->left->token;
 		char* typeFunc = "VOID";
 		node* FuncArgs = root->left->right->left;
 		stack* s = (stack*)malloc(sizeof(stack));
 		Args* argslist = NULL;
 		int* Args_Count = (int*)malloc(sizeof(int));
 		*Args_Count = 0;
 		int check;
 		int flag = 0;

		if(strcmp(FuncName,"main") == 0) 
 		{
 			MainFlag++;
 			if(strcmp(FuncArgs->left->token, "NONE_ARGS") != 0)
 			{
 				printf("Main Function cannot get argument\n");
 				exit(1);
 			}
 		}
 		if(MainFlag > 1)
		{
			printf("Only one main function can be declarded!\n");
			exit(1);
		}
		
		if(flag==0)
 		{
 			if(FuncArgs->left->left != NULL)
 			{
 				char* type = FuncArgs->left->left->token;
	 			argslist = (Args*)malloc(sizeof(Args));
	 			argslist->type = strdup(type);
	 			flag=1;
	 		}
 		}
 		
 		
 		check = insert(pro->ht,FuncName,typeFunc, pro->Scope->CurrentScope,Args_Count,argslist);
 		//printf("%s func has added to scope %d\n",FuncName, pro->Scope->CurrentScope);
 		if(check == 5)
 		{
 			Redclare = 5;	
			return 0;
 		}
 		
 
 		pro->Scope->List = addVarInfoToStart(pro->Scope->List,FuncName);
 		PushScopeToHead(pro,s);
 		if(strcmp(FuncArgs->left->token , "NONE_ARGS") != 0)
 		{
 			
 			node* IdsTree = NULL;
 			char* argName = NULL;

 			while(FuncArgs)
 			{
 				IdsTree = FuncArgs->left->right;
 				if(FuncArgs->left->left != NULL)
 				{
 					char* type = FuncArgs->left->left->token;

 					while(IdsTree != NULL)
 					{
 						++(*Args_Count);
 						
 						if(flag > 1)
 							argslist = addArg(argslist,BuildArgsNode(type));
 						flag++;
 						argName = IdsTree->left->left->token; 
 						s->List = addVarInfoToStart(s->List, argName);
 						check = insert(pro->ht, argName, type, pro->Scope->CurrentScope, NULL,NULL);
 						if(check == 5)
 						{
							Redclare = 5;
							exit(1); 						
 						}
 						IdsTree = IdsTree->right;
 					}
 				}
 				FuncArgs = FuncArgs->right;

 			}
 		}

		SyntaxAnalyzer(pro,root->left->right->right->left->left);
		pro->scope_Count--;
		pro->Scope = DeleteStackElement(pro->ht,pro->Scope);
		//printf("Function End!\n");
	}
	
	else if(strcmp(root->token, "FUNC") == 0)
	{
		char* FuncName = root->right->left->left->token;
 		char* typeFunc = root->left->token;
 		node* FuncArgs = root->right->right->left;
 		stack* s = (stack*)malloc(sizeof(stack));
 		Args* argslist = NULL;
 		int* Args_Count = (int*)malloc(sizeof(int));
 		*Args_Count = 0;
 		int check;
		int flag=0;
		
		if(flag==0)
 		{
 			if(FuncArgs->left->left != NULL)
 			{
 				char* type = FuncArgs->left->left->token;
	 			argslist = (Args*)malloc(sizeof(Args));
	 			argslist->type = strdup(type);
	 			flag=1;
	 		}
 		}
 		
 		check = insert(pro->ht,FuncName,typeFunc, pro->Scope->CurrentScope,Args_Count,argslist);
 		
 		//printf("%s func has added to scope %d\n",FuncName, pro->Scope->CurrentScope);
 		if(check == 5)
 		{
 			Redclare = 5;	
			return 0;
 		}
 		
 		pro->Scope->List = addVarInfoToStart(pro->Scope->List,FuncName);
 		PushScopeToHead(pro,s);
 		
 		insert(pro->ht,"Return",typeFunc,s->CurrentScope,NULL,NULL);
 		s->List = addVarInfoToStart(pro->Scope->List,"Return");
 		
 		if(strcmp(FuncArgs->left->token , "NONE_ARGS") != 0)
 		{
 			node* IdsTree = NULL;
 			char* argName = NULL;
 			while(FuncArgs)
 			{
 				IdsTree = FuncArgs->left->right;
 				if(FuncArgs->left->left != NULL)
 				{
 					char* type = FuncArgs->left->left->token;
 					while(IdsTree != NULL)
 					{
 						++(*Args_Count);
						if(flag > 0)
 							argslist = addArg(argslist,BuildArgsNode(type));
 						flag++;
 						argName = IdsTree->left->left->token; 
 						s->List = addVarInfoToStart(s->List, argName);
 						check = insert(pro->ht, argName, type, s->CurrentScope, NULL,NULL);
 						//printf("%s has been added to scope %d\n", argName, pro->Scope->CurrentScope);
 						if(check == 5)
 						{
							Redclare = 5;
							exit(1); 						
 						}
 						
 						IdsTree = IdsTree->right;
 					}
 				}
 				FuncArgs = FuncArgs->right;
 			}
 			LinkedList** FuncToHash = place(pro->ht, FuncName);
			(*FuncToHash)->Decl->args = DeleteArgsElement((*(FuncToHash))->Decl->args);
			
 		}
 		
		//CheckReturnType(root->right->right->right, typeFunc);
		SyntaxAnalyzer(pro,root->right->right->right->left);
		SyntaxAnalyzer(pro,root->right->right->right->right);
		root->type = root->right->right->right->right->type;
		if(strcmp(root->type, typeFunc) != 0)
		{
			printf("Function Return Type Error! %s\n", FuncName);
			exit(1);
		}
		pro->scope_Count--;
		pro->Scope = DeleteStackElement(pro->ht,pro->Scope);
		//printf("Function End!\n");
	}
	
	else if(strcmp(root->token, "EQUAL") == 0)
	{
		SyntaxAnalyzer(pro,root->left);
	}
	
	else if(strcmp(root->token, "RETURN") == 0)
	{
		SyntaxAnalyzer(pro,root->left);
		root->type = root->left->type;
	}
	
	else if(strcmp(root->token, "CALL_FUNC") == 0)
	{
		SyntaxAnalyzer(pro,root->left);
		if(root->right != NULL)
		{
			if(CheckFuncArgs(pro->ht,root->right->left,root->left->left->token) == 0)
			{
				printf("Function Argument are not feet!\n");
				exit(1);
			}
		}
		root->type = ReturnType(pro->ht,root->left->left->token);
	}

	else if (strcmp(root->token, "RETURN") == 0 )
	{
		if(strcmp(root->left->token, "CHAR_VAL") == 0 )
		{
			root->value = root->left->left->token;
			root->left->value = root->value;
			//printf("%s\n",root->left->value);
		}
	}
	
	else if (strcmp(root->token, "BLOCK") == 0 )
	{
		stack* s = (stack*)malloc(sizeof(stack));
		PushScopeToHead(pro, s);
		SyntaxAnalyzer(pro,root->left);
		pro->scope_Count--;
		pro->Scope = DeleteStackElement(pro->ht,pro->Scope);
	}
	
	else if(strcmp(root->token, "EXP") == 0)
	{
		SyntaxAnalyzer(pro,root->left);
	}
	
	else if (strcmp(root->token, "++") == 0 || strcmp(root->token, "--") == 0)
	{
		SyntaxAnalyzer(pro,root->left);
	}
	
	else if (strcmp(root->token, "DO") == 0 )
	{
		SyntaxAnalyzer(pro,root->left);
		SyntaxAnalyzer(pro,root->right);
	}
	
	else if (strcmp(root->token, "WHILE_DO") == 0 )
	{
		if(CheckCondition(root->left) == false)
		{
			printf("While statment must be bool\n");
			exit(1);
		}
		
		SyntaxAnalyzer(pro, root->left);
		//printf("WHILE DONE\n");
	}
	
	else if (strcmp(root->token, "WHILE") == 0 )
	{
		if(CheckCondition(root->left) == false)
		{
			printf("While statment must be bool\n");
			exit(1);
		}
		
		SyntaxAnalyzer(pro, root->left);
		SyntaxAnalyzer(pro, root->right);
		//printf("WHILE DONE\n");
	}
	
	else if (strcmp(root->token, "FOR") == 0 )
	{
		SyntaxAnalyzer(pro,root->left->left->left);
		SyntaxAnalyzer(pro,root->left->left->right->left);
		SyntaxAnalyzer(pro,root->left->left->right->right->right);
		SyntaxAnalyzer(pro,root->left->right);
		SyntaxAnalyzer(pro,root->right);
		//printf("FOR DONE\n");
	}
	
	else if (strcmp(root->token, "=") == 0 )
	{
		SyntaxAnalyzer(pro,root->left);
		SyntaxAnalyzer(pro,root->right);
		TypeGiving(pro->ht, root);
		if(strcmp(root->left->type,"BOOL") == 0)
		{
	
		}
		else
		{
			ChangeType(pro->ht, root->left->left->token,root->right->type);
		}
	}
	
	else if (strcmp(root->token, "EQ_RIGHT") == 0 )
	{
		SyntaxAnalyzer(pro,root->left);
		root->type = root->left->type;
	}	
	
	else if (strcmp(root->token, "+") == 0 || strcmp(root->token, "-") == 0 || strcmp(root->token, " * ") == 0 || strcmp(root->token, "/") == 0 )
	{
		SyntaxAnalyzer(pro,root->left);
		SyntaxAnalyzer(pro,root->right);
		//printf("LEFT TYPE = %s\t RIGHT TYPE = %s\n", root->left->type,root->left->type);
		root->type = TypeGiving(pro->ht, root);
	}
	
	else if(strcmp(root->token, "[ ]") == 0 )
	{
		char* type = "STRING";
		char* name = root->left->left->token;
		int check = insert(pro->ht, name, type, pro->Scope->CurrentScope ,NULL, NULL);
		if(check == 5) 
		{
			Redclare = 5;	
			return 0;
		}
		pro->Scope->List = addVarInfoToStart(pro->Scope->List, name);
	}
	
	else if(strcmp(root->token, "STRING_VALUE") == 0 )
	{
		SyntaxAnalyzer(pro,root->left);
		root->type = root->left->type;
	}
	
	else if(strcmp(root->token, "STR_VAL") == 0 )
	{
		root->value = root->left->token;
		root->type = "STRING";
	}
	
	else if(strcmp(root->token, "CHAR_ASS") == 0 )
	{
		SyntaxAnalyzer(pro,root->left->left);
		root->type = "CHAR";
	}
	
	else if(strcmp(root->token, "STR_ASS") == 0 )
	{
		char* type = "STRING";
		char* name = root->left->left->left->token;
		int check = insert(pro->ht, name, type, pro->Scope->CurrentScope ,NULL, NULL);
		if(check == 5) 
		{
			Redclare = 5;	
			return 0;
		}
		pro->Scope->List = addVarInfoToStart(pro->Scope->List, name);
	}
	
	else if(strcmp(root->token, "INT_HEX") == 0 )
	{
		root->value = root->left->token;
		root->type = "INT";
	}
	
	else if(strcmp(root->token, "INT_DEC") == 0 )
	{
		root->value = root->left->token;
		root->type = "INT";
	}	
	
	else if(strcmp(root->token, "REAL_VAL") == 0 )
	{	
		root->type = "REAL";
		root->value = root->left->token;
	}	
	
	else if(strcmp(root->token, "BOOL_VALUE") == 0 )
	{
		SyntaxAnalyzer(pro,root->left);
		root->type = root->left->type;
	}
	
	else if(strcmp(root->token, "CHAR_VALUE") == 0 )
	{
		SyntaxAnalyzer(pro,root->left);
		root->type = root->left->type;
	}
	
	else if(strcmp(root->token, "REAL_VALUE") == 0 )
	{
		SyntaxAnalyzer(pro,root->left);
		root->type = root->left->type;
	}
	
	else if (strcmp(root->token, "INT_VALUE") == 0 )
	{
		SyntaxAnalyzer(pro,root->left);
		root->type = root->left->type;
	}
	
	else if (strcmp(root->token, "CHAR_VAL") == 0 )
	{
		root->type = "CHAR";
		root->value = root->left->token;
	}
	
	else if (strcmp(root->token, "VALUE") == 0 )
	{
		SyntaxAnalyzer(pro,root->left);
		root->type = root->left->type;
	}	
		
	else if (strcmp(root->token, "IF_ELSE") == 0 )
	{
		SyntaxAnalyzer(pro,root->left);
		SyntaxAnalyzer(pro,root->right);
		
		//printf("IF_ELSE DONE\n");
	}
	
	else if (strcmp(root->token, "STAT") == 0 )
	{
		SyntaxAnalyzer(pro,root->left);
	}
	
	else if (strcmp(root->token, "<") == 0 || strcmp(root->token, ">") == 0 || strcmp(root->token, "<=") == 0 || strcmp(root->token, ">=") == 0 )
	{
		SyntaxAnalyzer(pro,root->left);
		SyntaxAnalyzer(pro,root->right);
		TypeGiving(pro->ht, root);
		root->type = "BOOL";
	}
	
	else if (strcmp(root->token, "==") == 0 || strcmp(root->token, "!=") == 0 )
	{
		SyntaxAnalyzer(pro,root->left);
		SyntaxAnalyzer(pro,root->right);
		TypeGiving(pro->ht, root);
		root->type = "BOOL";
	}
	
	else if (strcmp(root->token, "&&") == 0 || strcmp(root->token, "||") == 0 )
	{
		SyntaxAnalyzer(pro,root->left);
		SyntaxAnalyzer(pro,root->right);
		TypeGiving(pro->ht, root);
		root->type = "BOOL";
	}
	
	else if (strcmp(root->token, "ID_=_CF") == 0 )
	{
		SyntaxAnalyzer(pro,root->left);
		root->type = root->left->type;
	}
	
	else if (strcmp(root->token, "IF") == 0 )
	{
		if(CheckCondition(root->left) == false)
		{
			printf("If statment must be bool\n");
			exit(1);
		}
		
		SyntaxAnalyzer(pro, root->left);
		SyntaxAnalyzer(pro, root->right);
		//printf("IF DONE\n");
	}
	
	else if (strcmp(root->token, "ELSE") == 0 )
	{
		SyntaxAnalyzer(pro, root->left);
		//printf("ELSE DONE\n");
	}

	else if(strcmp(root->token, "REAL_ID") == 0)
	{
		SyntaxAnalyzer(pro, root->left);
		root->type = root->left->type;
	}
	
	else if(strcmp(root->token, "*") == 0)
	{
		if(strcmp(root->left->token,"ID") == 0)
		{
			if(strcmp(ReturnType(pro->ht ,root->left->left->token), "INT_POINT") != 0 && strcmp(ReturnType(pro->ht ,root->left->left->token), "REAL_POINT") != 0 && strcmp(ReturnType(pro->ht ,root->left->left->token), "CHAR_POINT") != 0)
			{
				printf("Operator * can turn on pointers only!\n");
				exit(1);
			}
			SyntaxAnalyzer(pro, root->left);
		}
		
		else
		{
			SyntaxAnalyzer(pro, root->left);
			root->type = root->left->type;
		}
	}
	
	else if(strcmp(root->token, "ADDRESS") == 0)
	{
		SyntaxAnalyzer(pro, root->left);
		root->type = root->left->type;
	}
	
	else if(strcmp(root->token, "&") == 0)
	{
		//printf("root type %s\n", root->left->left->token);
		if(strcmp(root->left->token, "[ ]") == 0)
		{
			
		}
		else if(strcmp(ReturnType(pro->ht ,root->left->left->token), "CHAR") != 0 && strcmp(ReturnType(pro->ht, root->left->left->token), "INT") != 0 && strcmp(ReturnType(pro->ht, root->left->left->token), "REAL") != 0)
		{
			printf("Operator & can turn on char, int or real only!\n");
			exit(1);
		}
		SyntaxAnalyzer(pro, root->left);
		//printf("$#$#$       %s\n", root->left->type);
		if(strcmp(root->left->type,"INT") == 0)
		{
			root->type = "INT_POINT";
		}
		if(strcmp(root->left->type,"REAL") == 0)
		{
			root->type = "REAL_POINT";
		}
		if(strcmp(root->left->type,"CHAR") == 0)
		{
			root->type = "CHAR_POINT";
		}
		if(strcmp(root->left->type,"STRING") == 0)
		{
			root->type = "STRING";
		}
		
	}
	
	else if(strcmp(root->token, "VAR:") == 0)
	{
		char* type = root->left->token;
		node* varArg = root->right;
		int check;
		while(varArg)
		{
			if(strcmp(varArg->token,"VAR_STAT") == 0)
			{
				check = insert(pro->ht,varArg->left->left->token,type ,pro->Scope->CurrentScope, NULL, NULL);
				//printf("%s has been added to scope %d\n", varArg->left->left->token,pro->Scope->CurrentScope);
				if(check == 5) 
				{
					Redclare = 5;	
					exit(1);
				}
				pro->Scope->List = addVarInfoToStart(pro->Scope->List, varArg->left->left->token);
			}
			
			else 
			{
				check = insert(pro->ht,varArg->left->left->left->token,type ,pro->Scope->CurrentScope, NULL, NULL);
				//printf("%s has been added to scope %d\n", varArg->left->left->left->token,pro->Scope->CurrentScope);
				if(check == 5) 
				{
					Redclare = 5;	
					exit(1);
				}
				pro->Scope->List = addVarInfoToStart(pro->Scope->List, varArg->left->left->left->token);
				SyntaxAnalyzer(pro, varArg->left);
			}
			varArg = varArg->right;
		}
		
	}
	
	else if(strcmp(root->token, "VAR") == 0)
	{
		char* type = root->left->token;
		node* varArg = root->right;
		while(varArg)
		{		
			int check = insert(pro->ht, varArg->left->left->token, type, pro->Scope->CurrentScope ,NULL, NULL);
			//printf("%s has been added to scope %d\n", varArg->left->left->token,pro->Scope->CurrentScope);
			if(check == 5) 
			{
				Redclare = 5;	
				exit(1);
			}
			pro->Scope->List = addVarInfoToStart(pro->Scope->List, varArg->left->left->token);
			varArg = varArg->right;
		}		
	}
	
	else if(strcmp(root->token, "ID") == 0)
	{
		if(checkIfIDInHT(pro->ht,root->left->token,pro->scope_Count) == false)
		{
			exit(1);
		}
		root->left->type = strdup(ReturnType(pro->ht ,root->left->token));
		root->type = root->left->type;
	}
	
	else if(strcmp(root->token, "ONE_ARG") == 0)
	{
		SyntaxAnalyzer(pro, root->left);
	}
}

char* ReturnType(HashTable *ht ,char* name)
{
	
	char* type;
	int hashing = hash(name, ht);
	if (search(ht, name) == 1)
	{
		LinkedList* lnode = getNode(ht->hashtable[hashing].chain, name);
		while(lnode)
		{
			if(strcmp(lnode->name, name) == 0)
			{
				type = lnode->Decl->type;
			}
			lnode = lnode->next;
		}
		
	}
	
	else if(strcmp(name, "CHAR_VAL") == 0 || strcmp(name, "CHAR_ID") == 0)
	{
		return "CHAR";
	}	
	
	else if(strcmp(name, "REAL_VAL") == 0 || strcmp(name, "REAL_ID") == 0)
	{
		return "REAL";
	}
	
	else if(strcmp(name, "INT_DEC") == 0 || strcmp(name, "INT_ID") == 0 || strcmp(name, "INT_HEX") == 0)
	{
		return "INT";
	}
	
	else 
	{
		return NULL;
	}
	return type;
}

int CheckFuncArgs(HashTable* ht, node* root, char* name)
{
	int *numofargs;
	numofargs = (ht->hashtable[hash(name,ht)].chain->Decl->Args_Count);
	Args* FunctionArgs = (ht->hashtable[hash(name,ht)].chain->Decl->args);
	
	int count = ReturnNumOfArgs(root);
	
	if(count != *numofargs)
	{
		if(count > *numofargs)
		{
			printf("Too many Arguments to function %s\n", name);
			exit(1);
		}
		
		else
		{
			printf("Too few Arguments to function %s\n", name);
			exit(1);
		}
	}
	if(strcmp(root->token, "FUNC_EXP") == 0)
	{
		CheckExp(root->left,FunctionArgs,ht);
		return 1;
	}
	CheckFuncTypes(root->left,name,FunctionArgs,ht);
	
	return 1;
}

int CheckExp(node* root, Args* args ,HashTable* ht)
{
	if(root == NULL)
	{
		return 1;
	}
	
	else if(strcmp(root->token, "") == 0)
	{
		if(root->left)
		{
			CheckExp(root->left, args, ht);
		}
		if(root->right)
		{
			CheckExp(root->right,args,ht);
		}
	}
	if(strcmp(root->token, "+") == 0 || strcmp(root->token, "-") == 0 || strcmp(root->token, "/") == 0 || strcmp(root->token, " * ") == 0)
	{
		CheckExp(root->left,args , ht);
		CheckExp(root->right, args, ht);
	}
	
	else if(strcmp(root->token, "VALUE") == 0)
	{
		CheckExp(root->left->left,args, ht);
	}
	
	else if(strcmp(root->token, "INT_DEC") == 0)
	{
		if(strcmp(ReturnType(ht, root->left->token),args->type) != 0)
		{
			printf("Function Type are not feet to what to the function should get\n");
			exit(1);
		}		
	}
	else if(strcmp(root->token, "INT_HEX") == 0)
	{
		if(strcmp(ReturnType(ht, root->left->token),args->type) != 0)
		{
			printf("Function Type are not feet to what to the function should get\n");
			exit(1);
		}		
	}
	
	else if(strcmp(root->token, "REAL_VAL") == 0)
	{
		if(strcmp(ReturnType(ht, root->left->token),args->type) != 0)
		{
			printf("Function Type are not feet to what to the function should get\n");
			exit(1);
		}		
	}
	
	else if(strcmp(root->token, "CHAR_VAL") == 0)
	{
		if(strcmp(ReturnType(ht, root->left->token),args->type) != 0)
		{
			printf("Function Type are not feet to what to the function should get\n");
			exit(1);
		}		
	}
	
	else if(strcmp(root->token, "REAL_ID") == 0)
	{
		CheckExp(root->left,args , ht);
	}
	
	else if(strcmp(root->token, "CHAR_ID") == 0)
	{
		CheckExp(root->left,args , ht);
	}
		
	else if(strcmp(root->token, "ID") == 0)
	{
		if(strcmp(ReturnType(ht, root->left->token),args->type) != 0)
		{
			printf("Function Type are not feet to what to the function should get\n");
			exit(1);
		}		
	}
	
}

int CheckFuncTypes(node* root, char* name, Args* args, HashTable* ht)
{
	
	//printf("%s\n", root->token);
	while(root != NULL && args != NULL)
	{
		//printf("%s ttttt\n", root->left->left->left->token);
		if(strcmp(root->left->left->left->token,"ID") == 0)
		{
			//printf("AFSDFSADFSADFD\n");
			if(strcmp(ReturnType(ht, root->left->left->left->left->token),args->type) != 0)
			{
				printf("Function Type are not feet to what to the function should get\n");
				exit(1);
			}
			
			root = root->right;
			args = args->next;
		}
		else
		{
			if(strcmp(ReturnType(ht, root->left->left->token),args->type) != 0)
			{
				printf("Function Type are not feet to what to the function should get\n");
				exit(1);
			}
			root = root->right;
			args = args->next;
		}
		
	}
	
	return 1;
}

int ReturnNumOfArgs(node* root)
{
	int count = 0;
	if(strcmp(root->token, "FUNC_EXP") == 0)
	{
		return 1;
	}
	root = root->left;
	while(root)
	{
		
		count++;
		root = root->right;
	}
	
	return count;
}

char* TypeGiving(HashTable *ht, node* root)
{
	if(root == NULL)
	{
		return 1;
	}
	
	else
	{
		if(!strcmp(root->token,"") == 0)
		{
			//printf("Token is -----53--------> %s\n" ,root->token);
		}	
	}
	if(strcmp(root->token,"+") == 0 || strcmp(root->token," * ") == 0 || strcmp(root->token,"-") == 0 || strcmp(root->token,"/") == 0) 
	{
		if(strcmp(root->left->type,"REAL") == 0 && strcmp(root->right->type,"INT") == 0) 
		{
			return "REAL";
		}
		
		else if(strcmp(root->left->type,"INT") == 0 && strcmp(root->right->type,"REAL") == 0) 
		{
			return "REAL";
		}
		
		else if(strcmp(root->left->type,"INT") == 0 && strcmp(root->right->type,"INT") == 0) 
		{
			return "INT";
		}
		
		else if(strcmp(root->left->type,"REAL") == 0 && strcmp(root->right->type,"REAL") == 0) 
		{
			return "REAL";
		}
		
		else if(strcmp(root->left->type,"CHAR") == 0 && strcmp(root->right->type,"CHAR") == 0) 
		{
			return "CHAR";
		}
		
		else if(strcmp(root->left->type,"BOOL") == 0 && strcmp(root->right->type,"BOOL") == 0) 
		{
			return "BOOL";
		}
		
		else if(strcmp(root->left->type,"INT_POINT") == 0 && strcmp(root->right->type,"INT") == 0) 
		{
			return "INT_POINT";
		}
		
		else if(strcmp(root->left->type,"INT") == 0 && strcmp(root->right->type,"INT_POINT") == 0) 
		{
			return "INT_POINT";
		}
		
		else if(strcmp(root->left->type,"INT_POINT") == 0 && strcmp(root->right->type,"INT_POINT") == 0) 
		{
			return "INT_POINT";
		}
		
		else if(strcmp(root->left->type,"REAL_POINT") == 0 && strcmp(root->right->type,"REAL_POINT") == 0) 
		{
			return "REAL_POINT";
		}
		
		else if(strcmp(root->left->type,"INT_POINT") == 0 && strcmp(root->right->type,"REAL_POINT") == 0) 
		{
			return "REAL_POINT";
		}
		
		else if(strcmp(root->left->type,"REAL_POINT") == 0 && strcmp(root->right->type,"INT_POINT") == 0) 
		{
			return "REAL_POINT";
		}
		
		else if(strcmp(root->left->type,"REAL_POINT") == 0 && strcmp(root->right->type,"INT") == 0) 
		{
			return "REAL_POINT";
		}
		
		else if(strcmp(root->left->type,"INT") == 0 && strcmp(root->right->type,"REAL_POINT") == 0) 
		{
			return "REAL_POINT";
		}
	}

	if(strcmp(root->token,"<") == 0 || strcmp(root->token,">") == 0 || strcmp(root->token,"<=") == 0 || strcmp(root->token,">=") == 0) 
	{
		if(strcmp(root->left->type,"REAL") == 0 && strcmp(root->left->type,"REAL") == 0)
		{
			//printf("Type between operand good\n");
		}
		else if(strcmp(root->left->type,"REAL") == 0 && strcmp(root->left->type,"INT") == 0)
		{
			//printf("Type between operand good\n");
		}
		
		else if(strcmp(root->left->type,"INT") == 0 && strcmp(root->left->type,"INT") == 0)
		{
			//printf("Type between operand good\n");
		}
		else if(strcmp(root->left->type,"INT") == 0 && strcmp(root->left->type,"REAL") == 0)
		{
			//printf("Type between operand good\n");
		}
		
		else
		{
			printf("TYPES BETWEEN OPERAND MUST BE INT OR REAL!\n");
			exit(1);
		}
	}

	if(strcmp(root->left->type,"BOOL") == 0 && strcmp(root->right->type,"CHAR") == 0) 
	{
		printf("Types not feet!\n");
		exit(1);
	}
	
	else if(strcmp(root->left->type,"INT") == 0 && strcmp(root->right->type,"BOOL") == 0) 
	{
		printf("Types not feet!\n");
		exit(1);
	}
		
	else if(strcmp(root->left->type,"BOOL") == 0 && strcmp(root->right->type,"INT") == 0) 
	{
		printf("Types not feet!\n");
		exit(1);
	}
	
	else if(strcmp(root->left->type,"CHAR") == 0 && strcmp(root->right->type,"BOOL") == 0) 
	{
		printf("Types not feet!\n");
		exit(1);
	}
		
	else if(strcmp(root->left->type,"INT") == 0 && strcmp(root->right->type,"CHAR") == 0) 
	{
		printf("Types not feet!\n");
		exit(1);
	}
		
	else if(strcmp(root->left->type,"CHAR") == 0 && strcmp(root->right->type,"INT") == 0) 
	{
		printf("Types not feet!\n");
		exit(1);
	}
		
	else if(strcmp(root->left->type,"CHAR") == 0 && strcmp(root->right->type,"REAL") == 0) 
	{
		printf("Types not feet!\n");
		exit(1);
	}
		
	else if(strcmp(root->left->type,"REAL") == 0 && strcmp(root->right->type,"CHAR") == 0) 
	{
		printf("Types not feet!\n");
		exit(1);
	}
		
	else if(strcmp(root->left->type,"INT") == 0 && strcmp(root->right->type,"BOOL") == 0) 
	{
		printf("Types not feet!\n");
		exit(1);
	}
		
	else if(strcmp(root->left->type,"REAL") == 0 && strcmp(root->right->type,"BOOL") == 0) 
	{
		printf("Types not feet!\n");
		exit(1);
	}
	
	else if(strcmp(root->left->type,"BOOL") == 0 && strcmp(root->right->type,"REAL") == 0) 
	{
		printf("Types not feet!\n");
		exit(1);
	}
	
	else return "";
}

int CheckReturnType(node* root, char* type)
{
	if(strcmp(type, "CHAR") == 0)
	{
		if(strcmp(root->token,"RETURN_CHAR1") != 0)
		{
			printf("Function should return char!\n");
			exit(1);
		}
	}
	
	if(strcmp(type, "INT") == 0)
	{
		if(strcmp(root->token,"RETURN_INT1") != 0)
		{
			printf("Function should return int!\n");
			exit(1);
		}
	}
	
	if(strcmp(type, "BOOL") == 0)
	{
		if(strcmp(root->token,"RETURN_BOOL1") != 0)
		{
			printf("Function should return bool!\n");
			exit(1);
		}
	}
	
	if(strcmp(type, "REAL") == 0)
	{
		if(strcmp(root->token,"RETURN_REAL1") != 0)
		{
			printf("Function should return real!\n");
			exit(1);
		}
	}
	
}

void ChangeType(HashTable* ht, char* name,char* type)
{
	char* temp = "";
	int hashing = hash(name, ht);
	if (search(ht, name) == 1)
	{
		
		LinkedList* lnode = getNode(ht->hashtable[hashing].chain, name);
		char* temp = lnode->Decl->type;
		lnode->Decl->type = strdup(type);
	}
	
	//printf("Var %s has been change from type %s to type %s\n" ,temp,type);
}

bool checkIfIDInHT(HashTable* ht, char* name, int scopeId)
{
	
	int hashing = hash(name, ht);
	if (search(ht, name) == 1)
	{
		
		LinkedList* lnode = getNode(ht->hashtable[hashing].chain, name);
		while(lnode)
		{
			if(strcmp(lnode->name, name) == 0)
			{
				return true;
			}
			lnode = lnode->next;
		}
	}
	
	else
	{
		printf("%s not declarded\n", name);
		return false;
	}
	
}

bool CheckCondition(node* root)
{
	if(strcmp(root->left->token,"EXP") == 0)
	{
		return false;
	}
	
	return true;
}

Args* DeleteArgsElement(Args* head)
{
	Args* temp = head->next;
	head->next = NULL;
	FreeArgsList(head);

	head = temp;
	return head;
}

Args* BuildArgsNode(char* type)
{
	Args* temp = (Args*)malloc(sizeof(Args));

	int len = strlen(type);
	temp->type = (char*)malloc(len + 1);
	if (temp->type == NULL) {
		printf("out of memory\n");
		exit(1);
	}

	temp->type[len] = '\0';
	strcpy(temp->type, type);

	temp->next = NULL;

	return temp;
}

VarInfo* addVarInfoToStart(VarInfo* head, char* id)
{
	
	VarInfo* new_head = mkVarInfo(id);
	new_head->next = head;
	head = new_head;
	//printf("%s has been added to scope\n" , id);
	return head;
}

Args* addArg(Args* head, Args* new_chain)
{
	Args* connect = NULL;

	if (head == NULL)
		head = new_chain;
	else
	{
		for (connect = head; connect->next != NULL; connect = connect->next);
		connect->next = new_chain;
	}
	return head;
}


void printHashTabe(HashTable* ht)
{
	if (ht)
	{
		printf("Number of elements: %d\n", ht->numOfElements);
		printf("Number of cells taken: %d\n", ht->cellsTaken);
		for (int i = 0, counter = 0; i < ht->tableSize; ++i) 
		{
			if (ht->hashtable[i].chain != NULL) 
			{
				printf("%d. \n", ++counter);
				PrintList(ht->hashtable[i].chain);
				printf("\n");
			}
		}
	}
}

void PrintList(LinkedList* head)
{
	while (head != NULL) {
		printf("\tFuncName:  \"%s\"\n", head->name);
		printDecList(head->Decl);
		head = head->next;
	}
	printf("\n");
}

int insert(HashTable* ht, char* name, char* type, int scopeId, int* Args_Count, Args* args)
 {
	if (ht == NULL || name == NULL || ht->hashtable == NULL) 
	{
		return 0;
	}
	
	int hashing = hash(name, ht);
	
	if (search(ht, name) == 1)
	{
		
		LinkedList* lnode = getNode(ht->hashtable[hashing].chain, name);
		if(lnode->Decl->scopeid == scopeId)
		{
			printf("%s has all ready been declarded in scope %d\n" , name , scopeId);
			return 5;
		}
		lnode->Decl = addDecToStart(lnode->Decl, type, scopeId, Args_Count, args);
	}
	else
	{
		if (ht->hashtable[hashing].chain == NULL)
		{
			++ht->cellsTaken;
		}
			
		ht->hashtable[hashing].chain = addToStart(ht->hashtable[hashing].chain, name, type, scopeId, Args_Count, args);
		++ht->numOfElements;
	}
	
	
	return 1;
}

Declaration* addDecToStart(Declaration* head, char* type, int scopeId, int* Args_Count, Args* args)
{
	Declaration* new_head;
	new_head = BuildDecNode(type, scopeId, Args_Count, args);
	new_head->next = head;

	return new_head;
}

Declaration* BuildDecNode(char* type, int scopeId, int* Args_Count, Args* args) 
{
	Declaration* temp = (Declaration*)malloc(sizeof(Declaration));
	if (temp == NULL) {
		printf("out of memory\n");
		exit(1);
	}

	int len = strlen(type);
	temp->type = (char*)malloc(len + 1);
	if (temp->type == NULL) {
		printf("out of memory\n");
		exit(1);
	}
	temp->type[len] = '\0';
	strcpy(temp->type, type);

	temp->scopeid = scopeId;

	if (Args_Count)
	{
		temp->Args_Count = Args_Count;
	}
	else
	{
		temp->Args_Count = NULL;
	}

	if (args)
	{
		temp->args = args;
	}
	else
	{
		temp->args = NULL;
	}

	temp->next = NULL;

	return temp;
}

LinkedList* getNode(LinkedList* head, char* name) 
{
	while (head != NULL) {
		if (strlen(name) == strlen(head->name) && strcmp(name, head->name) == 0) 
			return head;
		head = head->next;
	}
	return NULL;
}

LinkedList* addToStart(LinkedList* head, char* new_node, char* type, int scopeId, int* Args_Count, Args *args) 
{
	LinkedList* new_head;
	new_head = BuildNode(new_node, type, scopeId, Args_Count, args);
	new_head->next = head;
	return new_head;
}

int isInList(LinkedList* head, char* value)
{
	if (getNode(head, value))
	{
		return 1;
	}
	
	return 0;
}

LinkedList* BuildNode(char* name, char* type, int scopeId, int* Args_Count, Args* args)
{
	LinkedList* temp = (LinkedList*)malloc(sizeof(LinkedList));
	if (temp == NULL) {
		printf("out of memory\n");
		exit(1);
	}

	int len = strlen(name);
	temp->name = (char*)malloc(len + 1);
	if (temp->name == NULL)
	{
		printf("out of memory\n");
		exit(1);
	}
	temp->name[len] = '\0';
	strcpy(temp->name, name);

	temp->Decl = BuildDecNode(type, scopeId, Args_Count, args);

	temp->next = NULL;
	return temp;
}

int search(HashTable* ht, char* str)
{
	if (ht == NULL || str == NULL || ht->hashtable == NULL || !isInList(ht->hashtable[hash(str, ht)].chain, str))
		return 0;
	return 1;
}

HashTable* initTable(int tableSize)
{
	HashTable* new_hash_table = (HashTable*)malloc(sizeof(HashTable)); 
	if (new_hash_table == NULL)
	{
		printf("out of memory\n");
		return NULL;
	}
	new_hash_table->tableSize = tableSize;
	new_hash_table->hashtable = (HashTableElement*)malloc(tableSize * sizeof(HashTableElement));
	if (new_hash_table->hashtable == NULL)
	{
		free(new_hash_table);
		printf("out of memory\n");
		return NULL;
	}
	for (int i = 0; i < tableSize; ++i)
		new_hash_table->hashtable[i].chain = NULL;
	new_hash_table->hashFunction = 3;
	new_hash_table->cellsTaken = 0;
	new_hash_table->numOfElements = 0;

	return new_hash_table;
}

stack* PushScopeToHead(Project *proj, stack* scope)
{
	scope->PreviousScope = proj->Scope;
	proj->Scope = scope;
	scope->CurrentScope = ++(proj->scope_Count);
	return scope;
}

stack* DeleteStackElement(HashTable *ht, stack* head)
{ 
	stack* temp = head->PreviousScope;
	head->PreviousScope = NULL;
	FreeStacks(ht, head);
	head = temp;
	//printf("Stack element has been deleted\n");
	return head;
}

VarInfo* FreeIdList(HashTable* ht, VarInfo* head)
{
	if (head == NULL)
	{
		return head;
	}
	DelVarFromSymbolTable(ht, head->name);
	
	if (head->next != NULL)
	{
		FreeIdList(ht, head->next);
	}
	free(head->name);
	free(head);
	head = NULL;
	return head;
}

stack* FreeStacks(HashTable *ht, stack* head)
{
	if (head == NULL)
	{
		return head;
	}
	head->List = FreeIdList(ht, head->List);
	if (head->PreviousScope != NULL)
		FreeStacks(ht, head->PreviousScope);
	free(head);
	head = NULL;

	return head;
}

void DelVarFromSymbolTable(HashTable* ht, char* name)
{
	LinkedList** id = place(ht, name);
	if ((*id)->Decl)
	{
		(*id)->Decl = DeleteDecElement((*id)->Decl);
	}
	if (!((*id)->Decl))
	{
		*id = DelElement((*id), name);
		--ht->numOfElements;
		if (*id == NULL)
			--ht->cellsTaken;
	}
}

Args* FreeArgsList(Args* head)
{
	if (head == NULL)
		return head;
	if (head->next != NULL)
		FreeArgsList(head->next);
	free(head->type);
	free(head);
	head = NULL;

	return head;
}

Declaration* FreeDecList(Declaration* head)
{
	if (head == NULL)
		return head;
	if (head->next != NULL)
		FreeDecList(head->next);
	FreeArgsList(head->args);
	free(head->Args_Count);
	free(head->type);
	free(head);
	head = NULL;

	return head;
}


LinkedList* DelElement(LinkedList* head, char* value)
{

	LinkedList* temp = NULL;
	while (head != NULL)
	{
		if (strlen(value) == strlen(head->name) && strcmp(value, head->name) == 0)
		{ 
			temp = head->next;
			
			DeleteDecElement(head->Decl);
			free(head->name);
			free(head);
			head = temp;
			return head;
		}
		head = head->next;
	}
	return head;
}

Declaration* DeleteDecElement(Declaration* head)
{
	if(head == NULL)
	{
		return head;
	}
	Declaration* temp = head->next;
	head->next = NULL;
	FreeDecList(head);
	head = temp;
	return head;
}

LinkedList** place(HashTable* ht, char* str) 
{
	return &(ht->hashtable[hash(str, ht)].chain);
}

int improvedHashFunction(char* str) 
{
	int sum = 0;
	for (int i = 0, len = strlen(str); i < len; i++) { 
		sum += (int)(pow(31, (len - i - 1)) * str[i]); 
	}
	return sum;
}

int constantStringHashFunction(char* str) 
{
	return 3;
}

int accumulateStringHashFunction(char* str) 
{
	if (str == NULL) {
		return 0;
	}
	int sum = 0;
	for (int i = 0; str[i] != '\0' && str[i] != '\n'; i++) { 
		sum += str[i];
	}
	return sum;
}

int pow(int x, int y)
{
	int sum = 1;
	for(int i=0; i<y; i++)
	{
		sum*=x;
	}
	
	return sum;
}

int hash(char* str, HashTable* ht)
{
	int index;

	switch (ht->hashFunction)
	{
	case 1:
		index = constantStringHashFunction(str);
		break;
	case 2:
		index = accumulateStringHashFunction(str);
		break;
	case 3:
		index = improvedHashFunction(str);
		break;
	default:
		printf("Error: No such hash function - default index set to 0\n");
		freeHashTable(ht);
		return 0;
	}
	index = abs(index) % ht->tableSize;

	return index;
}


void printDecList(Declaration* head)
{
	while (head != NULL) {
		printf("\tscope id: %d\n\ttype: %s\n", head->scopeid, head->type);
		if (head->Args_Count)
		{
			printf("\targs count: %d\n", *head->Args_Count);
			if (*head->Args_Count > 0)
			{
				printf("\targs: ");
				printArgsList(head->args);
			}	
		}
		else
			printf("\n");
		
		head = head->next;
	}
}


void printArgsList(Args* head)
{
	while (head != NULL)
	{
		printf("%s ", head->type);
		head = head->next;
	}
	printf("\n");
}

HashTable* freeHashTable(HashTable* ht)
{
	if (ht != NULL && ht->hashtable != NULL)
		for (int i = 0; i < ht->tableSize; ++i)
			ht->hashtable[i].chain = FreeList(ht->hashtable[i].chain);
	if (ht != NULL)
	{
		if (ht->hashtable != NULL)
			free(ht->hashtable);
		ht->hashtable = NULL;
		free(ht);
		ht = NULL;
	}
	return ht;
}

LinkedList* FreeList(LinkedList* head)
{
	if (head == NULL)
		return head;
	if (head->next != NULL)
		FreeList(head->next);
	FreeDecList(head->Decl);
	free(head->name);
	free(head);
	head = NULL;

	return head;
}

void printIdList(VarInfo* idinfo)
{
	while(idinfo)
	{
		printf("%s  ", idinfo->name);
		idinfo = idinfo->next;
	}
}

VarInfo* mkVarInfo(char* name)
{
	VarInfo* idinfo = (VarInfo*)malloc(sizeof(VarInfo));
	if(!idinfo)
	{
		printf("Error! Out Of Memory\n");
		return NULL;
	}

	idinfo->name = strdup(name);
	idinfo->next = NULL;

	return idinfo;
}

node *mknode(node *left, node *right, char *token)
{
	node *newnode = (node*)malloc(sizeof(node));
	newnode->TAC = (T3AC*)malloc(sizeof(T3AC));
	char *newstr = (char*)malloc(sizeof(token) + 1);
	newnode->type = "";
	strcpy(newstr,token);
	newnode->left = left;
	newnode->right = right;
	newnode->token = newstr;
	
	return newnode;
}

void printTree(node *tree, int tab)
{
	int i;
	if(strcmp(tree->token, "") != 0)
	{
		if(tree->left || tree->right)
		{
			printf("\n");
			for(i = 0; i < tab; i++)
				printf(" ");
			printf("(");
		}
	}


	char* token = tree->token;
	printf("%s ",token);
	
	if(tree->left)
		printTree(tree->left, tab + 1);
	if(tree->right)
		printTree(tree->right, tab + 1);
	if(strcmp(tree->token,"") != 0){
		if(tree->right){
			for(i = 0; i < tab; i++)
				printf("");
			printf(")\n");
		}
	}
}

//--------------------------------------------------------PART 3------------------------------------------------------------

T3AC* mk3ac()
{
	T3AC* tac = (T3AC*)malloc(sizeof(T3AC));
	tac->code = NULL;
	tac->var = NULL;
	tac->falselab = NULL;
	tac->truelab = NULL;
	tac->count = 0;
	tac->begin = NULL;
	tac->after = NULL;
	return tac;
}



int T3ac(node* root)
{
	if(root == NULL)
	{
		return 1;
	}
	
	else
	{
		if(!strcmp(root->token,"") == 0)
		{
			//printf("Token is -------> %s\n" ,root->token);
		}	
	}
	
	if(strcmp(root->token,"") == 0)
	{
		if(root->left)
		{
			T3ac(root->left);
		}
	
		if(root->right)
		{
			T3ac(root->right);
		}
	}
	
	else if(strcmp(root->token,"CODE") == 0)
	{
		T3ac(root->left);
		
	}
	
	else if(strcmp(root->token,"FUNCTIONS") == 0)
	{
		T3ac(root->left);
		T3ac(root->right);
	}
	
	else if(strcmp(root->token,"FUNCTION") == 0)
	{	
		
		T3ac(root->left);
	}
	
	else if(strcmp(root->token,"VOID") == 0)
	{
		printf("%s:\n\t", root->left->left->left->token);
		printf("BeginFunc\n");
		T3ac(root->left->right->right->left);
		printf("\tEndFunc\n");
		TacCount = 0;
	}
	
	else if(strcmp(root->token,"FUNC") == 0)
	{
		printf("%s:\n\t", root->right->left->left->token);
		printf("BeginFunc\n");
		T3ac(root->right->right->right->left);
		printf("\tEndFunc\n");
		TacCount = 0;
	}
	
	else if(strcmp(root->token,"BLOCK") == 0)
	{
		T3ac(root->left);
	}
	
	else if(strcmp(root->token,"EQUAL") == 0)
	{
		T3ac(root->left);
	}
	
	else if(strcmp(root->token,"=") == 0)
	{
		T3ac(root->left);
		root->TAC->var = root->left->TAC->var;
		T3ac(root->right);
		root->TAC->var = root->right->TAC->var;
		gen(root);
	}
	
	else if(strcmp(root->token,"EQ_RIGHT") == 0)
	{
		T3ac(root->left);
		root->TAC->var = root->left->TAC->var;
		root->TAC->code = root->left->TAC->code;
	}
	
	else if(strcmp(root->token,"INT_DEC") == 0)
	{
		root->TAC->var = freshVar();
		char* temp = strdup(root->TAC->var);
		root->TAC->code = temp;
		printf("\t%s = %s\n" , root->TAC->var, root->left->token);
	}
	
	else if(strcmp(root->token,"INT_VALUE") == 0)
	{
		T3ac(root->left);
		root->TAC->var = root->left->TAC->var;
		root->TAC->code = root->left->TAC->code; 
	}
	
	else if(strcmp(root->token,"REAL_VALUE") == 0)
	{
		T3ac(root->left);
		root->TAC->var = root->left->TAC->var;
		root->TAC->code = root->left->TAC->code; 
	}
	
	else if(strcmp(root->token,"REAL_VAL") == 0)
	{
		root->TAC->var = freshVar();
		char* temp = strdup(root->TAC->var);
		root->TAC->code = temp;
		printf("\t%s = %s\n" , root->TAC->var, root->left->token);
	}
	
	else if(strcmp(root->token,"REAL_ID") == 0)
	{
		T3ac(root->left);
		root->TAC->var = root->left->TAC->var;
		root->TAC->code = root->left->TAC->code; 
	}
	
	else if(strcmp(root->token,"INT_ID") == 0)
	{
		T3ac(root->left);
		root->TAC->var = root->left->TAC->var;
		root->TAC->code = root->left->TAC->code; 
	}
	
	else if(strcmp(root->token,"()") == 0)
	{
		T3ac(root->left);
		root->TAC->var = root->left->TAC->var;
		root->TAC->code = root->left->TAC->code;
	}
	
	else if(strcmp(root->token,"VALUE") == 0)
	{
		T3ac(root->left);
		root->TAC->var = root->left->TAC->var;
		root->TAC->code = root->left->TAC->code; 
	}
	
	else if(strcmp(root->token,"+") == 0 || strcmp(root->token," * ") == 0 || strcmp(root->token,"-") == 0 || strcmp(root->token,"/") == 0 )
	{
		T3ac(root->left);
		T3ac(root->right);
		root->TAC->var = freshVar();
		//printf("%s\n", root->left->TAC->code);
		char* temp0 = strdup(root->left->TAC->code);
		
		strcat(temp0, root->right->TAC->code);
		root->TAC->code = strdup(temp0);
		printf("\t%s = %s\n",root->TAC->var,genValues(root));
	}
	
	else if(strcmp(root->token,"<") == 0 || strcmp(root->token,"<=") == 0 || strcmp(root->token,">") == 0 || strcmp(root->token,">=") == 0 || strcmp(root->token,"==") == 0 || strcmp(root->token,"!=") == 0)
	{
		T3ac(root->left);
		root->TAC->var = root->left->TAC->var;
		T3ac(root->right);
		root->TAC->var = root->right->TAC->var;
		root->TAC->var = genValues(root);
		char* x = freshVar();
		printf("\t%s = %s\n", x,root->left->TAC->var);
		root->TAC->code = root->left->TAC->var;	
	}
	
	else if(strcmp(root->token,"FOR") == 0)
	{
		T3ac(root->left->left->left);
		root->left->left->right->TAC->truelab = freshLabel();
		root->left->left->right->TAC->falselab = freshLabel();
		T3ac(root->left->left->right);
		root->TAC->after = freshLabel();
		printf("\tgoto %s\n", root->TAC->after);
		printf("    %s:",root->left->left->right->TAC->falselab);
		T3ac(root->right);
		T3ac(root->left->right);
		printf("\tgoto %s\n", root->left->left->right->TAC->truelab);
		printf("    %s:",root->TAC->after);
	}
	
	else if(strcmp(root->token,"CONDITION") == 0)
	{
		printf("    %s:if %s %s %s goto %s\n",root->TAC->truelab, root->left->left->token, root->right->left->token,root->right->right->left->token, root->TAC->falselab);
	}
	
	else if(strcmp(root->token,"IF_ELSE") == 0)
	{ 
		ElseFlag = 2;
		T3ac(root->left);
		root->right->TAC->after = freshLabel();
		printf("\tgoto %s\n", root->right->TAC->after);
		printf("    %s:",root->left->TAC->after);
		T3ac(root->right);
		printf("    %s:",root->right->TAC->after);
	}
	
	else if(strcmp(root->token,"ELSE") == 0)
	{
		
		T3ac(root->left);
	}

	else if(strcmp(root->token,"||") == 0)
	{
		root->TAC->begin = freshLabel();
		root->TAC->after = freshLabel();
		root->left->TAC->begin = strdup(root->TAC->begin);
		root->left->TAC->after = freshLabel();
		root->right->TAC->begin = strdup(root->TAC->begin);
		root->right->TAC->after = strdup(root->TAC->after);
		T3ac(root->left);
		T3ac(root->right);
		root->TAC->code = root->left->TAC->var;
		printf("\tif %s goto %s\n",root->right->left->TAC->code, root->left->TAC->after);
		printf("\tif %s goto %s\n",root->left->TAC->code , root->left->TAC->after);
		printf("\tgoto %s\n",root->TAC->falselab);
		printf("    %s:", root->left->TAC->after);
		root->TAC->code = "true";
		
	}
	
	else if(strcmp(root->token,"&&") == 0)
	{
		root->TAC->begin = freshLabel();
		root->TAC->after = freshLabel();
		root->left->TAC->begin = strdup(root->TAC->begin);
		root->left->TAC->after = freshLabel();
		root->right->TAC->begin = strdup(root->TAC->begin);
		root->right->TAC->after = strdup(root->TAC->after);
		T3ac(root->left);
		T3ac(root->right);
		root->TAC->code = root->left->TAC->var;
		printf("\tif %s goto %s\n",root->right->left->TAC->code, root->left->TAC->after);
		printf("\tif %s goto %s\n",root->left->TAC->code , root->left->TAC->after);
		printf("\tgoto %s\n",root->TAC->falselab);
		printf("    %s:", root->left->TAC->after);
		root->TAC->code = "true";
	}
	
	else if(strcmp(root->token,"COMP") == 0)
	{
		T3ac(root->left);
		root->TAC->var = root->left->TAC->var;
		root->TAC->code = root->left->TAC->code;
	}
		
	else if(strcmp(root->token,"IF") == 0)
	{
		root->TAC->begin = freshLabel();
		root->TAC->after = freshLabel();
		root->left->TAC->falselab = root->TAC->after;
		T3ac(root->left);
		
		printf("\tif %s goto %s\n\tgoto %s\n",root->left->TAC->code, root->TAC->begin,root->TAC->after);
		printf("    %s:",root->TAC->begin);
		T3ac(root->right);
		if(ElseFlag != 2)
		{
			printf("    %s:",root->TAC->after);
		}
		
	}
	
	else if(strcmp(root->token,"VALS:") == 0)
	{
		T3ac(root->left);
		root->TAC->var = root->left->TAC->var;
	}
		
	else if(strcmp(root->token,"FUNC_VALUES") == 0)
	{
		T3ac(root->left);
		root->TAC->var = root->left->TAC->var;
	}
	
	else if(strcmp(root->token,"VALS_FUNC") == 0)
	{
		T3ac(root->left);
		root->TAC->var = root->left->TAC->var;
		printf("\tPushParam %s\n", root->TAC->var);
		T3ac(root->right);
	}
	
	else if(strcmp(root->token,"VAL_FUNC") == 0)
	{
		T3ac(root->left);
		root->TAC->var = root->left->TAC->var;
		printf("\tPushParam %s\n", root->TAC->var);
	}
	
	else if(strcmp(root->token,"ID_=_CF") == 0)
	{
		T3ac(root->left);
		root->TAC->var = root->left->TAC->var;
	}
	
	else if(strcmp(root->token,"CALL_FUNC") == 0)
	{
		if(root->right)
		{
			T3ac(root->right);
			root->TAC->var = freshVar();
			printf("\t%s = LCall%s\n", root->TAC->var,root->left->left->token);
			node* Val_Root = root->right->left->left;
			while(Val_Root)
			{
				printf("\tPopParam %d\n",ReturnNumBytes(Val_Root->left->token));
				Val_Root = Val_Root->right;
			}
		}
	}
		
	else if(strcmp(root->token,"STAT") == 0)
	{
		T3ac(root->left);
		root->TAC->var = root->left->left->TAC->var;
		root->TAC->code = root->left->TAC->code;
	}
	
	else if(strcmp(root->token,"WHILE") == 0)
	{
		root->TAC->begin = freshLabel();
		char* startWhile = freshLabel();
		root->TAC->after = freshLabel();
		T3ac(root->left);
		printf("    %s:if %s goto %s\n\tgoto %s\n",root->TAC->begin,root->left->TAC->var, startWhile,root->TAC->after);
		printf("    %s:",startWhile);
		T3ac(root->right);
		printf("\tgoto %s\n",root->TAC->begin);
		printf("    %s:", root->TAC->after);
	}
	
	else if(strcmp(root->token,"EXP") == 0)
	{
		T3ac(root->left);
		root->TAC->var = root->left->TAC->var;
		root->TAC->code = root->left->TAC->code;
	}
	
	else if(strcmp(root->token,"ID") == 0)
	{
		root->TAC->var = root->left->token;
		root->TAC->code = "";
	}
}	

int ReturnNumBytes(char* type)
{
	if(strcmp(type,"CHAR_VALUE") == 0)
	{
		return 1;
	}
	
	if(strcmp(type,"REAL_VALUE") == 0)
	{
		return 8;
	}
	
	if(strcmp(type,"INT_VALUE") == 0)
	{
		return 4;
	}
}

char* freshVar()
{
	char* number = (char*)malloc(sizeof(char) * 9); 
	sprintf(number, "t_%d", TacCount++);
	return number;
}


char* genValues(node* root)
{
	char* temp = root->left->TAC->var;
	strcat(temp," "); 
	strcat(temp,root->token);
	strcat(temp," ");
	strcat(temp, root->right->TAC->var);
	return temp;
}

char* freshLabel()
{
	char* number = (char*)malloc(sizeof(char) * 9);
	sprintf(number, "L%d", ++LabelCount);
	return number;
}

void gen(node* root)
{
	printf("\t%s %s %s\n",root->left->TAC->var,root->token, root->right->TAC->var);
}

void printTypeTree(node *tree, int tab)
{
	int i;
	if(tree->type)
	{
		if(strcmp(tree->type, "") != 0)
		{
			if(tree->left || tree->right)
			{
				printf("\n");
				for(i = 0; i < tab; i++)
					printf(" ");
				printf("(");
			}
		}
	}

	
	
	
	char* token = tree->token;

	if(tree->left)
		printTypeTree(tree->left, tab + 1);
	if(tree->right)
		printTypeTree(tree->right, tab + 1);
	if(tree->type)
	{
		if(strcmp(tree->type,"") != 0)
		{
			if(tree->right){
				for(i = 0; i < tab; i++)
					printf("");
				printf(")\n");
			}
		}
	}

}

void yyerror(char* s)
{
	printf("%s - %s in line: %d\n",s,yytext,count);
}
