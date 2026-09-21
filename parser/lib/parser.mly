%{
	open Ast
%}

%token CONSTRAINTS
%token DEFINITIONS
%token INPUTS
%token OUTPUTS
%token PROOFS

%token INIT
%token NEXT
%token TRUE

%token ASSIGNMENT
%token COLON
%token COMMA
%token EOF
%token EQUIVALENCE
%token IMPLICATION
%token NOT
%token PARENTHESIS_LEFT
%token PARENTHESIS_RIGHT
%token SEMICOLON

%token <string> VAR

%start main
%type <ast option> main
%%

main:
	s=sections EOF	{ Some s }
	;

sections:
	INPUTS COLON i=list(inputs)
	DEFINITIONS COLON d=list(definitions)
	OUTPUTS COLON o=list(outputs)
	CONSTRAINTS COLON c=list(constraints)
	PROOFS COLON p=list(proofs)
	{ {constraints = c; definitions = d; inputs = i; outputs = o; proofs = p} }
	;

constraints:
	| e=loc SEMICOLON											{ e }
	| INIT PARENTHESIS_LEFT e=loc PARENTHESIS_RIGHT SEMICOLON	{ Init(e) }
	;

definitions:
	| v=VAR ASSIGNMENT e1=id EQUIVALENCE e2=id SEMICOLON						{ (v, Equivalence(e1, e2)) }
	| v=VAR ASSIGNMENT e1=lit IMPLICATION e2=lit SEMICOLON						{ (v, Implication(e1, e2)) }
	| v=VAR ASSIGNMENT e1=loc COMMA e2=loc SEMICOLON							{ (v, Memory(e1, e2)) }
	| v=VAR ASSIGNMENT NEXT PARENTHESIS_LEFT e=id PARENTHESIS_RIGHT SEMICOLON	{ (v, Next(e)) }
	;

inputs:
	| v=VAR SEMICOLON	{ v }
	;

outputs:
	| e=loc SEMICOLON	{ e }
	;

proofs:
	| e=loc SEMICOLON	{ e }
	;

lit:
	| e=id		{ e }
	| NOT e=id	{ Not(e) }
	;

loc:
	| e=atom		{ e }
	| NOT e=atom	{ Not(e) }
	;

atom:
	| e=id	{ e }
	| TRUE	{ True }
	;

id:
	| e=VAR	{ Variable(e) }
	;