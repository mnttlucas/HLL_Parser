%{
	open Ast
%}

%token CONSTANTS
%token CONSTRAINTS
%token DECLARATIONS
%token DEFINITIONS
%token INPUTS
%token NAMESPACES
%token OBLIGATIONS
%token OUTPUTS
%token PROOF
%token TYPES

%token BOOL
%token ENUM
%token INT
%token SIGNED
%token STRUCT
%token TUPLE
%token UNSIGNED

%token ALL
%token BIN2S
%token BIN2U
%token CAST
%token CONJ
%token DABS
%token DAND
%token DMAX
%token DMIN
%token DNOT
%token DOR
%token DOT
%token DXOR
%token DISJ
%token ELIF
%token ELSE
%token FALSE
%token IF
%token INIT
%token ITEMS
%token LAMBDA
%token NEXT
%token PCEQ
%token PCGT
%token PCLT
%token PRE
%token PROD
%token S2BIN
%token SELECT
%token SOME
%token SORT
%token SUM
%token THEN
%token TRUE
%token U2BIN
%token WITH

%token AND
%token ARROW
%token ASSIGNMENT
%token BRACEL
%token BRACER
%token BRACKL
%token BRACKR
%token CEIL
%token CIRCUMFLEX
%token COLON
%token COMMA
%token DIFFERENT
%token DOUBLE_COLON
%token EOF
%token EQUAL
%token EQUIVALENCE
%token FLOOR
%token GREATER
%token GREATER_EQUAL
%token IMPLICATION
%token LESS
%token LESS_EQUAL
%token MINUS
%token NOT
%token OR
%token PARL
%token PARR
%token PERCENT
%token PLUS
%token SEMICOLON
%token SHIFT_LEFT
%token SHIFT_RIGHT
%token SLASH
%token STAR
%token UNDERSCORE
%token VERTICAL
%token XOR

%token <string> ID
%token <string> LIT

%nonassoc p1

%right IMPLICATION

(* %nonassoc p2 *)

%left EQUIVALENCE XOR
%left OR
%left AND
%left GREATER GREATER_EQUAL LESS LESS_EQUAL EQUAL DIFFERENT COLON
%left SHIFT_LEFT SHIFT_RIGHT
%left PLUS MINUS
%left STAR SLASH CEIL FLOOR PERCENT
%right CIRCUMFLEX 

%nonassoc p3
(* %nonassoc p4 *)

%start hll
%type <sections list option> hll
%%

hll:
	s=list(sections) EOF	{ Some s }
	;

sections:
	| CONSTANTS COLON c=list(constants)			{ SConstants(c) }
	| CONSTRAINTS COLON c=list(constraints)		{ SConstraints(c) }
	| DECLARATIONS COLON d=list(declarations)	{ SDeclarations(d) }
	| DEFINITIONS COLON d=list(definitions)		{ SDefinitions(d) }
	| INPUTS COLON i=list(inputs)				{ SInputs(i) }
	| NAMESPACES COLON n=list(namespaces)		{ SNamespaces(n) }
	| OUTPUTS COLON o=list(outputs)				{ SOutputs(o) }
	| PROOF OBLIGATIONS COLON p=list(proofs)	{ SProofs(p) }
	| TYPES COLON t=list(types)					{ STypes(t) }
	;

outputs:
	| e=expr SEMICOLON	{ e }
	;

proofs:
	| e=expr SEMICOLON	{ e }
	;

namespaces:
	| i=id BRACEL sl=list(sections) BRACER	{ (i, sl) }
	;

constants:
	| BOOL i=id ASSIGNMENT e=expr SEMICOLON	{ CtBool(i, e) }
	| INT i=id ASSIGNMENT e=expr SEMICOLON	{ CtInt(i, e) }
	;

types:
	| t=typedef dl=declarator_list SEMICOLON			{ TType(t, dl) }
	| ENUM BRACEL il=id_list BRACER i=id SEMICOLON		{ TEnum(il, i) }
	| SORT i=id SEMICOLON								{ TSort(i) }
	| SORT pl=path_id_list LESS i=id SEMICOLON			{ TSortContrib(ScPaths(pl), i) }
	| SORT BRACEL il=id_list BRACER LESS i=id SEMICOLON	{ TSortContrib(ScIds(il), i) }
	;

declarator:
	| i=id sl=list(declarator_suffix)	{ DDeclarator(i, sl) }
	;

declarator_suffix:
	| BRACKL el=expr_list BRACKR	{ DsArray(el) }
	| PARL el=type_list PARR		{ DsFunction(el) }
	;

declarator_list: 
	| dl=separated_nonempty_list(COMMA, declarator)	{ dl }
	;

typedef:
	| BOOL																		{ TBool }
	| i=integer																	{ TInt(i) }
	| TUPLE BRACEL tl=type_list BRACER											{ TTuple(tl) }
	| STRUCT BRACEL ml=member_list BRACER										{ TStructure(ml) }
	| t=typedef CIRCUMFLEX PARL el=expr_list PARR								{ TArray(t, el) }
	| PARL tl=separated_nonempty_list(STAR, typedef) IMPLICATION t=typedef PARR	{ TFunction(tl, t) }
	| p=path_id																	{ TPath(p) }
	;

integer:
	| INT			{ IInt }
	| INT s=sign	{ s }
	| INT r=range	{ IRanged(r) }
	;

sign:
	| SIGNED i=id_or_int	{ ISigned(i) }
	| UNSIGNED i=id_or_int	{ IUnsigned(i) }
	;

id_or_int:
	| i=id			{ OId(i) }
	| i=int_literal	{ OInt(i) }
	;

range:
	| BRACKL e1=expr COMMA e2=expr BRACKR	{ (e1, e2) }
	;

type_list:
	| tl=separated_nonempty_list(COMMA, typedef)	{ tl }
	;

member:
	| i=id COLON t=typedef	{ (i, t) }
	;

member_list:
	| ml=separated_nonempty_list(COMMA, member)	{ ml }
	;

inputs:
	| il=input_declarator_list SEMICOLON			{ IUntyped(il) }
	| t=typedef il=input_declarator_list SEMICOLON	{ ITyped(t, il) }
	;

input_declarator:
	| d=declarator					{ IdName(d) }
	| INIT PARL d=declarator PARR	{ IdInit(d) }
	;

input_declarator_list:
	| il=separated_nonempty_list(COMMA, input_declarator)	{ il }
	;

declarations:
	| dl=declarator_list SEMICOLON				{ DcUntyped(dl) }
	| t=typedef dl=declarator_list SEMICOLON	{ DcTyped(t, dl) }
	;

constraints:
	| e=expr SEMICOLON					{ CrCons(e) }
	| INIT PARL e=expr PARR SEMICOLON	{ CrInit(e) }
	;

definitions:
	| l=lhs ASSIGNMENT r=rhs SEMICOLON					{ DfAssign(l, r) }
	| INIT PARL l=lhs PARR ASSIGNMENT r=rhs SEMICOLON	{ DfInit(l, r) }
	| NEXT PARL l=lhs PARR ASSIGNMENT r=rhs SEMICOLON	{ DfNext(l, r) }
	| l=lhs ASSIGNMENT r1=rhs COMMA r2=rhs SEMICOLON	{ DfMemory(l, r1, r2) }
	;

lhs:
	| u=unfolding							{ LUnfolding(u) }
	| i=id fl=nonempty_list(formal_param)	{ LParams(i, fl) }
	;

rhs:
	| e=expr		{ RExpr(e) }
	| c=collection	{ c }
	;

collection:
	| BRACEL rl=separated_nonempty_list(COMMA, rhs) BRACER	{ RCollection(rl) }
	;

unfold_lhs:
	| i=id			{ UlId(i) }
	| UNDERSCORE	{ UlUnderscore }
	;

unfolding:
	| ul=separated_nonempty_list(COMMA, unfold_lhs)	{ ul }
	;

formal_param:
	| BRACKL il=id_list BRACKR	{ FpArray(il) }
	| PARL il=id_list PARR		{ FpFunction(il) }
	;

accessor:
	| DOT i=id						{ AStruct(i) }
	| DOT i=int_literal				{ ATuple(i) }
	| BRACKL el=expr_list BRACKR	{ AArray(el) }
	| PARL el=expr_list PARR		{ AFunction(el) }
	;

expr:
	| IF e1=expr THEN e2=expr el=elif_list ELSE e3=expr	%prec p1													{ EIfThenElse(e1, e2, el, e3) }
	| LAMBDA nl=nonempty_list(declarator_suffix) COLON fl=nonempty_list(formal_param) ASSIGNMENT e=expr %prec p1	{ ELambda(nl, fl, e) }
	| e1=expr AND e2=expr																							{ EBinOp(e1, BAnd, e2) }
	| e1=expr CEIL e2=expr																							{ EBinOp(e1, BCeil, e2) }
	| e1=expr CIRCUMFLEX e2=expr																					{ EBinOp(e1, BPower, e2) }
	| e1=expr DIFFERENT e2=expr																						{ EBinOp(e1, BDifferent, e2) }
	| e1=expr EQUAL e2=expr																							{ EBinOp(e1, BEqual, e2) }
	| e1=expr EQUIVALENCE e2=expr																					{ EBinOp(e1, BEquivalence, e2) }
	| e1=expr FLOOR e2=expr																							{ EBinOp(e1, BFloor, e2) }
	| e1=expr GREATER e2=expr																						{ EBinOp(e1, BGreater, e2) }
	| e1=expr GREATER_EQUAL e2=expr																					{ EBinOp(e1, BGreater_Eq, e2) }
	| e1=expr IMPLICATION e2=expr																					{ EBinOp(e1, BImplication, e2) }
	| e1=expr LESS e2=expr																							{ EBinOp(e1, BLess, e2) }
	| e1=expr LESS_EQUAL e2=expr																					{ EBinOp(e1, BLess_Eq, e2) }
	| e1=expr MINUS e2=expr																							{ EBinOp(e1, BDifference, e2) }
	| e1=expr OR e2=expr																							{ EBinOp(e1, BOr, e2) }
	| e1=expr PERCENT e2=expr																						{ EBinOp(e1, BModulo, e2) }
	| e1=expr PLUS e2=expr																							{ EBinOp(e1, BSum, e2) }
	| e1=expr SHIFT_LEFT e2=expr																					{ EBinOp(e1, BShift_Left, e2) }
	| e1=expr SHIFT_RIGHT e2=expr																					{ EBinOp(e1, BShift_Right, e2) }
	| e1=expr SLASH e2=expr																							{ EBinOp(e1, BDivision, e2) }
	| e1=expr STAR e2=expr																							{ EBinOp(e1, BProduct, e2) }
	| e1=expr XOR e2=expr																							{ EBinOp(e1, BXor, e2) }
	| e=expr COLON d=domain (* %prec p2 *)																			{ EMember(e, d) }
	| u=unop e=expr	%prec p3																						{ EUnOp(u, e) }
	| ce=closed_expr al=list(accessor) (* %prec p4 *)																{ EClosed(ce, al) }
	;

elif:
	| ELIF e1=expr THEN e2=expr	{ (e1, e2) }
	;

elif_list:
	| el=list(elif)	{ el }
	;

domain:
	| r=range	{ DRange(r) }
	| p=path_id	{ DPath(p) }
	| BOOL		{ DBool }
	| INT		{ DInt }
	;

closed_expr:
	| b=bool_literal													{ b }
	| i=int_literal														{ CeIntLit(i) }
	| p=path_id															{ CePath(p) }
	| NEXT PARL e=expr PARR												{ CeNext(e) }
	| PRE PARL e=expr PARR												{ CeUntypedPre1(e) }
	| PRE LESS t=typedef GREATER PARL e=expr PARR						{ CeTypedPre1(t, e) }
	| PRE PARL e1=expr COMMA e2=expr PARR								{ CeUntypedPre2(e1, e2) }
	| PRE LESS t=typedef GREATER PARL e1=expr COMMA e2=expr PARR		{ CeTypedPre2(t, e1, e2) }	
	| f=fop PARL el=expr_list PARR										{ CeFop(f, el) }
	| CAST LESS t=typedef GREATER PARL e=expr PARR						{ CeCast(t, e) }
	| PARL e=expr WITH al=nonempty_list(accessor) ASSIGNMENT r=rhs PARR	{ CeWith(e, al, r) }
	| PARL el=expr_list cl=nonempty_list(case_item) PARR				{ CeSwitch(el, cl) }
	| q=quantif_expr													{ CeQuantif(q) }
	| PARL e=expr PARR													{ CeExpr(e) }
	;

bool_literal:
	| TRUE	{ CeTrue }
	| FALSE	{ CeFalse }
	;

int_literal:
	| i=LIT	{ String.concat "" (String.split_on_char '_' i) }
	;

case_item:
	| VERTICAL pl=pattern_list ARROW e=expr	{ (pl, e) }
	;

pattern:
	| e=expr				{ PExpr(e) }
	| p=path_id i=id		{ PPath(p, i) }
	| p=path_id UNDERSCORE	{ PPathUnderscore(p) }
	| UNDERSCORE			{ PUnderscore }
	;

pattern_list:
	| pl=separated_nonempty_list(COMMA, pattern)	{ pl }	
	;

quantif_expr:
	| q=quantifier ql=quantif_var_list qe=quantif_expr				{ QeQuantif(q, ql, qe) }
	| q=quantifier ql=quantif_var_list PARL e=expr PARR				{ QeExpr(q, ql, e) }
	| q=quantifier ql=quantif_var_list PARL e=expr COMMA r=rhs PARR	{ QeExprRhs(q, ql, e, r) }
	;

quantif_var:
	| i=id COLON d=domain				{ QvDomain(i, d) }
	| i=id COLON ITEMS PARL e=expr PARR	{ QvItems(i, e) }
	;

quantif_var_list:
	| ql=separated_nonempty_list(COMMA, quantif_var)	{ ql }
	;

unop:
	| MINUS	{ UMinus }
	| NOT	{ UNot }
	;

fop:
	| BIN2S	{ Fb2s }
	| BIN2U	{ Fb2u }
	| DABS	{ FDabs }
	| DAND	{ FDand }
	| DOR	{ FDor }
	| DMAX 	{ FDmax }
	| DMIN	{ FDmin }
	| DNOT	{ FDnot }
	| DXOR	{ FDxor }
	| PCEQ	{ FPceq }
	| PCGT	{ FPcgt }
	| PCLT	{ FPclt }
	| S2BIN	{ Fs2b }
	| U2BIN	{ Fu2b }
	;

expr_list:
	| el=separated_nonempty_list(COMMA, expr)	{ el }
	;

id_list:
	| il=separated_nonempty_list(COMMA, id)	{ il }
	;

path_id_list:
	| pl=separated_nonempty_list(COMMA, path_id)	{ pl }
	;

path_id:
	| pl=path_list				{ PRelative(pl) }
	| DOUBLE_COLON pl=path_list	{ PAbsolute(pl) }
	;

path_list:
	| pl=separated_nonempty_list(DOUBLE_COLON, id)	{ pl }
	;

id:
	| i=ID	{ i }
	;

quantifier:
	| ALL		{ QAll }
	| CONJ		{ QConj }
	| DISJ		{ QDisj }
	| DMAX		{ QDmax }
	| DMIN		{ QDmin }
	| PROD		{ QProd }
	| SELECT	{ QSelect }
	| SOME		{ QSome }
	| SUM		{ QSum }
	;