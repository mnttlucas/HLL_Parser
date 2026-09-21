open List
open Printf
open String

(** [Ast] module defines the different types of HLL expressions,
	as well as the pretty printing functions used when the -print
	option is enabled. Types are based on HLL-LDD-pr4.0rc1.pdf'
	syntax.*)

type binop =
	| BAnd
	| BCeil
	| BDifference
	| BDifferent
	| BDivision
	| BEqual
	| BEquivalence
	| BFloor
	| BGreater
	| BGreater_Eq
	| BImplication
	| BLess
	| BLess_Eq
	| BModulo
	| BOr
	| BPower
	| BProduct
	| BShift_Left
	| BShift_Right
	| BSum
	| BXor

type unop =
	| UMinus
	| UNot

type fop =
	| Fb2s
	| Fb2u
	| FDabs
	| FDand
	| FDor
	| FDmax
	| FDmin
	| FDnot
	| FDxor
	| FPceq
	| FPcgt
	| FPclt
	| Fs2b
	| Fu2b

type quantifier =
	| QAll
	| QConj
	| QDisj
	| QDmax
	| QDmin
	| QProd
	| QSelect
	| QSome
	| QSum

type id_or_int =
	| OId	of string
	| OInt	of string

type path =
	| PAbsolute	of string list
	| PRelative	of string list

type sort_contrib =
	| ScIds		of string list
	| ScPaths	of path list

type formal_param =
	| FpArray		of string list
	| FpFunction	of string list

type typedef =
	| TBool
	| TInt			of integer
	| TTuple		of typedef list
	| TStructure	of (string * typedef) list
	| TArray		of typedef * expr list
	| TFunction		of typedef list * typedef
	| TPath			of path
	
and integer =
	| IInt
	| ISigned	of id_or_int
	| IUnsigned	of id_or_int
	| IRanged	of range

and range = expr * expr

and domain =
	| DRange	of range
	| DPath		of path
	| DBool
	| DInt

and accessor = 
	| AStruct	of string
	| ATuple	of string
	| AArray	of expr list
	| AFunction	of expr list

and pattern =
	| PExpr				of expr
	| PPath				of path * string
	| PPathUnderscore	of path
	| PUnderscore

and case = pattern list * expr

and quantif_var =
	| QvDomain	of string * domain
	| QvItems	of string * expr

and quantif_expr =
	| QeQuantif	of quantifier * quantif_var list * quantif_expr
	| QeExpr	of quantifier * quantif_var list * expr
	| QeExprRhs	of quantifier * quantif_var list * expr * rhs

and declarator_suffix =
	| DsArray	of expr list
	| DsFunction	of typedef list

and rhs =
	| RExpr			of expr
	| RCollection	of rhs list

and closed_expr =
	| CeTrue
	| CeFalse
	| CeIntLit		of string
	| CePath		of path
	| CeExpr		of expr
	| CeNext		of expr
	| CeFop			of fop * expr list
	| CeCast		of typedef * expr
	| CeWith		of expr * accessor list * rhs
	| CeTypedPre1	of typedef * expr
	| CeTypedPre2	of typedef * expr * expr
	| CeUntypedPre1	of expr
	| CeUntypedPre2	of expr * expr
	| CeSwitch		of expr list * case list
	| CeQuantif		of quantif_expr

and expr =
	| EBinOp		of expr * binop * expr
	| EUnOp			of unop * expr
	| EClosed		of closed_expr * accessor list
	| EMember		of expr * domain
	| EIfThenElse	of expr * expr * (expr * expr) list * expr
	| ELambda		of declarator_suffix list * formal_param list * expr

type declarator = 
	| DDeclarator of string * declarator_suffix list

type input_declarator =
	| IdName	of declarator
	| IdInit	of declarator

type unfold_lhs =
	| UlId			of string
	| UlUnderscore

type lhs = 
	| LUnfolding	of unfold_lhs list
	| LParams		of string * formal_param list

type constants =
	| CtBool	of string * expr
	| CtInt		of string * expr

type constraints =
	| CrCons	of expr
	| CrInit	of expr

type declarations =
	| DcTyped	of typedef * declarator list
	| DcUntyped	of declarator list

type definitions =
	| DfAssign	of lhs * rhs
	| DfInit	of lhs * rhs
	| DfNext	of lhs * rhs
	| DfMemory	of lhs * rhs * rhs

type inputs =
	| ITyped		of typedef * input_declarator list
	| IUntyped		of input_declarator list

type types =
	| TType			of typedef * declarator list
	| TEnum			of string list * string
	| TSort			of string
	| TSortContrib	of sort_contrib * string

type sections =
	| SConstants	of constants list
	| SConstraints	of constraints list
	| SDeclarations	of declarations list
	| SDefinitions	of definitions list
	| SInputs		of inputs list
	| SNamespaces	of (string * sections list) list
	| SOutputs		of expr list
	| SProofs		of expr list
	| STypes		of types list

let binop_string = function
	| BAnd			-> "And"
	| BCeil			-> "Ceil"
	| BDifference	-> "Difference"
	| BDifferent	-> "Different"
	| BDivision		-> "Division"
	| BEqual		-> "Equal"
	| BEquivalence	-> "Equivalence"
	| BFloor		-> "Floor"
	| BGreater		-> "Greater"
	| BGreater_Eq	-> "Greater_Equal"
	| BImplication	-> "Implication"
	| BLess			-> "Less"
	| BLess_Eq		-> "Less_Equal"
	| BModulo		-> "Modulo"
	| BOr			-> "Or"
	| BPower		-> "Power"
	| BProduct		-> "Product"
	| BShift_Left	-> "Shift_Left"
	| BShift_Right	-> "Shift_Right"
	| BSum			-> "Sum"
	| BXor			-> "Xor"

let unop_string = function
	| UMinus	-> "Minus"
	| UNot		-> "Not"

let fop_string = function
	| Fb2s	-> "b2s"
	| Fb2u	-> "b2u"
	| FDabs	-> "Abs"
	| FDand	-> "And"
	| FDor	-> "Or"
	| FDmax	-> "Max"
	| FDmin	-> "Min"
	| FDnot	-> "Not"
	| FDxor	-> "Xor"
	| FPceq	-> "PCequal"
	| FPcgt	-> "PCgreater"
	| FPclt	-> "PCless"
	| Fs2b	-> "s2b"
	| Fu2b	-> "u2b"

let quantifier_string = function
	| QAll		-> "All"
	| QConj		-> "Conj"
	| QDisj		-> "Disj"
	| QDmax		-> "Max"
	| QDmin		-> "Min"
	| QProd		-> "Prod"
	| QSelect	-> "Select"
	| QSome		-> "Some"
	| QSum		-> "Sum"

let id_or_int_string = function
	| OId(s)	-> s
	| OInt(s)	-> s

let path_string = function
	| PAbsolute(sl)	-> "/" ^ concat "/" sl
	| PRelative(sl)	-> concat "/" sl

let sort_contrib_string = function
	| ScIds(sl)		->
		(match sl with
			| [s]	-> s
			| _		-> "[" ^ concat "; " sl ^ "]")
	| ScPaths(pl)	->
		(match pl with
			| [p]	-> path_string p
			| _		-> "[" ^ concat "; " (List.map (fun p -> path_string p) pl) ^ "]")

let formal_param_string = function
	| FpArray(sl)		-> "ArrayParam([" ^ concat "; " sl ^ "])"
	| FpFunction(sl)	-> "FunctionParam([" ^ concat "; " sl ^ "])"

let rec typedef_string = function
	| TBool				-> "Bool"
	| TInt(i)			-> sprintf "%s" (integer_string i)
	| TTuple(tl)		-> "Tuple([" ^ concat "; " (List.map (fun t -> typedef_string t) tl) ^ "])"
	| TStructure(t)		-> "Structure([" ^ concat "; " (List.map (fun t -> fst t ^ ":" ^ typedef_string (snd t)) t) ^ "])"
	| TArray(t, el)		-> sprintf "Array(%s: %s)" (typedef_string t) ("[" ^ concat "; " (List.map (fun e -> expr_string e) el) ^ "]")
	| TPath(p)			-> path_string p
	| TFunction(tl, t)	->
		(match tl with
			| [l] -> sprintf "Function(%s -> %s)" (typedef_string l) (typedef_string t)
			| _ -> sprintf "Function(%s -> %s)" ("(" ^ concat "; " (List.map (fun t -> typedef_string t) tl) ^ ")") (typedef_string t))

and integer_string = function
	| IInt			-> "Int"
	| ISigned(i)	-> sprintf "Signed(%s)" (id_or_int_string i)
	| IUnsigned(i)	-> sprintf "Unsigned(%s)" (id_or_int_string i)
	| IRanged(r)	-> sprintf "Range(%s)" (range_string r)

and range_string (e1, e2) = sprintf "%s, %s" (expr_string e1) (expr_string e2)

and domain_string = function
	| DRange(r)	-> range_string r
	| DPath(p)	-> path_string p
	| DInt		-> "Int"
	| DBool		-> "Bool"

and accessor_string = function
	| AStruct(s)	-> s
	| ATuple(i)		-> i
	| AArray(el)	-> "[" ^ concat "; " (List.map (fun e -> expr_string e) el) ^ "]"
	| AFunction(el)	-> "(" ^ concat "; " (List.map (fun e -> expr_string e) el) ^ ")"

and pattern_string = function
	| PExpr(e)				-> expr_string e
	| PPath(p, s)			-> "(" ^ path_string p ^ ", " ^ s ^ ")"
	| PPathUnderscore(p)	-> "(" ^ path_string p ^ ", _)"
	| PUnderscore			-> "_"

and case_string (pl, e) = "[" ^ concat "; " (List.map (fun p -> pattern_string p) pl) ^ "] -> " ^ expr_string e

and quantif_var_string = function
	| QvDomain(s, d)	-> s ^ ":" ^ domain_string d
	| QvItems(s, e)		-> sprintf "Items(%s, %s)" s (expr_string e)

and quantif_expr_string = function
	| QeQuantif(q, ql, qe)	-> sprintf "Quantif(%s: %s %s)" (quantifier_string q) ("[" ^ concat "; " (List.map (fun q -> quantif_var_string q) ql) ^ "]") (quantif_expr_string qe)
	| QeExpr(q, ql, e)		-> sprintf "QeExpr(%s: %s %s)" (quantifier_string q) ("[" ^ concat "; " (List.map (fun q -> quantif_var_string q) ql) ^ "]") (expr_string e)
	| QeExprRhs(q, ql, e, r)	-> sprintf "QeExprRhs(%s: %s %s %s)" (quantifier_string q) ("[" ^ concat "; " (List.map (fun q -> quantif_var_string q) ql) ^ "]") (expr_string e) (rhs_string r)
	
and declarator_suffix_string = function
	| DsArray(el)	-> concat "; " (List.map (fun e -> expr_string e) el)
	| DsFunction(tl)	-> concat "; " (List.map (fun t -> typedef_string t) tl)

and rhs_string = function
	| RExpr(e)			-> expr_string e
	| RCollection(rl)	-> "[" ^ concat "; " (List.map (fun r -> rhs_string r) rl) ^ "]"

and closed_expr_string = function
	| CeTrue					-> "True"
	| CeFalse					-> "False"
	| CeIntLit(i)				-> i
	| CePath(p)					-> sprintf "%s" (path_string p)
	| CeExpr(e)					-> sprintf "%s" (expr_string e)
	| CeNext(e)					-> sprintf "Next(%s)" (expr_string e)
	| CeFop(f, el)				-> sprintf "%s(%s)" (fop_string f) ("[" ^ concat "; " (List.map (fun e -> expr_string e) el) ^ "]")
	| CeCast(t, e)				-> sprintf "Cast(<%s> %s)" (typedef_string t) (expr_string e)
	| CeWith(e, al, r)			-> sprintf "With(%s(%s) : %s)" (expr_string e) ("[" ^ concat "; " (List.map (fun a -> accessor_string a) al) ^ "]") (rhs_string r)
	| CeTypedPre1(t, e)			-> sprintf "Pre(<%s> %s)" (typedef_string t) (expr_string e)
	| CeTypedPre2(t, e1, e2)	-> sprintf "Pre(<%s> %s, %s)" (typedef_string t) (expr_string e1) (expr_string e2)
	| CeUntypedPre1(e)			-> sprintf "Pre(%s)" (expr_string e)
	| CeUntypedPre2(e1, e2)		-> sprintf "Pre(%s, %s)" (expr_string e1) (expr_string e2)
	| CeSwitch(el, cl)			-> sprintf "Switch(%s : %s)" ("[" ^ concat "; " (List.map (fun e -> expr_string e) el) ^ "]") ("[" ^ concat "; " (List.map (fun c -> case_string c) cl) ^ "]")
	| CeQuantif(q)				-> sprintf "%s" (quantif_expr_string q)

and expr_string = function
	| EBinOp(e1, b, e2)				-> sprintf "%s(%s, %s)" (binop_string b) (expr_string e1) (expr_string e2)
	| EUnOp(u, e)					-> sprintf "%s(%s)" (unop_string u) (expr_string e)
	| EClosed(ce, al)				-> 
		(match al with
			| []	-> closed_expr_string ce
			| _		-> sprintf "%s %s" (closed_expr_string ce) ("[" ^ concat "; " (List.map (fun a -> accessor_string a) al) ^ "]"))
	| EMember(e, d)					-> sprintf "Domain(%s, %s)" (expr_string e) (domain_string d)
	| EIfThenElse(e1, e2, el, e3)	->
		(match el with
			| []	-> sprintf "IfThenElse(%s, %s, %s)" (expr_string e1) (expr_string e2) (expr_string e3)
			| _		-> sprintf "IfThenElse(%s, %s, %s, %s)" (expr_string e1) (expr_string e2) ("[" ^ concat "; " (List.map (fun t -> expr_string (fst t) ^ ":" ^ expr_string (snd t)) el) ^ "]") (expr_string e3))
	| ELambda(nl, pl, e)			-> sprintf "Lambda(%s, %s, %s)" ("[" ^ concat "; " (List.map (fun n -> declarator_suffix_string n) nl) ^ "]") ("[" ^ concat "; " (List.map (fun p -> formal_param_string p) pl) ^ "]") (expr_string e)

let declarator_string = function
	| DDeclarator(s, nl) ->
	(match nl with
		| []	-> s
		| _		-> s ^ "[" ^ concat "; " (List.map (fun n -> declarator_suffix_string n) nl) ^ "]")

let input_declarator_string = function
	| IdName(n)	-> declarator_string n
	| IdInit(n)	-> sprintf "Init(%s)" (declarator_string n) 

let unfold_lhs_string = function
	| UlId(i)		-> i
	| UlUnderscore	-> "_"

let lhs_string = function
	| LUnfolding(ul)	-> concat "; " (List.map (fun u -> unfold_lhs_string u) ul)
	| LParams(s, pl)	-> 
		(match pl with
			| []	-> s
			| _		-> s ^ " [" ^ concat "; " (List.map (fun p -> formal_param_string p) pl) ^ "]")
	
let constant_string = function
	| CtBool(s, e)	-> sprintf "Bool(%s; %s)" s (expr_string e)
	| CtInt(s, e)	-> sprintf "Int(%s; %s)" s (expr_string e)

let constraint_string = function
	| CrCons(e)	-> expr_string e
	| CrInit(e)	-> sprintf "Init(%s)" (expr_string e)

let declaration_string = function
	| DcTyped(t, nl)	->
		(match nl with
			| [n]	-> sprintf "Typed(%s; %s)" (typedef_string t) (declarator_string n)
			| _		-> sprintf "Typed(%s; %s)" (typedef_string t) ("[" ^ concat "; " (List.map (fun n -> declarator_string n) nl) ^ "]"))
	| DcUntyped(nl)		->
		(match nl with
			| [n]	-> sprintf "Untyped(%s)" (declarator_string n)
			| _		-> sprintf "Untyped(%s)" ("[" ^ concat "; " (List.map (fun n -> declarator_string n) nl) ^ "]"))

let definition_string = function
	| DfAssign(l, r)		-> sprintf "Assign(%s: %s)" (lhs_string l) (rhs_string r)
	| DfInit(l, r)			-> sprintf "Init(%s: %s)" (lhs_string l) (rhs_string r)
	| DfNext(l, r)			-> sprintf "Next(%s: %s)" (lhs_string l) (rhs_string r)
	| DfMemory(l, r1, r2)	-> sprintf "Memory(%s: %s; %s)" (lhs_string l) (rhs_string r1) (rhs_string r2)

let input_string = function
	| ITyped(t, il)	->
		(match il with
			| [i]	-> sprintf "Typed(%s; %s)" (typedef_string t) (input_declarator_string i)
			| _		-> sprintf "Typed(%s; %s)" (typedef_string t) ("[" ^ concat ", " (List.map (fun i -> input_declarator_string i) il) ^ "]"))
	| IUntyped(il)	->
		(match il with
			| [i]	-> sprintf "Untyped(%s)" (input_declarator_string i)
			| _		-> sprintf "Untyped(%s)" ("[" ^ concat "; " (List.map (fun i -> input_declarator_string i) il) ^ "]"))

let type_string = function
	| TType(t, nl)			->
		(match nl with
			[n] -> sprintf "Type(%s; %s)" (typedef_string t) (declarator_string n)
			| _ -> sprintf "Type(%s; %s)" (typedef_string t) ("[" ^ concat "; " (List.map (fun n -> declarator_string n) nl) ^ "]"))
	| TEnum(sl, s)			-> sprintf "Enum(%s; %s)" ("[" ^ concat "; " sl ^ "]") s
	| TSort(s)				-> sprintf "Sort(%s)" s
	| TSortContrib(sc, s)	-> sprintf "SortContrib(%s; %s)" (sort_contrib_string sc) s

let rec sections_string = function
	| SConstants(l)		-> "Constants[\n" ^ List.fold_left (fun acc c -> acc ^ "\t" ^ constant_string c ^ ";\n") "" l ^ "]\n"
	| SConstraints(l)	-> "Constraints[\n" ^ List.fold_left (fun acc c -> acc ^ "\t" ^ constraint_string c ^ ";\n") "" l ^ "]\n"
	| SDeclarations(l)	-> "Declarations[\n" ^ List.fold_left (fun acc d -> acc ^ "\t" ^ declaration_string d ^ ";\n") "" l ^ "]\n"
	| SDefinitions(l)	-> "Definitions[\n" ^ List.fold_left (fun acc d -> acc ^ "\t" ^ definition_string d ^ ";\n") "" l ^ "]\n"
	| SInputs(l)		-> "Inputs[\n" ^ List.fold_left (fun acc i -> acc ^ "\t" ^ input_string i ^ ";\n") "" l ^ "]\n"
	| SNamespaces(l)	-> "Namespaces[\n" ^ List.fold_left (fun acc t -> acc ^ "\t" ^ fst t ^ ast_string (Some (snd t)) ^ ";\n") "" l ^ "]\n"
	| SOutputs(l)		-> "Outputs[\n" ^ List.fold_left (fun acc e -> acc ^ "\t" ^ expr_string e ^ ";\n") "" l ^ "]\n"
	| SProofs(l)		-> "Proof Obligations[\n" ^ List.fold_left (fun acc e -> acc ^ "\t" ^ expr_string e ^ ";\n") "" l ^ "]\n"
	| STypes(l)			-> "Types[\n" ^ List.fold_left (fun acc t -> acc ^ "\t" ^ type_string t ^ ";\n") "" l ^ "]\n"

and ast_string = function
	| Some ast ->
		let rec sprintf_ast a =
			match a with
				| [] -> ""
				| h :: t -> sections_string h ^ "\n" ^ sprintf_ast t
		in sprintf_ast ast
	| None -> "[+] -print : Some error occured or no AST found to be printed"