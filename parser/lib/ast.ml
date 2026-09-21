open List
open Printf

(** [Ast] module defines the different types of LLL expressions,
	as well as the pretty printing functions used when the -print
	option is enabled*)

(** [expr] is the subtype used by the [ast] type, and can be used
	to recursively define any expression present in the AST of an
	LLL file.*)
type expr =
	| Equivalence	of expr * expr
	| Implication	of expr * expr
	| Init			of expr
	| Memory		of expr * expr
	| Next			of expr
	| Not 			of expr
	| True
	| Variable 		of string

(** [ast] is the type returned by the parser, the LLL's LFD
	specifies that an LLL file will always be composed of these 5
	sections - even if empty - and that there are no two
	occurrences of the same section, which is not true for HLL. A
	record was therefore more appropriate than a “section list”
	type, where section would be either a string list or an expr
	list \[...\].*)
type ast =
	{
		constraints	: expr list;
		definitions	: (string * expr) list;
		inputs		: string list;
		outputs		: expr list;
		proofs		: expr list;
	}

(** [expr_string e] recursively constructs the pretty print
	string of each possible expr [e].*)
let rec expr_string = function
	| Equivalence(e1, e2)	-> sprintf "Equivalence(%s, %s)" (expr_string e1) (expr_string e2)
	| Implication(e1, e2)	-> sprintf "Implication(%s, %s)" (expr_string e1) (expr_string e2)
	| Init(e)				-> sprintf "Init(%s)" (expr_string e)
	| Memory(e1, e2)		-> sprintf "Memory(%s, %s)" (expr_string e1) (expr_string e2)
	| Next(e)				-> sprintf "Next(%s)" (expr_string e)
	| Not(e)				-> sprintf "Not(%s)" (expr_string e)
	| True					-> "TRUE"
	| Variable(s)			-> s

(** [ast_string a] constructs the pretty print by concatenating
	each AST section.*)
let ast_string = function
	| Some a ->
		"Inputs[" ^ fold_left (fun acc s -> acc ^ s ^ ";\n") "" a.inputs ^ "]\n"
		^ "\n" ^ "Definitions[" ^ fold_left (fun acc t -> acc ^ fst t ^ ": " ^ expr_string (snd t) ^ ";\n") "" a.definitions ^ "]\n"
		^ "\n" ^ "Outputs[" ^ fold_left (fun acc e -> acc ^ expr_string e ^ ";\n") "" a.outputs ^ "]\n"
		^ "\n" ^ "Constraints[" ^ fold_left (fun acc e -> acc ^ expr_string e ^ ";\n") "" a.constraints ^ "]\n"
		^ "\n" ^ "Proofs[" ^ fold_left (fun acc e -> acc ^ expr_string e ^ ";\n") "" a.proofs ^ "]\n"
	| None -> "[+] -print : Some error occured or no AST found to be printed"