open Ast
open List

(** [Processing] module is used to apply transformations to the
	AST to make translation into SMT formulas easier and more
	readable*)

type declarations_proc =
	| ProcDc	of typedef * declarator

type definitions_proc =
	| ProcDfAssign	of typedef * lhs * rhs
	| ProcDfInit	of typedef * lhs * rhs
	| ProcDfNext	of typedef * lhs * rhs
	| ProcDfMemory	of typedef * lhs * rhs * rhs

type inputs_proc =
	| ProcIn		of typedef * input_declarator

type types_proc =
	| ProcTType			of typedef * declarator
	| ProcTEnum			of string list * string
	| ProcTSort			of string
	| ProcTSortContrib	of sort_contrib * string

type ast = 
	{
		constants		: constants list;
		constraints		: constraints list;
		declarations	: declarations_proc list;
		definitions		: definitions_proc list;
		inputs			: inputs_proc list;
		namespaces		: (string * ast) list;
		outputs			: expr list;
		proofs			: expr list;
		types			: types_proc list;
	}

let empty_ast = {constants = []; constraints = []; declarations = []; definitions = []; inputs = []; namespaces = []; outputs = []; proofs = []; types = []}

let rec process_declarations dcl l =
	match dcl with
		| [] -> l
		| h :: t ->
		(match h with
			| DcTyped(ty, dl) -> process_declarations t (l @ (map (fun d -> ProcDc(ty, d)) dl))
			| DcUntyped(dl) -> process_declarations t (l @ (map (fun d -> ProcDc(TBool, d)) dl)))

let rec get_type d l =
	match d with
		| [] -> TBool
		| ProcDc(ty, DDeclarator(s, [])) :: _ when s = l -> ty
		| _ :: t -> get_type t l

(* Pour l'instant pas d'implémentation complète comme DefUndeclaredType page 94 *)
let rec process_definitions sa dfl l =
	match dfl with
		| [] -> l
		| h :: t ->
		(match h with
			| DfAssign(LUnfolding([UlId(s)]), r) -> process_definitions sa t (l @ [ProcDfAssign(get_type sa.declarations s, LUnfolding([UlId(s)]), r)])
			| DfInit(LUnfolding([UlId(s)]), r) -> process_definitions sa t (l @ [ProcDfInit(get_type sa.declarations s, LUnfolding([UlId(s)]), r)])
			| DfNext(LUnfolding([UlId(s)]), r) -> process_definitions sa t (l @ [ProcDfNext(get_type sa.declarations s, LUnfolding([UlId(s)]), r)])
			| DfMemory(LUnfolding([UlId(s)]), r1, r2) -> process_definitions sa t (l @ [ProcDfMemory(get_type sa.declarations s, LUnfolding([UlId(s)]), r1, r2)])
			| _ -> Printf.printf "[+] process_definitions : Unexpected definition\n"; process_definitions sa t l)

let rec process_inputs il l =
	match il with
		| [] -> l
		| h :: t ->
		(match h with
			| ITyped(ty, dl) -> process_inputs t (l @ (map (fun d -> ProcIn(ty, d)) dl))
			| IUntyped(dl) -> process_inputs t (l @ (map (fun d -> ProcIn(TBool, d)) dl)))

let rec process_types tl l =
	match tl with
		| [] -> l
		| h :: t ->
		(match h with
			| TType(ty, dl) -> process_types t (l @ (map (fun d -> ProcTType(ty, d)) dl))
			| TEnum(st, s) -> process_types t (l @ [ProcTEnum(st, s)])
			| TSort(s) -> process_types t (l @ [ProcTSort(s)])
			| TSortContrib(sc, s) -> process_types t (l @ [ProcTSortContrib(sc, s)]))

(*gère les def implicites (crade)*)
let rec definitions_undeclared d dfl l =
	match dfl with
		| [] -> l
		| DfAssign(LUnfolding([UlId(s)]), _) :: t when for_all (fun dc -> match dc with | ProcDc(_, DDeclarator(st, _)) -> st <> s) d -> (* Printf.printf "%s\n" s; *) definitions_undeclared d t (l @ [ProcDc(TBool, DDeclarator(s, []))])
		| DfInit(LUnfolding([UlId(s)]), _) :: t when for_all (fun dc -> match dc with | ProcDc(_, DDeclarator(st, _)) -> st <> s) d -> (* Printf.printf "%s\n" s; *) definitions_undeclared d t (l @ [ProcDc(TBool, DDeclarator(s, []))])
		| DfNext(LUnfolding([UlId(s)]), _) :: t when for_all (fun dc -> match dc with | ProcDc(_, DDeclarator(st, _)) -> st <> s) d -> (* Printf.printf "%s\n" s; *) definitions_undeclared d t (l @ [ProcDc(TBool, DDeclarator(s, []))])
		| DfMemory(LUnfolding([UlId(s)]), _, _) :: t when for_all (fun dc -> match dc with | ProcDc(_, DDeclarator(st, _)) -> st <> s) d -> (* Printf.printf "%s\n" s; *) definitions_undeclared d t (l @ [ProcDc(TBool, DDeclarator(s, []))])
		| _ :: t -> definitions_undeclared d t l


let process_ast a sa dfl =
	match a with
		| Some ast ->
			let rec process_some_ast a sa dfl = 
			match a with
				| [] -> {sa with definitions = sa.definitions @ (process_definitions sa dfl []); declarations = sa.declarations @ (definitions_undeclared sa.declarations dfl [])}
				| h :: t ->
				(match h with
					| SConstants(l) -> process_some_ast t {sa with constants = sa.constants @ l} dfl
					| SConstraints(l) -> process_some_ast t {sa with constraints = sa.constraints @ l} dfl
					| SDeclarations(l) -> process_some_ast t {sa with declarations = sa.declarations @ (process_declarations l [])} dfl
					| SDefinitions(l) -> process_some_ast t sa (dfl @ l)
					| SInputs(l) -> process_some_ast t {sa with inputs = sa.inputs @ (process_inputs l [])} dfl
					| SNamespaces(l) -> process_some_ast t {sa with namespaces = sa.namespaces @ (map (fun (s, sl) -> (s, process_some_ast sl empty_ast [])) l)} dfl
					| SOutputs(l) -> process_some_ast t {sa with outputs = sa.outputs @ l} dfl
					| SProofs(l) -> process_some_ast t {sa with proofs = sa.proofs @ l} dfl
					| STypes(l) -> process_some_ast t {sa with types = sa.types @ (process_types l [])} dfl)
				in process_some_ast ast sa dfl
		| None -> empty_ast