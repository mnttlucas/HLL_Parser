open Ast
open Digest
open List
open Utils

(** [Checker] module checks that a .lll file meets the six
	restrictions in the C672_LLL_LFD_1.0_B.pdf documentation by
	constructing the associated syntax tree and then applying
	checks on this tree.*)

(**/**)

let r1_check_definition s e =
	match (s, e) with
		| (_, Equivalence(x, y)) when x <> y -> true
		| (_, Implication(x, y)) when x <> y && x <> Not(y) && y <> Not(x) -> true
		| (x, Memory(y, z)) when (y = True || y = Not(True)) && (y = z || Variable(x) = z) -> false
		| (_, Memory(_, _)) -> true
		| (_, Next(_)) -> true
		| _ -> false

(**/**)

(** [r1_stream_definitions a] checks that the R1 from the LFD is
	met, definitions have to match a specific format.*)
let r1_stream_definitions = function
	| Some a -> fold_left (fun acc t -> r1_check_definition (fst t) (snd t) && acc) true a.definitions
	| None -> false

(**/**)	

let r2_check_no_equivalent d m =
	match d with
		| Equivalence(e1, e2) -> not(StringSet.mem (string (expr_string d)) m) && not(StringSet.mem (string (expr_string (Equivalence(e2, e1)))) m)
		| Implication(e1, e2) -> not(StringSet.mem (string (expr_string d)) m) && not(StringSet.mem (string (expr_string (Implication(_inv_not e2, _inv_not e1)))) m)
		| Memory(e1, e2) -> not(StringSet.mem (string (expr_string d)) m) && not(StringSet.mem (string (expr_string (Memory(_inv_not e1, _inv_not e2)))) m)
		| Next(_) -> not(StringSet.mem (string (expr_string d)) m)
		| _ -> false

(**/**)

(** [r2_no_equivalent_definition a] checks that the R2 from the
	LFD is met, a file should not contain two equivalent stream
	Definitions.*)
let r2_no_equivalent_definition = function
	| Some a -> fold_left (fun acc t -> (r2_check_no_equivalent (snd t) (snd acc) && (fst acc), StringSet.add (string (expr_string (snd t))) (snd acc))) (true, StringSet.empty) a.definitions
	| None -> (false, StringSet.empty)

(**/**)

let r3_next_valid e l = not(StringSet.mem (expr_string (_base_var e)) l)
	
(**/**)

(** [r3_next_streams a] checks that the R3 from the LFD is met,
	a Next should apply only to an Input or another Next.*)
let r3_next_streams = function
	| Some a -> fold_left (fun acc t -> match t with | (_, Next(e)) -> (r3_next_valid e (snd acc) && (fst acc), snd acc) | (s, _) -> (fst acc, StringSet.add s (snd acc))) (true, StringSet.empty) a.definitions
	| None -> (false, StringSet.empty)

(** [r4_definitions_start a] checks that the R4 from the LFD is
	met, the Definitions section should must start with :
	__init__ = TRUE, ~TRUE*)
let r4_definitions_start = function
	| Some a ->
	(match a.definitions with
		| h :: _ when fst h = "__init__" && snd h = Memory(True, Not(True)) -> true
		| _ -> false)
	| None -> false

(**/**)

let r5_definitions_valid i d =
	let mapi1 = StringSet.of_list i in
	let mapd = StringSet.of_list (rev_map (fun t -> fst t) d) in
	let mapi2 = fold_left (fun acc t ->
		match (snd t) with 
			| Equivalence(e, _) when not(StringSet.mem (expr_string (_base_var e)) acc) && not(StringSet.mem (expr_string (_base_var e)) mapd) -> StringSet.add (expr_string (_base_var e)) acc
			| Equivalence(_, e) when not(StringSet.mem (expr_string (_base_var e)) acc) && not(StringSet.mem (expr_string (_base_var e)) mapd) -> StringSet.add (expr_string (_base_var e)) acc
			| Implication(e, _) when not(StringSet.mem (expr_string (_base_var e)) acc) && not(StringSet.mem (expr_string (_base_var e)) mapd) -> StringSet.add (expr_string (_base_var e)) acc
			| Implication(_, e) when not(StringSet.mem (expr_string (_base_var e)) acc) && not(StringSet.mem (expr_string (_base_var e)) mapd) -> StringSet.add (expr_string (_base_var e)) acc
			| Memory(e, _) when not(StringSet.mem (expr_string (_base_var e)) acc) && not(StringSet.mem (expr_string (_base_var e)) mapd) -> StringSet.add (expr_string (_base_var e)) acc
			| Memory(_, e) when not(StringSet.mem (expr_string (_base_var e)) acc) && not(StringSet.mem (expr_string (_base_var e)) mapd) -> StringSet.add (expr_string (_base_var e)) acc
			| Next(e) when not(StringSet.mem (expr_string (_base_var e)) acc) && not(StringSet.mem (expr_string (_base_var e)) mapd) -> StringSet.add (expr_string (_base_var e)) acc
			| _ -> acc) mapi1 d in
	let rec iterate_definitions l m =
	match l with
		| [] -> true
		| h :: t ->
		(match h with
			| (_, Equivalence(e1, e2)) when not(StringSet.mem (expr_string (_base_var e1)) m) || not(StringSet.mem (expr_string (_base_var e2)) m) -> false
			| (_, Implication(e1, e2)) when not(StringSet.mem (expr_string (_base_var e1)) m) || not(StringSet.mem (expr_string (_base_var e2)) m) -> false
			| (_, Memory(e1, _)) when not(StringSet.mem (expr_string (_base_var e1)) m) -> false
			| (_, Next(e)) when not(StringSet.mem (expr_string (_base_var e)) m) -> false
			| (s, _) -> iterate_definitions t (StringSet.add s m))
	in
	iterate_definitions d mapi2

(**/**)

(** [r5_definitions_sorted a] checks that the R5 from the LFD is
	met, the Definitions must be topologically sorted.*)
let r5_definitions_sorted = function
	| Some a -> r5_definitions_valid a.inputs a.definitions
	| None -> false

(**/**)

let rec r6_check_definitions l1 l2 m =
	match (l1, l2) with
		| ([], []) -> true
		| ([], h :: _) when StringSet.mem h m -> false
		| ([] , h :: t) -> r6_check_definitions [] t (StringSet.add h m)
		| (h :: _, _) when StringSet.mem h m -> false
		| (h :: t, l2) -> r6_check_definitions t l2 (StringSet.add h m)

(**/**)

(** [r6_no_double_definitions a] checks that the R6 from the LFD
	is met, an input stream must not have a definition and a
	stream cannot be defined more than once. Note that map is not
	tail-recursive so we should re-implement it or use another
	tail-recursive function such as rev_map here.*)
let r6_no_double_definitions = function
	| Some a -> r6_check_definitions a.inputs (rev_map (fun t -> fst t) a.definitions) StringSet.empty
	| None -> false

(** [check_ast a] is called when the -check option is enabled,
	this functions checks that each previous fule from the LFD is
	met.*)
let check_ast a =
	r1_stream_definitions a
	&& fst (r2_no_equivalent_definition a)
	&& fst (r3_next_streams a)
	&& r4_definitions_start a
	&& r5_definitions_sorted a
	&& r6_no_double_definitions a