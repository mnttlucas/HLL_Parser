open Ast
open Float

(** [Utils] module contains various utility functions.*)

let bits_from_int i = to_int(floor ((log2 (of_int i)) /. 4.0 +. 1.0)) * 4

(** [_inv_not e] returns the negation of an expression.*)
let _inv_not e =
	match e with
		| EUnOp(UNot, e)	-> e
		| e					-> EUnOp(UNot, e)

let rec _max_bv s acc =
	match s with
		| 0 -> "#x" ^ acc
		| _ -> _max_bv (s - 4) (acc ^ "f") 

let rec rep n s acc =
	match n with
		| 0 -> acc
		| n -> rep (n - 1) s (acc ^ " " ^ s)