open Ast
open List

(** [Utils] module contains various utility functions, in
	particular for the [Checker] module.*)

(** [StringSet] is used in [Checker] module to have existence
	checks in O(log n) *)
module StringSet = Set.Make(String)

(** [_var_list e] recursively returns the list of all variables
	present in any expression [e].*)
let rec _var_list e =
	match e with
		| Equivalence(e1, e2)	-> (_var_list e1) @ (_var_list e2)
		| Implication(e1, e2)	-> (_var_list e1) @ (_var_list e2)
		| Init(e)				-> _var_list e
		| Memory(e1, e2)		-> (_var_list e1) @ (_var_list e2)
		| Next(e)				-> _var_list e
		| Not(e)				-> _var_list e
		| True					-> []
		| Variable(e)			-> [e]

(** [_base_var e] retrieves the basic variables inside a Not in
	an LLL definition.*)
let rec _base_var e =
	match e with
		| Not(e)	-> _base_var e
		| e			-> e

(** [_inv_not e] returns the negation of an expression.*)
let _inv_not e =
	match e with
		| Not(e)	-> e
		| e			-> Not(e)