open Alcotest
open Lib
open Lib.Ast

let pp (out:Format.formatter)(a:Ast.ast option) = Format.pp_print_string out (Ast.ast_string a)
let tree = testable pp (fun x y -> x = y)

let test_constraints = Some {constraints = [Variable("s1"); Not(Variable("s1")); True; Not(True); Init(Variable("s1")); Init(Not(Variable("s1")))]; definitions = []; inputs = []; outputs = []; proofs = []}
let test_definitions = Some {constraints = []; definitions = [("__init__", Memory(True, Not(True))); ("s1", Implication(Not(Variable("s2")), Variable("s3"))); ("s4", Equivalence(Variable("s5"), Variable("s6"))); ("s11", Next(Variable("s5")))]; inputs = []; outputs = []; proofs = []}
let test_inputs = Some {constraints = []; definitions = []; inputs = ["s3"; "s4"; "s5"; "s6"]; outputs = []; proofs = []}
let test_outputs = Some {constraints = []; definitions = []; inputs = []; outputs = [Variable("s1"); Not(Variable("s2")); True; Not(True)]; proofs = []}
let test_proofs = Some {constraints = []; definitions = []; inputs = []; outputs = []; proofs = [True; Variable("s37"); Not(Variable("s1")); Not(True)]}

let test_0 () = 
	Alcotest.(check tree) "same" test_constraints (Parse.parse_with_error (Lexing.from_channel (open_in "in/sections/test_constraint.lll")))

let test_1 () = 
	Alcotest.(check tree) "same" test_definitions (Parse.parse_with_error (Lexing.from_channel (open_in "in/sections/test_definition.lll")))

let test_2 () = 
	Alcotest.(check tree) "same" test_inputs (Parse.parse_with_error (Lexing.from_channel (open_in "in/sections/test_input.lll")))

let test_3 () = 
	Alcotest.(check tree) "same" test_outputs (Parse.parse_with_error (Lexing.from_channel (open_in "in/sections/test_output.lll")))

let test_4 () = 
	Alcotest.(check tree) "same" test_proofs (Parse.parse_with_error (Lexing.from_channel (open_in "in/sections/test_proof.lll")))

let tests = [
	test_case "Constraints Test"		`Quick test_0;
	test_case "Definitions Test"		`Quick test_1;
	test_case "Inputs Test"				`Quick test_2;
	test_case "Outputs Test"			`Quick test_3;
	test_case "Proof Obligations Test"	`Quick test_4;
]