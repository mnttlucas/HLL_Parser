open Alcotest
open Lib

let test_i0 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/r4/test_invalid1.lll")) in
	Alcotest.(check bool) "same" false (Checker.r4_definitions_start ast)

let test_i1 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/r4/test_invalid2.lll")) in
	Alcotest.(check bool) "same" false (Checker.r4_definitions_start ast)

let test_v0 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/r4/test_valid.lll")) in
	Alcotest.(check bool) "same" true (Checker.r4_definitions_start ast)

let tests = [
	test_case "Invalid Definitions Start 1"	`Quick test_i0;
	test_case "Invalid Definitions Start 2"	`Quick test_i1;
	test_case "Valid Definitions Start"		`Quick test_v0;
]