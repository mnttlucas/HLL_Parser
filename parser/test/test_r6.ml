open Alcotest
open Lib

let test_i0 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/r6/test_invalid1.lll")) in
	Alcotest.(check bool) "same" false (Checker.r6_no_double_definitions ast)

let test_i1 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/r6/test_invalid2.lll")) in
	Alcotest.(check bool) "same" false (Checker.r6_no_double_definitions ast)

let test_i2 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/r6/test_invalid3.lll")) in
	Alcotest.(check bool) "same" false (Checker.r6_no_double_definitions ast)

let test_v0 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/r6/test_valid.lll")) in
	Alcotest.(check bool) "same" true (Checker.r6_no_double_definitions ast)

let tests = [
	test_case "Invalid Single Definitions 1"	`Quick test_i0;
	test_case "Invalid Single Definitions 2"	`Quick test_i1;
	test_case "Invalid Single Definitions 3"	`Quick test_i2;
	test_case "Valid Single Definitions"		`Quick test_v0;
]