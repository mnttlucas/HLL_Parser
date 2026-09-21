open Alcotest
open Lib

let test_i0 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/r5/test_invalid1.lll")) in
	Alcotest.(check bool) "same" false (Checker.r5_definitions_sorted ast)

let test_i1 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/r5/test_invalid2.lll")) in
	Alcotest.(check bool) "same" false (Checker.r5_definitions_sorted ast)

let test_i2 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/r5/test_invalid3.lll")) in
	Alcotest.(check bool) "same" false (Checker.r5_definitions_sorted ast)

let test_i3 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/r5/test_invalid4.lll")) in
	Alcotest.(check bool) "same" false (Checker.r5_definitions_sorted ast)

let test_v0 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/r5/test_valid.lll")) in
	Alcotest.(check bool) "same" true (Checker.r5_definitions_sorted ast)

let tests = [
	test_case "Invalid Definitions Sort 1"	`Quick test_i0;
	test_case "Invalid Definitions Sort 2"	`Quick test_i1;
	test_case "Invalid Definitions Sort 3"	`Quick test_i2;
	test_case "Invalid Definitions Sort 3"	`Quick test_i3;
	test_case "Valid Definitions Sort"		`Quick test_v0;
]