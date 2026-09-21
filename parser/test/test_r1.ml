open Alcotest
open Lib

let test_i0 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/r1/test_invalid1.lll")) in
	Alcotest.(check bool) "same" false (Checker.r1_stream_definitions ast)

let test_i1 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/r1/test_invalid2.lll")) in
	Alcotest.(check bool) "same" false (Checker.r1_stream_definitions ast)

let test_i2 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/r1/test_invalid3.lll")) in
	Alcotest.(check bool) "same" false (Checker.r1_stream_definitions ast)

let test_i3 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/r1/test_invalid4.lll")) in
	Alcotest.(check bool) "same" false (Checker.r1_stream_definitions ast)

let test_i4 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/r1/test_invalid5.lll")) in
	Alcotest.(check bool) "same" false (Checker.r1_stream_definitions ast)

let test_v0 () =	
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/r1/test_valid.lll")) in
	Alcotest.(check bool) "same" true (Checker.r1_stream_definitions ast)

let test_v1 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/complete/test_river.lll")) in
	Alcotest.(check bool) "same" true (Checker.r1_stream_definitions ast)

let tests = [
	test_case "Invalid Implication 1"	`Quick test_i0;
	test_case "Invalid Implication 2"	`Quick test_i1;
	test_case "Invalid Equivalence"		`Quick test_i2;
	test_case "Invalid Memory 1"		`Quick test_i3;
	test_case "Invalid Memory 2"		`Quick test_i4;
	test_case "Valid Definitions"		`Quick test_v0;
	test_case "Valid River"				`Quick test_v1;
]