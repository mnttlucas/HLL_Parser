open Alcotest
open Lib

let test_i0 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/r2/test_invalid1.lll")) in
	Alcotest.(check bool) "same" false (fst (Checker.r2_no_equivalent_definition ast))

let test_i1 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/r2/test_invalid2.lll")) in
	Alcotest.(check bool) "same" false (fst (Checker.r2_no_equivalent_definition ast))

let test_i2 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/r2/test_invalid3.lll")) in
	Alcotest.(check bool) "same" false (fst (Checker.r2_no_equivalent_definition ast))

let test_i3 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/r2/test_invalid4.lll")) in
	Alcotest.(check bool) "same" false (fst (Checker.r2_no_equivalent_definition ast))

let test_i4 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/r2/test_invalid5.lll")) in
	Alcotest.(check bool) "same" false (fst (Checker.r2_no_equivalent_definition ast))

let test_i5 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/r2/test_invalid6.lll")) in
	Alcotest.(check bool) "same" false (fst (Checker.r2_no_equivalent_definition ast))

let test_i6 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/r2/test_invalid7.lll")) in
	Alcotest.(check bool) "same" false (fst (Checker.r2_no_equivalent_definition ast))

let test_v0 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/r2/test_valid.lll")) in
	Alcotest.(check bool) "same" true (fst (Checker.r2_no_equivalent_definition ast))

let tests = [
	test_case "Invalid Equivalent Equivalences 1"	`Quick test_i0;
	test_case "Invalid Equivalent Equivalences 2"	`Quick test_i1;
	test_case "Invalid Equivalent Implications 1"	`Quick test_i2;
	test_case "Invalid Equivalent Implications 2"	`Quick test_i3;
	test_case "Invalid Equivalent Memories 1"		`Quick test_i4;
	test_case "Invalid Equivalent Memories 2"		`Quick test_i5;
	test_case "Invalid Equivalent Next"				`Quick test_i6;
	test_case "Valid Independant Definitions"		`Quick test_v0;
]