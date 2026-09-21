open Alcotest
open Lib

let test_i0 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/r3/test_invalid.lll")) in
	Alcotest.(check bool) "same" false (fst (Checker.r3_next_streams ast))

let test_v0 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/r3/test_valid.lll")) in
	Alcotest.(check bool) "same" true (fst (Checker.r3_next_streams ast))

let tests = [
	test_case "Invalid Next Stream"	`Quick test_i0;
	test_case "Valid Next Stream"	`Quick test_v0;
]