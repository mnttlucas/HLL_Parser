open Alcotest
open Lib

let pp (out:Format.formatter)(a:Ast.ast option) = Format.pp_print_string out (Ast.ast_string a)
let error = testable pp (fun x y -> x = y)

let test_i0 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/errors/test_invalid1.lll")) in
	Alcotest.(check error) "same" None ast

let test_i1 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/errors/test_invalid2.lll")) in
	Alcotest.(check error) "same" None ast

let test_i2 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/errors/test_invalid3.lll")) in
	Alcotest.(check error) "same" None ast

let test_i3 () =
	let ast = Parse.parse_with_error (Lexing.from_channel (open_in "in/errors/test_invalid4.lll")) in
	Alcotest.(check error) "same" None ast

let tests = [
	test_case "Lexer Error 1"		`Quick test_i0;
	test_case "Lexer Error 2"		`Quick test_i1;
	test_case "Parser Error 1"		`Quick test_i2;
	test_case "Parser Error 2"		`Quick test_i3;
]