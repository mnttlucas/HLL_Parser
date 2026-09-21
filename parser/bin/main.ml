open Arg
open Lib
open Printf
open Sys

let usage_msg = "dune exec -- Parser -lll <path-to-LLL_file> [-check] [-help] [-print] [-smt <path-to-SMT-output> -mode <bv/int/...> -stop <s> -lookahead <l>]"
let input_file = ref ""
let output_dir = ref ""
let option_check = ref false
let option_print = ref false
let mode = ref "bv" (* Default : Int, bv : Bit Vector *)
let timesteps = ref 0
let lookahead = ref 0
let speclist = 
	[
		("-check", Set option_check, "Run restrictions checker");
		("-lll", Set_string input_file, "Set LLL input file");
		("-print", Set option_print, "Print the AST to stdout");
		("-smt", Set_string output_dir, "Set SMT output directory");
		("-mode", Set_string mode, "Set SMT logic");
		("-stop", Set_int timesteps, "Number of timesteps for the generated SMT");
		("-lookahead", Set_int lookahead, "Lookahead for the generated SMT")
	]

let () = parse speclist (fun _ -> ()) usage_msg

let () =
	let lexbuf = Lexing.from_channel (open_in !input_file) in
	let ast = Parse.parse_with_error lexbuf in

	(if !option_print then
		printf "%s\n" (Ast.ast_string ast));

	(if !option_check then 
		if Checker.check_ast ast then
			printf "[+] -check : AST Valid\n"
		else
			printf "[+] -check : AST Invalid\n");
	
	(if !output_dir != "" then
		(if not (file_exists !output_dir && is_directory !output_dir) then mkdir !output_dir 0o755;
		match !mode with
			| "bv" -> Translater.generate_smt_bv !output_dir !timesteps !lookahead ast
			| "int" -> Translater.generate_smt_int !output_dir !timesteps !lookahead ast
			| _ -> fprintf stderr "[+] -smt : Unknown SMT mode\n"));