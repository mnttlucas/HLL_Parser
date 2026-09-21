open Arg
open Lib
open Printf
open Sys

let usage_msg = "dune exec -- Parser [-help] -hll <path-to-HLL_file> [-print] [-smt <path-to-SMT-output> -mode <bmc|ind> -stop <s> -lookahead <l>]"
let input_file = ref ""
let output_dir = ref ""
let option_print = ref false
let mode = ref "bv"
let timesteps = ref 0
let lookahead = ref 0
let speclist =
	[
		("-hll", Set_string input_file, "Set HLL input file");
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
	let smt_ast = Processing.process_ast ast Processing.empty_ast [] in

	(if !option_print then
		printf "%s" (Ast.ast_string ast));

	(if !output_dir != "" then
		((if not (file_exists !output_dir && is_directory !output_dir) then mkdir !output_dir 0o755);
		match !mode with
			| "bmc" -> Translater.generate_bmc_smt !output_dir !timesteps !lookahead smt_ast
			| "ind" -> Translater.generate_ind_smt !output_dir !timesteps !lookahead smt_ast
			| _ -> fprintf stderr "[+] -smt : Unknown SMT mode\n"));