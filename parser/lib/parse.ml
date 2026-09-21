open Lexing
open Printf

(** [Parse] module makes the [Main] module and the tests one
	lighter, with the main function returning an ast option from a
	[lexbuf].*)

(** [print_position outx lexbuf] is used in the event of a Lexer
	or Parser error to redirect a character string to the [outx]
	stream indicating the type of error as well as the line and
	character	of the [lexbuf] that caused the error.*)
let print_position outx lexbuf =
	let pos = lexbuf.lex_curr_p in
	fprintf outx "[Line %d, Character %d]" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

(** [parse_with_error lexbuf] this function parses a [lexbuf]
	file, returning Some ast on success and None on any Lexer or
	Parser error. In the case of an error, it also displays
	information about it by calling [print_position lexbuf].*)
let parse_with_error lexbuf =
	try Parser.main Lexer.token lexbuf with
		| Lexer.SyntaxError e	-> fprintf stderr "%s : %a\n" e print_position lexbuf; None
		| Parser.Error			-> fprintf stderr "Syntax Error : %a\n" print_position lexbuf; None