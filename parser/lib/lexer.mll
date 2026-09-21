{
	exception SyntaxError of string
	open Lexing
	open Parser
}

let eol = (("//"[^'\r''\n']*)?'\r'?'\n')
let id = ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''_''0'-'9']*
let ws = [' ''\t']

rule token = parse
	| ws					{ token lexbuf }
	| eol					{ new_line lexbuf; token lexbuf }

	| "Constraints"			{ CONSTRAINTS }
	| "Definitions"			{ DEFINITIONS }
	| "Inputs"				{ INPUTS }
	| "Outputs"				{ OUTPUTS }
	| "Proof Obligations"	{ PROOFS }

	| 'I'					{ INIT }
	| 'X'					{ NEXT }
	| "TRUE"				{ TRUE }

	| ":="					{ ASSIGNMENT }
	| ':'					{ COLON }
	| ','					{ COMMA }
	| "<->"					{ EQUIVALENCE }
	| "->"					{ IMPLICATION }
	| "~"					{ NOT }
	| '('					{ PARENTHESIS_LEFT }
	| ')'					{ PARENTHESIS_RIGHT }
	| ';'					{ SEMICOLON }

	| id as v				{ VAR v }

	| _						{ raise(SyntaxError("Unexpected token: `" ^ lexeme lexbuf ^ "`")) }
	
	| eof					{ EOF }