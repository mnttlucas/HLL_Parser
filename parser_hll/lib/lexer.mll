{
	exception SyntaxError of string
	open Lexing
	open Parser
	open Stdlib

	let comments = ref 0
}

let eol = (("//"[^'\r''\n']*)?'\r'?'\n')
let id = 
	['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''_''0'-'9']*
	| '\''[^'\n''\'']+'\''
	| '\"'[^'\n''\"']+'\"'
let int_literal =
	['0'-'9']('_'?['0'-'9'])*
	| '0'['B''b']['0'-'1']('_'?['0'-'1'])*
	| '0'['X''x']['0'-'9''A'-'F''a'-'f']('_'?['0'-'9''A'-'F''a'-'f'])*
let pragma = '@'[^'\n']*
let ws = [' ''\t']

rule token = parse
	| "/*"								{ incr comments; comment lexbuf }
	| pragma							{ token lexbuf }
	| ws								{ token lexbuf }
	| eol								{ new_line lexbuf; token lexbuf }

	| "Constants" | "constants"			{ CONSTANTS }
	| "Constraints" | "constraints"		{ CONSTRAINTS }
	| "Declarations" | "declarations"	{ DECLARATIONS }
	| "Definitions" | "definitions"		{ DEFINITIONS }
	| "Inputs" | "inputs"				{ INPUTS }
	| "Namespaces" | "namespaces"		{ NAMESPACES }
	| "Obligations" | "obligations"		{ OBLIGATIONS }
	| "Outputs" | "outputs"				{ OUTPUTS }
	| "Proof" | "proof"					{ PROOF }
	| "Types" | "types"					{ TYPES }

	| "bool"							{ BOOL }
	| "enum"							{ ENUM }
	| "int"								{ INT }
	| "signed"							{ SIGNED }
	| "struct"							{ STRUCT }
	| "tuple"							{ TUPLE }
	| "unsigned"						{ UNSIGNED }

	| "ALL"								{ ALL }
	| "bin2s"							{ BIN2S }
	| "bin2u"							{ BIN2U }
	| "cast"							{ CAST }
	| "CONJ"							{ CONJ }
	| "$abs"							{ DABS }
	| "$and"							{ DAND }
	| "$max"							{ DMAX }
	| "$min"							{ DMIN }
	| "$not"							{ DNOT }
	| "$or"								{ DOR }
	| "$xor"							{ DXOR }
	| "DISJ"							{ DISJ }
	| "elif"							{ ELIF }
	| "else"							{ ELSE }
	| "FALSE" | "False" | "false"		{ FALSE }
	| "if"								{ IF }
	| 'I'								{ INIT }
	| "$items"							{ ITEMS }
	| "lambda"							{ LAMBDA }
	| 'X'								{ NEXT }
	| "population_count_eq"				{ PCEQ }
	| "population_count_gt"				{ PCGT }
	| "population_count_lt"				{ PCLT }
	| "PRE" | "pre"						{ PRE }
	| "PROD"							{ PROD }
	| "s2bin"							{ S2BIN }
	| "SELECT"							{ SELECT }
	| "SOME"							{ SOME }
	| "sort"							{ SORT }
	| "SUM"								{ SUM }
	| "then"							{ THEN }
	| "TRUE" | "True" | "true"			{ TRUE }
	| "u2bin"							{ U2BIN }
	| "with"							{ WITH }

	| '&'								{ AND }
	| "=>"								{ ARROW }
	| ":="								{ ASSIGNMENT }
	| '{'								{ BRACEL }
	| '}'								{ BRACER }
	| '['								{ BRACKL }
	| ']'								{ BRACKR }
	| "/<"								{ CEIL }
	| '^'								{ CIRCUMFLEX }
	| ':'								{ COLON }
	| ','								{ COMMA }
	| "!=" | "<>"						{ DIFFERENT }
	| '.'								{ DOT }
	| "::"								{ DOUBLE_COLON }
	| '=' | "=="						{ EQUAL }
	| "<->"								{ EQUIVALENCE }
	| "/>"								{ FLOOR }
	| '>'								{ GREATER }
	| ">="								{ GREATER_EQUAL}
	| "->"								{ IMPLICATION }
	| '<'								{ LESS }
	| "<="								{ LESS_EQUAL }
	| '-'								{ MINUS }
	| "~"								{ NOT }
	| '#'								{ OR }
	| '('								{ PARL }
	| ')'								{ PARR }
	| '%'								{ PERCENT }
	| '+'								{ PLUS }
	| ';'								{ SEMICOLON }
	| "<<"								{ SHIFT_LEFT }
	| ">>"								{ SHIFT_RIGHT }
	| '/'								{ SLASH }
	| '*'								{ STAR }
	| '_'								{ UNDERSCORE }
	| '|'								{ VERTICAL }
	| "#!"								{ XOR }

	| int_literal as i					{ LIT i }
	| id as i							{ ID i }

	| _									{ raise(SyntaxError("Unexpected token: `" ^ lexeme lexbuf ^ "`")) }
	
	| eof								{ EOF }

and comment = parse
	| "/*"								{ incr comments; comment lexbuf }
	| "*/"								{ decr comments; if !comments = 0 then token lexbuf else comment lexbuf}
	| '\n'								{ new_line lexbuf; comment lexbuf }
	| _									{ comment lexbuf }