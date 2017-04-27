{
	open Lexing
	open Parser

	let mloc lexbuf : Ast.loc_t =
		Ast.mloc lexbuf.lex_start_p lexbuf.lex_curr_p

}

rule token = parse

	| [' ' '\t']+    { token lexbuf }
	| "#"  [^ '\n']* { token lexbuf }
	| "--" [^ '\n']* { token lexbuf }
	| ['\n'] {new_line lexbuf; token lexbuf }

	| ['a'-'z' 'A'-'Z' '_'] (['a'-'z' 'A'-'Z' '0'-'9' '_'] )* as s {
		VAR s
	}

	| (['0'-'9'])* as s {
		NUM (int_of_string s)
	}

	| '(' { LEFT }
	| ')' { RIGHT }

	| '\\' { LAMBDA }
	| ':' { IN }
	| '.' { DOT }

	| "->" { ARROW }
	| "*" { UNIV }

	| '$' { LET }
	| '=' { EQ }
	| ';' { END }

	| eof { EOF }

	| (_) as c {
		Ast.throw (mloc lexbuf) (
			Format.sprintf "Unexpected character '%c'." c
		)
	}

