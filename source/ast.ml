
	(* Locations *)

type pos_t = {
	p_line : int;
	p_colm : int;
}

type loc_t = {
	l_beg : pos_t;
	l_end : pos_t;
}

let mpos pos : pos_t = {
	p_line = Lexing.(pos.pos_lnum);
	p_colm = Lexing.(pos.pos_cnum - pos.pos_bol);
}

let mloc start end_ : loc_t = {
	l_beg = mpos start;
	l_end = mpos end_;
}

let loc_merge a b : loc_t = {
	l_beg = a.l_beg;
	l_end = b.l_end;
}

exception ParseError of loc_t * string
let throw loc msg = raise (ParseError (loc, msg))

	(* Terms *)

type term_t = {
	t_loc : loc_t;
	t_node : node_t;
}

and node_t =
| T_Var of string
| T_Uni of int
| T_Abs of string * term_t * term_t
| T_Arr of string * term_t * term_t
| T_App of term_t * term_t

type stmt_t = {
	s_name : string;
	s_type : term_t;
	s_term : term_t;
}

