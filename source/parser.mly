%{
	open Ast
	open Lexing

	let mkterm start end_ node : Ast.term_t = {
		t_loc = Ast.mloc start end_;
		t_node = node;
	}

%}

%token <string> VAR
%token <int> NUM
%token LEFT RIGHT
%token LAMBDA IN DOT
%token ARROW UNIV
%token LET EQ END
%token EOF

%start file
%type <Ast.stmt_t list> file

%%

atom:
	x=VAR {mkterm $startpos $endpos (T_Var x)}
|	UNIV {mkterm $startpos $endpos (T_Uni 0)}
|	UNIV i=NUM {mkterm $startpos $endpos (T_Uni i)}
|	LEFT t=term RIGHT {t}
;

expr:
	t=atom {t}
|	a=atom ARROW b=expr {mkterm $startpos $endpos (T_Arr ("", a, b))}
|	LEFT x=VAR IN a=term RIGHT ARROW b=expr {mkterm $startpos $endpos (T_Arr (x, a, b))}
|	f=atom xs=atom+ {
		List.fold_left (fun f x -> {
			t_loc = loc_merge f.t_loc x.t_loc;
			t_node = T_App (f, x);
		}) f xs
	}
;

term:
	t=expr {t}
|	LAMBDA x=VAR IN ty=term DOT t=term {
		mkterm $startpos $endpos (T_Abs (x, ty, t))
	}
|	LET x=VAR IN ty=term EQ t1=term END t2=term {
		let abs = {t_loc = ty.t_loc; t_node = T_Abs (x, ty, t2)} in
		mkterm $startpos $endpos (T_App (abs, t1))
	}
;

stmt:
	LET x=VAR IN ty=term EQ t=term END END
		{{ s_name = x; s_term = t; s_type = ty; }}
;

file:
	f=stmt* EOF {f}

