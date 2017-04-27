
open Format
open Core

let rec print_term scope fmt term =
	match term with
	| T_Var i -> fprintf fmt "%i" (scope - i - 1)
	| T_Uni i -> fprintf fmt "*%i" i
	| T_Abs (a, T_Abs (b, c)) -> fprintf fmt "\\%i:@[%a@].@;<1 2>%a"
		scope
		(print_atom scope) a
		(print_term (scope+1)) (T_Abs (b, c))
	| T_Abs (a, b) -> fprintf fmt "\\%i:@[%a@].@;<1 2>@[%a@]"
		scope
		(print_atom scope) a
		(print_term (scope+1)) b
	| T_Arr (a, T_Arr (b, c)) -> fprintf fmt "(%i:@[%a@]) ->@;<1 2>%a"
		scope
		(print_term scope) a
		(print_term (scope+1)) (T_Arr (b, c))
	| T_Arr (a, b) -> fprintf fmt "(%i:@[%a@]) ->@;<1 2>@[%a@]"
		scope
		(print_term scope) a
		(print_term (scope+1)) b
	| T_App (T_Abs (a, b), c) -> fprintf fmt "$%i:%a=%a;@;<1 2>%a"
		scope
		(print_atom scope) a
		(print_atom (scope+1)) b
		(print_term scope) c
	| T_App (a, b) -> fprintf fmt "%a@;<1 2>%a"
		(print_atom scope) a
		(print_atom scope) b

and print_atom scope fmt term =
	match term with
	| T_Abs (_,_) | T_Arr (_,_) | T_App (_,_) ->
		fprintf fmt "@[(%a)@]" (print_term scope) term
	|_ -> print_term scope fmt term

let print fmt term =
	print_term 0 fmt term

let print_term env fmt term =
	print_term Core.(env.e_scope) fmt term

