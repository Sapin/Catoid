
let todo () = assert false

module SMap = Map.Make (String)

type env_t = {
	e_name_map : int SMap.t;
	e_scope : int;
}

let env_empty : env_t = {
	e_name_map = SMap.empty;
	e_scope = 0;
}

let env_getid p s : int =
	p.e_scope - (SMap.find s p.e_name_map) - 1

let env_scope_enter p s : env_t = {
	e_name_map = SMap.add s p.e_scope p.e_name_map;
	e_scope = p.e_scope + 1;
}

let rec term p t : Core.term_t =
	match Ast.(t.t_node) with
	| Ast.T_Var s -> Core.T_Var (env_getid p s)
	| Ast.T_Uni i -> Core.T_Uni i
	| Ast.T_Abs (s, a, b) ->
		let a = term p a in
		let p = env_scope_enter p s in
		let b = term p b in
		Core.T_Abs (a, b)
	| Ast.T_Arr (s, a, b) ->
		let a = term p a in
		let p = env_scope_enter p s in
		let b = term p b in
		Core.T_Arr (a, b)
	| Ast.T_App (a, b) ->
		Core.T_App (term p a, term p b)

