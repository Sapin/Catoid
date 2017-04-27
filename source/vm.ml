
open Format

let load filename : Ast.stmt_t list =
	let file = open_in filename in
	let lexbuf = Lexing.from_channel file in
	let prog = Parser.file Lexer.token lexbuf in
	let () = close_in file in
	prog

type env_t = {
	r_ast_env : Ast2core.env_t;
	r_core_env : Core.env_t;
}

let env_empty : env_t = {
	r_ast_env = Ast2core.env_empty;
	r_core_env = Core.env_empty;
}

let env_enter_scope p s ty : env_t = {
	r_ast_env = Ast2core.env_scope_enter p.r_ast_env s;
	r_core_env = Core.env_scope_enter p.r_core_env ty;
}

type type_mismatch_t =
| TM_BadApplication of Ast.loc_t * Core.env_t * Core.term_t * Core.term_t
| TM_NotAnArrow of Ast.loc_t * Core.env_t * Core.term_t

exception TypeMismatch of type_mismatch_t

let rec check_typing p at ct : Core.term_t =
	try Core.term_type p.r_core_env (Core.term_reduce p.r_core_env ct)
	with
	| Core.TypeMismatch -> begin
		match (Ast.(at.t_node), ct) with
		| Ast.T_Var _, Core.T_Var _
		| Ast.T_Uni _, Core.T_Uni _ ->
			(raise Core.TypeMismatch)
		| Ast.T_Abs (s, a1, a2), Core.T_Abs (b1, b2)
		| Ast.T_Arr (s, a1, a2), Core.T_Arr (b1, b2) ->
			let _ = check_typing p a1 b1 in
			let p = env_enter_scope p s (Core.term_reduce p.r_core_env b1) in
			let _ = check_typing p a2 b2 in
			(raise Core.TypeMismatch)
		| Ast.T_App (a1, a2), Core.T_App (b1, b2) -> begin
			let y1 = check_typing p a1 b1 in
			let y2 = check_typing p a2 b2 in
			match y1 with
			| Core.T_Arr (yexp, _) -> (raise (TypeMismatch (
					TM_BadApplication (Ast.(a2.t_loc), p.r_core_env, y2, yexp)
				)))
			| _ -> (raise (TypeMismatch (
					TM_NotAnArrow (Ast.(a1.t_loc), p.r_core_env, y1)
				)))
			end
		| _, _ -> assert false
	end

let stmt_run p s : env_t =
	let te = Ast2core.term p.r_ast_env Ast.(s.s_term) in
	let ty = Ast2core.term p.r_ast_env Ast.(s.s_type) in

	let () = try ignore (check_typing p Ast.(s.s_term) te) with
	| TypeMismatch (TM_BadApplication (loc, env, typ, exp)) ->
		let () = Format.printf
		"@[ (%i:%i):(%i,%i) : Got type@;<1 2>@[%a@], expected type was @;<1 2>@[%a@]@]@."
		Ast.(loc.l_beg.p_line) Ast.(loc.l_beg.p_colm)
		Ast.(loc.l_end.p_line) Ast.(loc.l_end.p_colm)
		Core_printer.print typ
		Core_printer.print exp
		in (raise Core.TypeMismatch)
	| TypeMismatch (TM_NotAnArrow (loc, env, typ)) ->
		let () = Format.printf
		"@[ (%i:%i):(%i:%i) :@;<1 2>@[%a@]@;<1 2>is not an arrow type@]@."
		Ast.(loc.l_beg.p_line) Ast.(loc.l_beg.p_colm)
		Ast.(loc.l_end.p_line) Ast.(loc.l_end.p_colm)
		Core_printer.print typ
		in (raise Core.TypeMismatch)
	in

	let te = Core.term_reduce p.r_core_env te in
	let ty = Core.term_reduce p.r_core_env ty in
	let i = Core.(p.r_core_env.e_scope) in

	let tty = Core.term_type p.r_core_env te in

	let () = fprintf std_formatter "@[ $ %i : @[%a@] : *%i@;<1 2>= @[%a@] ;;@]@." i
		(Core_printer.print_term p.r_core_env) tty
		(Core.type_kind p.r_core_env tty)
		(Core_printer.print_term p.r_core_env) te
	in

	let () = if ty <> tty then
		fprintf std_formatter "@[ ==> Expected type was :@;<1 2>%a@]@."
			(Core_printer.print_term p.r_core_env) ty
		else ()
	in

	let () = print_newline () in

	{
		r_ast_env = Ast2core.env_scope_enter p.r_ast_env Ast.(s.s_name);
		r_core_env = Core.env_scope_enter_term p.r_core_env ty te;
	}

let run p sl : env_t =
	List.fold_left stmt_run p sl

