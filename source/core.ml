
type term_t =
| T_Var of int
| T_Uni of int
| T_Abs of term_t * term_t
| T_Arr of term_t * term_t
| T_App of term_t * term_t

exception TypeMismatch

let term_scope_enter_k term c : term_t =
	let rec aux t k = match t with
	| T_Var i when i >= k -> T_Var (i+c)
	| T_Var i -> T_Var i
	| T_Uni i -> T_Uni i
	| T_Abs (a, b) -> T_Abs (aux a k, aux b (k+1))
	| T_Arr (a, b) -> T_Arr (aux a k, aux b (k+1))
	| T_App (a, b) -> T_App (aux a k, aux b k)
	in
	aux term 0

let term_scope_enter term : term_t =
	term_scope_enter_k term 1

let term_scope_leave term subst : term_t =
	let rec aux t k s = match t with
	| T_Var i when i > k -> T_Var (i-1)
	| T_Var i when i = k -> s
	| T_Var i -> T_Var i
	| T_Uni i -> T_Uni i
	| T_Abs (a, b) -> T_Abs (aux a k s, aux b (k+1) (term_scope_enter s))
	| T_Arr (a, b) -> T_Arr (aux a k s, aux b (k+1) (term_scope_enter s))
	| T_App (a, b) -> T_App (aux a k s, aux b k s)
	in
	aux term 0 subst

module IMap = Map.Make (struct type t = int let compare = compare end)

type env_t = {
	e_var_type : term_t IMap.t;
	e_var_term : (term_t option) IMap.t;
	e_scope : int;
}

let env_empty : env_t = {
	e_var_type = IMap.empty;
	e_var_term = IMap.empty;
	e_scope = 0;
}

let env_var_type p i : term_t =
	let t = IMap.find (p.e_scope - i - 1) p.e_var_type in
	term_scope_enter_k t (i + 1)

let env_var_term p i : term_t option =
	let t = IMap.find (p.e_scope - i - 1) p.e_var_term in
	match t with
	| Some t -> Some (term_scope_enter_k t (i + 1))
	| None -> None

let env_scope_enter p t : env_t = {
	e_var_type = IMap.add p.e_scope t p.e_var_type;
	e_var_term = IMap.add p.e_scope None p.e_var_term;
	e_scope = p.e_scope + 1;
}

let env_scope_enter_term p ty t : env_t = {
	e_var_type = IMap.add p.e_scope ty p.e_var_type;
	e_var_term = IMap.add p.e_scope (Some t) p.e_var_term;
	e_scope = p.e_scope + 1;
}

let rec term_reduce p t : term_t =
	match t with
	| T_Var i ->
		begin match env_var_term p i with
		| Some t -> term_reduce p t
		| None -> T_Var i
		end
	| T_Uni i ->
		T_Uni i
	| T_Abs (a, b) ->
		let a = term_reduce p a in
		let p = env_scope_enter p a in
		let b = term_reduce p b in
		T_Abs (a, b)
	| T_Arr (a, b) ->
		let a = term_reduce p a in
		let p = env_scope_enter p a in
		let b = term_reduce p b in
		T_Arr (a, b)
	| T_App (a, b) ->
		let a = term_reduce p a in
		let b = term_reduce p b in
		let ay = term_type p a in
		let by = term_type p b in
		let () = begin match ay with
		| T_Arr (x, _) when x = by -> ()
		| _ -> (raise TypeMismatch)
		end in
		begin match a with
		| T_Abs (_, c) -> term_reduce p (term_scope_leave c b)
		| _ -> T_App (a, b)
		end

and term_type p t : term_t =
	match t with
	| T_Var i ->
		env_var_type p i
	| T_Uni i ->
		T_Uni (i+1)
	| T_Abs (a, b) ->
		let p = env_scope_enter p a in
		T_Arr (a, term_type p b)
	| T_Arr (_, _) ->
		T_Uni (type_kind p t)
	| T_App (a, b) ->
		let ay = term_type p a in
		let by = term_type p b in
		let cy = begin match ay with
		| T_Arr (x, cy) when x = by -> cy
		| _ -> (raise TypeMismatch)
		end in
		term_reduce p (term_scope_leave cy b)

and type_kind p t : int =
	let rec aux p t = match t with
	| T_Var i ->
		let (a, b) = (aux p (env_var_type p i)) in
		(a-1, b-1)
	| T_Uni i ->
		(i+1, i)
	| T_Abs (a, b) ->
		let p' = env_scope_enter p a in
		let (a, b) = (aux p (T_Arr (a, term_type p' b))) in
		(a-1, b-1)
	(* | T_Arr (T_Uni i, b) ->
		let p = env_scope_enter p (T_Uni i) in
		max i (type_kind p b)
	| T_Arr (a, b) ->
		let k = type_kind p a in
		let p = env_scope_enter p a in
		max k (type_kind p b) *)
	| T_Arr (a, b) ->
		let (a1, a2) = aux p a in
		let p = env_scope_enter p a in
		let (b1, b2) = aux p b in
		(max a2 b1, max a1 b2)
	| T_App (a, b) ->
		let (a, b) = (aux p (term_type p t)) in
		(a-1, b-1)
	in
	let (k,_) = aux p t in
	k

