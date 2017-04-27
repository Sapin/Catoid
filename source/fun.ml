
#use "core.ml";;

let env = env_empty ;;

let typ = T_Uni 0 ;;
let id = T_Abs (typ, T_Abs (T_Var 0, T_Var 0)) ;;

let nat  = T_Arr (typ, T_Arr (T_Var 0, T_Arr (T_Arr (T_Var 1, T_Var 2), T_Var 2))) ;;
let zero = T_Abs (typ, T_Abs (T_Var 0, T_Abs (T_Arr (T_Var 1, T_Var 2), T_Var 1))) ;;

(* n T x f *)
let succ = T_Abs (nat, T_Abs (typ, T_Abs (T_Var 0, T_Abs (T_Arr (T_Var 1, T_Var 2),
	(* f (((n T) x) f)  *)
	T_App (T_Var 0, T_App (T_App (T_App (T_Var 3, T_Var 2), T_Var 1), T_Var 0))
)))) ;;

let eq =
(*
	\X:^. \x:X. \y:X.
		(C:^) -> (f:X->C) ->
			(f x) -> (f y)
*)
	T_Abs (typ, T_Abs (T_Var 0, T_Abs (T_Var 1,
		T_Arr (typ, T_Arr (T_Arr (T_Var 3, T_Var 0),
			T_Arr (T_App (T_Var 0, T_Var 3), T_App (T_Var 1, T_Var 3))
		))
	)))
;;

let eq_refl =
(*
	:: (X:^) -> (x:X) -> (eq X x x)
	\X:^. \x:X.
		\C:^. \f:X->C.
			id (X->C)
*)
	T_Abs (typ, T_Abs (T_Var 0,
		T_Abs (typ, T_Abs (T_Arr (T_Var 2, T_Var 1),
			T_App (id, T_Arr (T_Var 3, T_Var 2))
		))
	))
;;

