(*****************************
* Unification of type terms *
*****************************)

open Ast

(* invariant for substitutions: no id on a lhs occurs in any term earlier  *)
(* in the list                                                             *)
type substitution = (id * typ) list

(* check if a variable occurs in a term *)
let rec occurs (x : id) (t : typ) : bool =
  match t with
  | TVar y -> x = y
  | Arrow (u, v) -> occurs x u || occurs x v

(* substitute term s for all occurrences of var x in term t *)
let rec subst (s : typ) (x : id) (t : typ) : typ =
  match t with
  | TVar y -> if x = y then s else t
  | Arrow (u, v) -> Arrow (subst s x u, subst s x v)

(* apply a substitution to t right to left *)
let apply (s : substitution) (t : typ) : typ =
  List.fold_right (fun (x, e) -> subst e x) s t

(* unify one pair *)
let rec unify_one (s : typ) (t : typ) : substitution =
  match (s, t) with
  | (TVar x, TVar y) -> if x = y then [] else [(x, t)]
  | (Arrow (x, y), Arrow (u, v)) -> unify [(x, u); (y, v)]
  | ((TVar x, (Arrow (u, v) as z)) | ((Arrow (u, v) as z), TVar x)) ->
      if occurs x z
      then failwith "not unifiable: circularity"
      else [(x, z)]

(* unify a list of pairs *)
and unify (s : (typ * typ) list) : substitution =
  match s with
  | [] -> []
  | (x, y) :: t ->
      let t2 = unify t in
      let t1 = unify_one (apply t2 x) (apply t2 y) in
      t1 @ t2

