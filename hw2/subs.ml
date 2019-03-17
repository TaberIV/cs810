open Ast

type subst = (string,Ast.texpr) Hashtbl.t

let create () = Hashtbl.create 15

let extend (s : subst) var typeExp = Hashtbl.add s var typeExp

let remove (s : subst) var = Hashtbl.remove s var

let replace (s : subst) var typeExp = Hashtbl.replace s var typeExp

let remove_all (subs: subst list) (x: string):unit =
  List.iter (fun sub -> remove sub x) subs

let lookup (s : subst) var = Hashtbl.find_opt s var

let rec apply_to_texpr s = function
  | VarType var -> (
      match lookup s var with
      | None -> VarType var
      | Some typeExp -> typeExp
    )
  | FuncType (typeExp1, typeExp2) -> FuncType (
      apply_to_texpr s typeExp1, 
      apply_to_texpr s typeExp2
    )
  | RefType typeExp -> RefType (apply_to_texpr s typeExp)
  | typeExp -> typeExp

let rec apply_to_expr s = function
  | Add (exp1, exp2) -> App (apply_to_expr s exp1, apply_to_expr s exp2)
  | Sub (exp1, exp2) -> Sub (apply_to_expr s exp1, apply_to_expr s exp2)
  | Mul (exp1, exp2) -> Mul (apply_to_expr s exp1, apply_to_expr s exp2)
  | Div (exp1, exp2) -> Div (apply_to_expr s exp1, apply_to_expr s exp2)
  | Let (var, exp1, exp2) -> Let (var, apply_to_expr s exp1, apply_to_expr s exp2)
  | IsZero exp -> IsZero (apply_to_expr s exp)
  | ITE (exp1, exp2, exp3) -> ITE (
      apply_to_expr s exp1,
      apply_to_expr s exp2,
      apply_to_expr s exp3
    )
  | Proc (var, typeExp, exp) -> Proc (var, apply_to_texpr s typeExp, apply_to_expr s exp)
  | ProcUntyped (var, exp) -> ProcUntyped (var, apply_to_expr s exp)
  | App (exp1, exp2) -> App (apply_to_expr s exp1, apply_to_expr s exp2)
  | Letrec (typeExp1, var1, var2, typeExp2, exp1, exp2) -> 
    Letrec (
      apply_to_texpr s typeExp1,
      var1, var2,
      apply_to_texpr s typeExp2,
      apply_to_expr s exp1,
      apply_to_expr s exp2
    )
  | BeginEnd l -> BeginEnd (List.map (apply_to_expr s) l)
  | NewRef exp -> NewRef (apply_to_expr s exp)
  | DeRef exp -> DeRef (apply_to_expr s exp)
  | SetRef (exp1, exp2) -> SetRef (apply_to_expr s exp1, apply_to_expr s exp2)
  | exp -> exp

let apply_to_env (s1:subst) (s2:subst) = 
  Hashtbl.iter (fun var t1 -> replace s2 var @@ apply_to_texpr s1 t1) s2

let apply_to_envs (s1:subst) (lst:subst list):subst list =
  List.iter (fun x -> apply_to_env s1 x) lst; lst

let check_two (sub1:subst) (sub2:subst):((texpr*texpr) list) = 
  (Hashtbl.fold
     (fun x t3 acc -> 
        match lookup sub2 x with
        | None -> acc
        | Some t4 -> (t3,t4)::acc) sub1 [])

let rec check_all (subs:subst list):((texpr*texpr) list) = 
  match subs with
  | hd1::tl -> 
    List.append
      (List.flatten 
         (List.map
            (fun x -> check_two x hd1) tl)) @@ check_all tl
  | _ -> []


let string_of_subs t =
  match Hashtbl.fold (fun var typeExp acc -> var ^ ":= " ^ string_of_texpr typeExp ^ ",") t "" with
  | "" -> "empty"
  | x -> x

let domain (s: subst) = 
  Hashtbl.fold(fun var typeExp acc -> (var)::acc) s []

let merge s1 s2 = 
  Hashtbl.iter (fun var t1 -> 
      match lookup s2 var with
      | None -> extend s2 var t1
      | Some (t2) -> replace s2 var @@ apply_to_texpr s1 t2) s1;
  s2

let join (lst:subst list):subst =
  List.fold_right (fun x y -> merge x y) lst (create ()) 