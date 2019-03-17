open Unification
open Subs
open Ast

type 'a error = OK of 'a | Error of string

type typing_judgement = subst*expr*texpr


let rec infer' (e:expr) (n:int): (int*typing_judgement) error =
  match e with
  | Unit  -> OK(n, (create (), e, UnitType))
  | Int n1 -> OK(n, (create (), e, IntType))
  | Var s -> 
    let sub = (create ())
    and ft = VarType ("_V" ^ string_of_int (n)) in
    extend sub s ft;
    OK(n + 1, (sub, e, ft))
  | Add(e1,e2)  -> 
    (match infer' e1 n with
     | OK(n1, (tc1, e_ret1, t1)) -> 
       (match infer' e2 n1 with
        | OK(n2, (tc2, e_ret2, t2)) -> 
          (match mgu @@ (t1, IntType)::(t2, IntType)::(check_all [tc1;tc2]) with
           | UOk sub -> OK(n2, (join @@ apply_to_envs sub [tc1;tc2], 
                                apply_to_expr sub @@ Add(e_ret1, e_ret2),
                                IntType))
           | UError (t1, t2) -> Error ("cannot unify " ^ (string_of_texpr t1) ^ " and " ^ (string_of_texpr t2)))
        | err -> err)
     | err -> err)
  | Sub(e1, e2) ->
    (match infer' e1 n with
     | OK(n1, (tc1, e_ret1, t1)) -> 
       (match infer' e2 n1 with
        | OK(n2, (tc2, e_ret2, t2)) -> 
          (match mgu @@ (t1, IntType)::(t2, IntType)::(check_all [tc1;tc2]) with
           | UOk sub -> OK(n2, (join @@ apply_to_envs sub [tc1;tc2],
                                apply_to_expr sub @@ Sub(e_ret1, e_ret2),
                                IntType))
           | UError (t1, t2) -> Error ("cannot unify " ^ (string_of_texpr t1) ^ " and " ^ (string_of_texpr t2)))
        | err -> err)
     | err -> err)
  | Mul(e1, e2) ->
    (match infer' e1 n with
     | OK(n1, (tc1, e_ret1, t1)) -> 
       (match infer' e2 n1 with
        | OK(n2, (tc2, e_ret2, t2)) -> 
          (match mgu @@ (t1, IntType)::(t2, IntType)::(check_all [tc1;tc2]) with
           | UOk sub -> OK(n2, (join @@ apply_to_envs sub [tc1;tc2],
                                apply_to_expr sub @@ Mul(e_ret1, e_ret2),
                                IntType))
           | UError (t1, t2) -> Error ("cannot unify " ^ (string_of_texpr t1) ^ " and " ^ (string_of_texpr t2)))
        | err -> err)
     | err -> err)
  | Div(e1, e2) ->
    (match infer' e1 n with
     | OK(n1, (tc1, e_ret1, t1)) -> 
       (match infer' e2 n1 with
        | OK(n2, (tc2, e_ret2, t2)) -> 
          (match mgu @@ (t1, IntType)::(t2, IntType)::(check_all [tc1;tc2]) with
           | UOk sub -> OK(n2, (join @@ apply_to_envs sub [tc1;tc2],
                                apply_to_expr sub @@ Mul(e_ret1, e_ret2),
                                IntType))
           | UError (t1, t2) -> Error ("cannot unify " ^ (string_of_texpr t1) ^ " and " ^ (string_of_texpr t2)))
        | err -> err)
     | err -> err)
  | NewRef(e1) -> 
    (match infer' e1 n with
     | OK(n1, (tc1, e_ret, t1)) -> OK(n1, (tc1, NewRef(e_ret), RefType t1))
     | err -> err)
  | DeRef(e1) -> 
    (match infer' e1 n with
     | OK(n1, (tc1, e_ret, t1)) -> 
       let ft = VarType("_V" ^ string_of_int n1) in
       (match mgu [(t1, RefType ft)] with
        | UOk sub -> OK(n1 + 1, (join @@ apply_to_envs sub [tc1],
                                 apply_to_expr sub @@ DeRef(e_ret),
                                 apply_to_texpr sub ft))
        | UError (t1, t2) -> Error ("cannot unify " ^ (string_of_texpr t1) ^ " and " ^ (string_of_texpr t2)))
     | err -> err)
  | SetRef(e1,e2) -> 
    (match infer' e1 n with
     | OK(n1, (tc1, e_ret1, t1)) -> 
       (match infer' e2 n1 with 
        | OK(n2, (tc2, e_ret2, t2)) -> 
          (match mgu @@ (t1, RefType t2)::(check_all [tc1;tc2]) with
           | UOk sub -> OK(n2, (join @@ apply_to_envs sub [tc1;tc2],
                                apply_to_expr sub @@ SetRef(e_ret1, e_ret2), 
                                UnitType))
           | UError (t3, t4) -> Error ("cannot unify " ^ (string_of_texpr @@ t3) ^ " and " ^ (string_of_texpr t4)))
        | err -> err)
     | err -> err)
  | Let(x,def,body) -> 
    (match infer' def n with
     | OK(n1, (tc1, e_ret1, t1)) ->
       (match infer' body n1 with
        | OK(n2, (tc2, e_ret2, t2)) -> 
          let copy_tc1 = Hashtbl.copy tc1
          and pair = (match lookup tc2 x with
              | None -> []
              | Some (tx) -> [(tx, t1)]) in
          remove_all [copy_tc1] x;
          (match mgu @@ pair@check_all [copy_tc1;tc2] with
           | UOk sub -> 
             remove_all [tc2] x;
             OK(n2, (join @@ apply_to_envs sub [tc1;tc2], 
                     apply_to_expr sub @@ Let(x, e_ret1, e_ret2), 
                     apply_to_texpr sub t2))
           | UError (t1, t2) -> Error ("cannot unify " ^ (string_of_texpr t1) ^ " and " ^ (string_of_texpr t2)))
        | err -> err)
     | err -> err)
  | Proc(x,t,body) -> 
    (match infer' (ProcUntyped(x, body)) n with
     | OK(n1, (tc1, e_ret1, FuncType(t1, t2))) ->
       (match mgu [(t1, t)] with
        | UOk sub -> OK(n1, (join @@ apply_to_envs sub [tc1], 
                             apply_to_expr sub e_ret1,
                             apply_to_texpr sub @@ FuncType(t1, t2)))
        | UError (t1, t2) -> Error ("cannot unify " ^ (string_of_texpr t1) ^ " and " ^ (string_of_texpr t2)))
     | err -> err)
  | ProcUntyped(x,body) -> 
    (match infer' (Var x) n with
     | OK(n1, (tc1, e_ret1, t1)) ->
       (match infer' body n1 with
        | OK(n2, (tc2, e_ret2, t2)) ->
          (match mgu @@ check_all [tc1;tc2] with
           | UOk sub -> 
             remove tc2 x;
             OK(n2, 
                (join @@ apply_to_envs sub [tc2],
                 apply_to_expr sub @@ Proc(x, t1, e_ret2), 
                 apply_to_texpr sub @@ FuncType(t1, t2)))
           | UError (t1, t2) -> Error ("cannot unify " ^ (string_of_texpr t1) ^ " and " ^ (string_of_texpr t2)))
        | err -> err)
     | err -> err)
  | App(e1,e2) -> 
    (match infer' e1 n with
     | OK(n1, (tc1, e_ret1, t1)) -> 
       (match infer' e2 n1 with
        | OK(n2, (tc2, e_ret2, t2)) -> 
          let ft = VarType("_V" ^ string_of_int n2) in
          (match mgu @@ (t1, FuncType(t2, ft))::(check_all [tc1;tc2]) with
           | UOk sub -> OK(n2 + 1,
                           (join @@ apply_to_envs sub [tc1;tc2], 
                            apply_to_expr sub @@ App(e_ret1, e_ret2),
                            apply_to_texpr sub ft))
           | UError (t3, t4) -> Error ("cannot unify " ^ (string_of_texpr t3) ^ " and " ^ (string_of_texpr t4)))
        | err -> err)
     | err -> err)
  | IsZero(e1) -> 
    (match infer' e1 n with 
     | OK(n1, (tc1, e_ret, t1)) -> 
       (match mgu [(t1, IntType)] with
        | UOk sub -> OK(n1, 
                        (join @@ apply_to_envs sub [tc1], 
                         apply_to_expr sub @@ IsZero(e_ret), 
                         BoolType))
        | UError (t2, t3) -> Error ("cannot unify " ^ (string_of_texpr t2) ^ " and " ^ (string_of_texpr t3)))
     | err -> err)
  | ITE(e1,e2,e3) -> 
    (match infer' e1 n with
     | OK(n1, (tc1, e_ret1, t1)) -> 
       (match infer' e2 n1 with
        | OK(n2, (tc2, e_ret2, t2)) -> 
          (match infer' e3 n2 with
           | OK(n3, (tc3, e_ret3, t3)) ->
             (match mgu @@ (t1, BoolType)::(t3, t2)::(check_all [tc1;tc2;tc3]) with
              | UOk sub -> OK(n3,
                              (join @@ apply_to_envs sub [tc1;tc2;tc3],
                               apply_to_expr sub @@ ITE(e_ret1, e_ret2, e_ret3), 
                               apply_to_texpr sub t2))
              | UError (t3, t4) -> Error ("cannot unify " ^ (string_of_texpr t3) ^ " and " ^ (string_of_texpr t4)))
           | err -> err)
        | err -> err)
     | err -> err)
  | Letrec(tRes,x,param,tPara,def,body) -> 
    (match infer' (LetrecUntyped(x, param, def, body)) n with
     | OK(n1, (tc1, (Letrec(tRes2,x2, param2, tPara2, def2, body2)), t1)) -> 
       (match mgu [(tRes2, tRes);(tPara2, tPara)] with
        | UOk sub -> OK(n1, 
                        (join @@ apply_to_envs sub [tc1], 
                         apply_to_expr sub @@ Letrec(tRes2, x2, param2, tPara2, def2, body2),
                         apply_to_texpr sub t1))
        | UError (t3, t4) -> Error ("cannot unify " ^ (string_of_texpr t3) ^ " and " ^ (string_of_texpr t4)))
     | err -> err)
  | LetrecUntyped(f,x,def,body) -> 
    (match infer' def n with
     | OK(n1, (tc1, e_ret1, t1)) -> 
       (match infer' body n1 with
        | OK(n2, (tc2, e_ret2, t2)) -> 
          (match infer' (Var(f)) n2 with
           | OK(n3, (tc3, _, t3)) ->  
             let ft = (match lookup tc1 x with
                 | None -> VarType("_V" ^ string_of_int @@ n3)
                 | Some(tx) -> tx) in remove tc1 x;
             let pairs = (match lookup tc1 f, lookup tc2 f with
                 | (None, None) -> []
                 | (Some tx, None) | (None, Some tx) -> [(tx, t3)]
                 | (Some tx, Some ty) -> [(tx, t3);(ty, t3)]) in
             (match mgu @@ pairs@(t3, FuncType(ft, t1))::(check_all [tc1;tc2;tc3]) with
              | UOk sub -> 
                remove_all [tc1;tc2] f;
                remove_all [tc1] x;
                OK(n3 + 1, 
                   (join @@ apply_to_envs sub [tc1;tc2], 
                    apply_to_expr sub @@ Letrec(t1, f, x, ft, e_ret1, e_ret2),
                    apply_to_texpr sub t2))
              | UError (t3, t4) -> Error ("cannot unify " ^ (string_of_texpr t3) ^ " and " ^ (string_of_texpr t4)))
           | err -> err)
        | err -> err)
     | err -> err)
  | BeginEnd(es) -> 
    let checkList = 
      List.map 
        (fun x -> 
           (match infer' x n with
            | OK(n, (tc1, e_ret1, t1)) -> OK(n, (tc1, e_ret1, t1))
            | Error(s) -> Error(s))) es in
    let filteredErrors = 
      List.filter
        (function
          | OK(_, (_, _, _)) -> false
          | Error(_) -> true) checkList in
    if (List.length filteredErrors) > 0
    then List.hd filteredErrors
    else 
      let all_subs =
        (List.map
           (function
             | OK(_, (tc, _, _)) -> tc
             | _ -> failwith "impossible") checkList)
      and end_ty = 
        (match (List.nth checkList (List.length checkList - 1)) with
         | OK(_, (_, _, t_end)) -> t_end
         | _ -> failwith "impossible") in
      (match mgu @@ check_all all_subs with
       | UOk sub -> OK(n, 
                       (join @@ apply_to_envs sub all_subs, 
                        apply_to_expr sub @@ BeginEnd(es), 
                        apply_to_texpr sub end_ty))
       | UError (t3, t4) -> Error ("cannot unify " ^ (string_of_texpr t3) ^ " and " ^ (string_of_texpr t4)))


let string_of_typing_judgement (s,e,t) =
  "\027[31m"^string_of_subs s^"\027[37m ⊢ \027[34m"^string_of_expr e
  ^": \027[35m "^string_of_texpr t 

let infer_type (AProg e) =
  match infer' e 0 with
  | OK (_, tj) -> string_of_typing_judgement tj
  | Error s -> "Error! "^ s

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(* Interpret an expression *)
let inf (e:string) : string =
  e |> parse |> infer_type 

let test (n:int) : string =
  Examples.expr n |> parse |> infer_type