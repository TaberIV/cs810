open Bindlib
open Ast

type 'a error = OK of 'a | Error of string

module Ctx = Map.Make(String)

let rec synthesize : texpr Ctx.t -> term -> texpr error = fun gamma m ->
  match m with
  | CstTrue | CstFalse -> OK BoolType
  | Var var -> (try OK(Ctx.find (name_of var) gamma) 
                with Not_found -> Error "Variable not found")
  | App (func, arg) -> (
      match synthesize gamma func with
      | OK(FuncType(inType, retType)) -> (
          match check gamma arg inType with
          | OK(argType) when argType=inType -> OK(retType)
          | err -> err
        )
      | OK(_) -> Error "cannot apply non-function"
      | err -> err
    )
  | TDecl (term, texper) -> check gamma term texper
  | _ -> Error "Synthesis Error"
and check : texpr Ctx.t -> term -> texpr -> texpr error = fun gamma m sigma ->
  match m with
  | ITE (cond, tterm, fterm) -> (
      match check gamma cond BoolType with
      | OK(BoolType) -> (
          match synthesize gamma fterm with
          | OK(resType) -> check gamma tterm resType
          | err -> err
        )
      | OK(_) -> Error "Expected bool in if conditional"
      | err -> err
    )
  | Abs bind ->
    let (x, body) = unbind bind
    in (
      match sigma with 
      | FuncType(d, r) -> (
          match check (Ctx.add (name_of x) d gamma) body r with
          | OK t1 when t1=r -> OK(FuncType(d, r))
          | OK _ -> Error "Bad arg type"
          | err -> err
        )
      | _ -> Error "Cannot apply non-function"
    )
  | _ -> match synthesize gamma m with 
    | OK s when s=sigma -> OK sigma
    | OK _ -> Error "type mismatch"
    | err -> err

let tc : term -> texpr error = synthesize Ctx.empty
