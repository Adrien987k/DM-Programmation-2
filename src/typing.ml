open Ast
open Utils

type ('a, 'b) env = ('a * 'b) list

(* TODO do better *)
type typing_error = string

exception Typing_error of typing_error

let bind env k v =
  if List.mem_assoc k env then
    let env = List.remove_assoc k env in
    (k, v) :: env
  else
    (k, v) :: env

let value env k = List.assoc k env

let rec type_equal ty1 ty2 =
  match ty1, ty2 with
  | TyInt, TyInt
  | TyBool, TyBool
  | TyVar _, TyVar _ -> true
  | TyArrow(tya1, tya2), TyArrow(tyb1, tyb2)
  | TyTimes(tya1, tya2), TyTimes(tyb1, tyb2) ->
    (type_equal tya1 tyb1) && (type_equal tya2 tyb2)
  | _ -> false


let print_error fmt err = failwith "todo print error"

let rec typing_ast (ast : Ast.t) : (Ast.t, typing_error) Utils.result =
  let ast = List.map
      (fun cmd_l -> let _, cmd = cmd_l in cmd ) ast
  in
  let env = [] in
  let new_ast = [] in
  try
    let _, new_ast = List.fold_left
        (fun env_ast cmd ->
           let _, ast = env_ast in
           let new_env, typed_cmd = (typing_cmd env cmd) in
           (new_env, typed_cmd :: ast)
        )
        (env, new_ast) ast
    in
    Ok(new_ast)
  with Typing_error error -> Error(error)

and typing_cmd env cmd =
  match cmd with
  | Let (var, ty_opt, (loc, expr)) ->
    let new_env, ty = type_of_expr env expr in
    (bind new_env var ty, (loc, Let (var, Some ty, (loc, expr))))
  | LetRec (var, ty_opt, (loc, expr)) -> (* TODO Let Rec *)
    let new_env, ty = type_of_expr env expr in
    (bind new_env var ty, (loc, LetRec (var, Some ty, (loc, expr))))

and type_of_expr env expr =
  match expr with
  | Var var -> (env, value env var)
  | App ((_, expr1), (_, expr2)) ->
    let env1, ty1 = type_of_expr env expr1 in
    let env2, ty2 = type_of_expr env expr2 in
    begin
      match ty1 with
      | TyArrow(tya1, tya2) ->
        if tya1 = ty2 then (env, tya2)
        else raise (Typing_error "")
      | _ -> raise (Typing_error "")
    end
  | Lam (var,  ty_opt,  (_, expr)) ->
    (* let env1, ty1 = type_of_expr env expr in *)
    raise (Typing_error "TODO")
  | Pair ((_, expr1), (_, expr2)) ->
    let _, ty1 = type_of_expr env expr1 in
    let _, ty2 = type_of_expr env expr2 in
    (env, TyTimes(ty1, ty2))
  | LetIn (var, (_, expr1), (_, expr2)) ->
    let _, ty1 = type_of_expr env expr1 in
    let _, ty2 = type_of_expr (bind env var ty1) expr2 in
    (env, ty2)
  | Fix (_, expr) ->
    begin
      match type_of_expr env expr with
      | (_, TyArrow(tya1, tya2)) ->
        if tya1 = tya2 then (env, tya1)
        else raise (Typing_error "")
      | _ -> raise (Typing_error "")
    end
  | Int i -> env, TyInt
  | Bool b -> env, TyBool
  | Proj proj -> type_of_proj env proj
  | Ite ((_, expr1), (_, expr2), (_, expr3)) ->
    begin
      match type_of_expr env expr1, type_of_expr env expr2, type_of_expr env expr3 with 
      | ((_, TyBool), (_, ty1), (_, ty2)) ->
        if ty1 = ty2 then (env, ty1)
        else raise (Typing_error "")
      | _ -> raise (Typing_error "")
    end
  | Binop (binop, (_, expr1), (_, expr2))  -> 
    type_of_binop env binop expr1 expr2

(* TODO FACTORISER *)
and type_of_proj env (proj : Ast.expr Ast.loc Ast.either) =
  match proj with
  | Left ((_, expr)) ->
    begin
      match type_of_expr env expr with
      | (_, TyTimes(ty1, ty2)) -> (env, ty1)
      | _ -> raise (Typing_error "")
    end
  | Right ((_, expr)) ->
    begin
      match type_of_expr env expr with
      | (_, TyTimes(ty1, ty2)) -> (env, ty2) 
      | _ -> raise (Typing_error "")
    end

and type_of_binop env binop expr1 expr2 =
  let op =
    match binop with
    | Plus -> '+'
    | Minus -> '-'
    | Times -> '*'
    | Div -> '/'
    | And -> '&'
    | Or -> '|'
    | Eq -> '='
    | Gt -> '>'
  in
  match type_of_expr env expr1, type_of_expr env expr2 with
  | (_, ty1), (_, ty2) -> 
    if op = '=' && (type_equal ty1 ty2) then (env, TyBool)
    else if op = '=' then raise (Typing_error "Comparaison between two diffrents types")
    else 
      begin match ty1, ty2 with
        | TyInt, TyInt ->
          if List.mem op ['+'; '-'; '*'; '/'; '>']
          then (env, TyInt)
          else raise (Typing_error "")
        | TyBool, TyBool ->
          if List.mem op ['&'; '|']
          then (env, TyInt)
          else raise (Typing_error "")
        | _ -> raise (Typing_error "")
      end