open Ast
open Utils

type ('a, 'b) env = ('a * 'b) list

type typing_error = string

exception Typing_error of typing_error

let bind env k v =
  if List.mem_assoc k env then
    let env = List.remove_assoc k env in
    (k, v) :: env
  else
    (k, v) :: env

let value env k = 
  try
    List.assoc k env
  with Not_found -> raise (Typing_error ("Unbound value " ^ k))

let rec type_equal ty1 ty2 =
  match ty1, ty2 with
  | TyInt, TyInt
  | TyBool, TyBool -> true
  | TyVar v1, TyVar v2 -> true (* (String.compare v1 v2) = 0 *)
  | TyArrow(tya1, tya2), TyArrow(tyb1, tyb2)
  | TyTimes(tya1, tya2), TyTimes(tyb1, tyb2) ->
    (type_equal tya1 tyb1) && (type_equal tya2 tyb2)
  | _ -> false


let print_error fmt (err : typing_error) =
  Format.fprintf fmt "%s" err;
  ()

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
  | App ((_, expr1), (_, expr2)) as app ->
    let env1, ty1 = type_of_expr env expr1 in
    let env2, ty2 = type_of_expr env expr2 in
    begin
      match ty1 with
      | TyArrow(tya1, tya2) ->
        if type_equal tya1 ty2 then (env, tya2)
        else raise (Typing_error ("Application t1 t2"))
      | TyVar _ ->
        begin
          match expr1 with
          | Var var ->
            let new_env = bind env var (TyArrow ((TyVar "'b"), (TyVar "'c"))) in
            type_of_expr new_env app
          | _ -> failwith "Should not happend"
        end
      | _ -> raise (Typing_error "Application")
    end
  | Lam (var,  _,  (loc, expr)) ->
    let new_env = bind env var (TyVar "'a") in
    let new_env, tyB = type_of_expr new_env expr in
    let tyA = value new_env var in
    (new_env, TyArrow(tyA, tyB))
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
        else raise (Typing_error "Fixpoint has type A->B with A != B")
      | _ -> raise (Typing_error "Fixpoint type should be of the form A->A")
    end
  | Int i -> env, TyInt
  | Bool b -> env, TyBool
  | Proj(Left ((_, expr))) -> type_of_proj env expr true
  | Proj(Right ((_, expr))) -> type_of_proj env expr false
  | Ite ((_, expr1), (_, expr2), (_, expr3)) ->
    begin
      match type_of_expr env expr1, type_of_expr env expr2, type_of_expr env expr3 with 
      | ((_, TyBool), (_, ty1), (_, ty2)) ->
        if ty1 = ty2 then (env, ty1)
        else raise (Typing_error "In 'if b then u else v', type(u) sould be equal to type(v)")
      | _ -> raise (Typing_error "In 'if b then u else v' type(b) should be bool")
    end
  | Binop (binop, (_, expr1), (_, expr2))  -> 
    type_of_binop env binop expr1 expr2

and type_of_proj env expr is_left =
  match type_of_expr env expr with
  | (_, TyTimes(ty1, ty2)) ->
    if is_left then (env, ty1) else (env, ty2)
  | _ -> raise (Typing_error "proj")

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
          else raise (Typing_error "Int operation should be between two int")
        | TyBool, TyBool ->
          if List.mem op ['&'; '|']
          then (env, TyInt)
          else raise (Typing_error "Operation between two bool should be && or ||")
        | _ -> raise (Typing_error "Bool operation should be between two bool")
      end