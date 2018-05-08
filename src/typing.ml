open Ast
open Utils

type ('a, 'b) env = ('a * 'b) list

type typing_error = string * loc

exception Typing_error of typing_error

let letters =
  [|'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm';
    'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z'|]

let word_pointer = ref []

let fresh_var () : string =
  let inc_pointer =
    match !word_pointer with
    | [] -> word_pointer := [0];
    | n :: q ->
      if n = 25
      then
        let cont = ref true in
        word_pointer := 0 :: List.map (fun x ->
            if x = 25
            then if !cont then 0 else x
            else begin cont := false; x end) !word_pointer;
      else word_pointer := (n + 1) :: q
  in
  inc_pointer;
  List.fold_left (fun acc n -> acc ^ (String.make 1 (Array.get letters n))) ("") (!word_pointer)

let next_var_type () = "'" ^ (fresh_var ())

let bind env k v =
  if List.mem_assoc k env then
    let env = List.remove_assoc k env in
    (k, v) :: env
  else
    (k, v) :: env

let value env k loc =
  try
    List.assoc k env
  with Not_found -> raise (Typing_error ("Unbound value " ^ k, loc))

let rec string_of_type = function
  | TyInt   -> "int"
  | TyBool  -> "bool"
  | TyVar x -> x
  | TyArrow(tyl, tyr) -> (string_of_ty_wp tyl) ^ " -> " ^ (string_of_type tyr)
  | TyTimes(tyl, tyr) -> (string_of_ty_wp tyl) ^ " * " ^ (string_of_type tyr)
  | TyUnit -> "unit"
and string_of_ty_wp = function
  | TyInt | TyBool | TyVar _ | TyUnit as t -> string_of_type t
  | _ as ty -> "(" ^ (string_of_type ty) ^ ")"

let rec replace_type_var env x new_type =
  let rec replace_in_type ty x new_type =
    match ty with
    | TyVar v -> if (String.compare v x) = 0 then new_type else TyVar v
    | TyArrow(v1, v2) -> TyArrow(replace_in_type v1 x new_type, replace_in_type v2 x new_type)
    | TyTimes(v1, v2) -> TyTimes(replace_in_type v1 x new_type, replace_in_type v2 x new_type)
    | t -> t
  in
  List.map (fun entry ->
      let var, ty = entry in var, replace_in_type ty x new_type)
    env

let rec unify_and_type_equal env ty1 ty2 =
  match ty1, ty2 with
  | TyVar v1, TyVar v2 -> replace_type_var env v2 (TyVar v1), Some(TyVar v1)
  | TyVar v1, _ -> replace_type_var env v1 ty2, Some(ty2)
  | _, TyVar v2 -> replace_type_var env v2 ty1, Some(ty1)
  | TyInt, TyInt -> env, Some TyInt
  | TyBool, TyBool -> env, Some TyBool
  | TyArrow(tya1, tya2), TyArrow(tyb1, tyb2)
  | TyTimes(tya1, tya2), TyTimes(tyb1, tyb2) ->
    let is_arrow = match ty1 with TyArrow (_, _) -> true | _ -> false in
    let n_env, t1_opt = unify_and_type_equal env tya1 tyb1 in
    let n_env, t2_opt = unify_and_type_equal n_env tya2 tyb2 in
    begin
      match t1_opt, t2_opt with
      | Some t1, Some t2 ->
        n_env, Some (if is_arrow then TyArrow(t1, t2) else TyTimes(t1, t2))
      | _ -> env, None
    end
  | _ -> (env, None)

let print_error fmt (err : typing_error) =
  let str, loc = err in
  let x, y = pos_of_loc loc in
  Format.fprintf fmt "Line:%s, Charactere:%s, %s" (string_of_int x) (string_of_int y) str;
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
           let env, ast = env_ast in
           let env, typed_cmd = (typing_cmd env cmd) in
           (env, typed_cmd :: ast)
        )
        (env, new_ast) ast
    in
    Ok(List.rev new_ast)
  with Typing_error error -> Error(error)

and typing_cmd env cmd =
  match cmd with
  | Let (var, ty_opt, (loc, expr)) ->
    let env, ty = type_of_expr env (loc, expr) in
    (bind env var ty, (loc, Let(var, Some ty, (loc, expr))))
  | LetRec (var, ty_opt, (loc, expr)) ->
    let env, ty = type_of_expr (bind env var (TyVar(next_var_type()))) (loc, expr) in
    let env = bind env var ty in
    (env, (loc, LetRec (var, Some ty, (loc, expr))))

and type_of_expr env expr_l =
  match expr_l with
  | (loc, Var var) -> (env, value env var loc)
  | (loc, App (expr1_l, expr2_l)) ->
    let env, ty1 = type_of_expr env expr1_l in
    let env, ty2 = type_of_expr env expr2_l in
    begin
      match ty1 with
      | TyArrow(tya1, tya2) ->
        begin
          match unify_and_type_equal env tya1 ty2 with
          | env, Some ty -> (env, tya2)
          | env, None ->
            raise (Typing_error
                     (("Typing: Application " ^ (string_of_type tya1) ^ " != " ^ (string_of_type ty2)), 
                      loc))
        end
      | TyVar v ->
        let tya1 = TyVar (next_var_type()) in
        let tya2 = TyVar (next_var_type()) in
        let env = replace_type_var env v (TyArrow (tya1, tya2)) in
        begin
          match unify_and_type_equal env tya1 ty2 with
          | env, Some ty -> env, tya2
          | _ -> 
            raise (Typing_error
                     (("Typing: Application incompatible types : " ^ (string_of_type tya1) ^ " != " ^ (string_of_type ty2), 
                       loc)))  
        end
      | _ -> raise (Typing_error 
                      (("Typing: Application of something not a lambda : " ^ (string_of_type ty1)), loc))
    end
  | (loc, Lam (var, ty_opt, expr_l')) ->
    let env =
      begin
        match ty_opt with
        | Some(ty) -> bind env var ty
        | None -> bind env var (TyVar (next_var_type()))
      end
    in
    let env, tyB = type_of_expr env expr_l' in
    let tyA = value env var loc in
    (env, TyArrow(tyA, tyB))
  | (loc, Pair(expr1_l, expr2_l)) ->
    let env, ty1 = type_of_expr env expr1_l in
    let env, ty2 = type_of_expr env expr2_l in
    (env, TyTimes(ty1, ty2))
  | (loc, LetIn (var, expr1_l, expr2_l)) ->
    let env, ty1 = type_of_expr env expr1_l in
    let env, ty2 = type_of_expr (bind env var ty1) expr2_l in
    (env, ty2)
  | (loc, Fix (expr_l)) ->
    begin
      let env, ty = type_of_expr env expr_l in
      match ty with
      | TyArrow(tya1, tya2) ->
        begin
          match unify_and_type_equal env tya1 tya2 with
          | env, Some ty -> env, ty
          | _ -> raise (Typing_error ("Typing: Fixpoint has type A->B with A != B", loc))
        end
      | t -> raise (Typing_error ("Typing: Fixpoint type should be of the form A->A | " ^ (string_of_type (t)), loc)) 
    end
  | (loc, Int i) -> env, TyInt
  | (loc, Bool b) -> env, TyBool
  | (loc, Proj(Left (expr_l))) -> type_of_proj env expr_l true
  | (loc, Proj(Right (expr_l))) -> type_of_proj env expr_l false
  | (loc, Ite (expr1_l, expr2_l, expr3_l)) ->
    let (env, ty1) = type_of_expr env expr1_l in
    let (env, ty2) = type_of_expr env expr2_l in
    let (env, ty3) = type_of_expr env expr3_l in
    begin
      match ty1, ty2, ty3 with
      | TyBool, _, _ ->
        begin 
          match unify_and_type_equal env ty2 ty3 with
          | env, Some ty -> env, ty
          | _ -> raise (Typing_error ("Typing: In 'if b then u else v', type(u) sould be equal to type(v)", loc))
        end
      | TyVar v, _, _ ->
        let env = replace_type_var env v TyBool in
        begin
          match unify_and_type_equal env ty2 ty3 with
          | env, Some ty -> env, ty
          | _ -> raise (Typing_error ("Typing: In 'if b then u else v', type(u) sould be equal to type(v)", loc))
        end
      | _, _, _ -> raise (Typing_error ("Typing: In 'if b then u else v' type(b) = " ^ (string_of_type ty1), loc))
    end
  | (loc, Binop (binop, expr1_l, expr2_l)) ->
    type_of_binop env binop expr1_l expr2_l
  | (loc, Unit) -> env, TyUnit

and type_of_proj env expr is_left =
  let env, ty = type_of_expr env expr in
  let loc, _ = expr in
  match ty with
  | TyTimes(ty1, ty2) ->
    if is_left then (env, ty1) else (env, ty2)
  | TyVar v ->
    let ty1 = TyVar (next_var_type()) in
    let ty2 = TyVar (next_var_type()) in
    let env = replace_type_var env v (TyTimes(ty1, ty2)) in
    if is_left then (env, ty1) else (env, ty2)
  | _ -> raise (Typing_error
                  ("Typing: Projection (" ^ (if is_left then "fst" else "snd") ^ ") on something not a couple", loc))

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
  let loc, _ = expr1 in
  let env, ty1 = type_of_expr env expr1 in
  let env, ty2 = type_of_expr env expr2 in
  match unify_and_type_equal env ty1 ty2 with
  | env, Some ty ->
    if op = '=' then env, TyBool
    else
      begin
        match ty with
        | TyInt ->
          if List.mem op ['+'; '-'; '*'; '/'] then (env, TyInt)
          else if List.mem op  ['>'] then (env, TyBool)
          else raise (Typing_error ("Typing: Int operation should be between two int", loc))
        | TyBool ->
          if List.mem op ['&'; '|']
          then (env, TyInt)
          else raise (Typing_error ("Typing: Operation between two bool should be && or ||", loc))
        | _ -> raise (Typing_error ("Typing: Operation on something not a bool or int", loc))
      end
  | _ ->
    raise (Typing_error ("Typing: Operation " ^ (String.make 1 op) ^ " between type " ^
                         (string_of_type ty1) ^ " and " ^ (string_of_type ty2), loc))