open Ast
open Utils

type ('a, 'b) env = ('a * 'b) list

type typing_error = string

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

(* TODO z -> aa *)
let next_var_type () = "'" ^ (fresh_var ())

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

let string_of_expr = function
  | Var   _ -> "var"
  | App   (_, _) -> "app"
  | Lam   (_,_,_) -> "lam"
  | Pair  (_,_) -> "pair"
  | LetIn (_) -> "letIn"
  | Fix   _ -> "fix"
  | Int   _ -> "int"
  | Bool  _ -> "bool"
  | Proj  _ -> "proj"
  | Ite  _ -> "ite"
  | Binop _ -> "binop"
  | Unit -> "unit"


let print_env env =
  Printf.printf "=== ENV ===\n";
  List.iter (fun e ->
      let var, ty = e in
      Printf.printf "%s : %s\n" var (string_of_type ty)) env;
  Printf.printf "===========\n"

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

(* TODO factoriser *)
let rec unify_and_type_equal env ty1 ty2 =
  match ty1, ty2 with
  | TyVar v1, TyVar v2 -> replace_type_var env v2 (TyVar v1), Some(TyVar v1)
  | TyVar v1, _ -> replace_type_var env v1 ty2, Some(ty2)
  | _, TyVar v2 -> replace_type_var env v2 ty1, Some(ty1)
  | TyInt, TyInt -> env, Some TyInt
  | TyBool, TyBool -> env, Some TyBool
  | TyArrow(tya1, tya2), TyArrow(tyb1, tyb2) ->
    let n_env, t1_opt = unify_and_type_equal env tya1 tyb1 in
    let n_env, t2_opt = unify_and_type_equal n_env tya2 tyb2 in
    begin
      match t1_opt, t2_opt with
      | Some t1, Some t2 ->
        n_env, Some (TyArrow(t1, t2))
      | _ -> env, None
    end
  | TyTimes(tya1, tya2), TyTimes(tyb1, tyb2) ->
    let n_env, t1_opt = unify_and_type_equal env tya1 tyb1 in
    let n_env, t2_opt = unify_and_type_equal n_env tya2 tyb2 in
    begin
      match t1_opt, t2_opt with
      | Some t1, Some t2 ->
        n_env, Some (TyTimes(t1, t2))
      | _ -> env, None
    end
  | _ -> (env, None)

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
    let env, ty = type_of_expr env expr in
    (bind env var ty, (loc, Let(var, Some ty, (loc, expr))))
  | LetRec (var, ty_opt, (loc, expr)) ->
    let env, ty = type_of_expr (bind env var (TyVar(next_var_type()))) expr in
    let env = bind env var ty in
    (env, (loc, LetRec (var, Some ty, (loc, expr))))

and type_of_expr env expr =
  match expr with
  | Var var -> (env, value env var)
  | App ((_, expr1), (_, expr2)) ->
    let env, ty1 = type_of_expr env expr1 in
    let env, ty2 = type_of_expr env expr2 in
    begin
      match ty1 with
      | TyArrow(tya1, tya2) ->
        begin
          match unify_and_type_equal env tya1 ty2 with
          | env, Some ty -> (env, tya2)
          | env, None ->
            raise (Typing_error
                     ("Application " ^ (string_of_type tya1) ^ " != " ^ (string_of_type ty2)))
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
                     ("Application " ^ (string_of_type tya1) ^ " != " ^ (string_of_type ty2)))  
        end
      | _ -> raise (Typing_error ("Application " ^ (string_of_type ty1)))
    end
  | Lam (var, ty_opt, (loc, expr)) ->
    let env =
      begin
        match ty_opt with
        | Some(ty) -> bind env var ty
        | None -> bind env var (TyVar (next_var_type()))
      end
    in
    let env, tyB = type_of_expr env expr in
    let tyA = value env var in
    (env, TyArrow(tyA, tyB))
  | Pair((_, expr1), (_, expr2)) ->
    let env, ty1 = type_of_expr env expr1 in
    let env, ty2 = type_of_expr env expr2 in
    (env, TyTimes(ty1, ty2))
  | LetIn (var, (_, expr1), (_, expr2)) ->
    let env, ty1 = type_of_expr env expr1 in
    let env, ty2 = type_of_expr (bind env var ty1) expr2 in
    (env, ty2)
  | Fix (_, expr) ->
    begin
      let env, ty = type_of_expr env expr in
      match ty with
      | TyArrow(tya1, tya2) ->
        begin 
          match unify_and_type_equal env tya1 tya2 with
          | env, Some ty -> env, ty
          | _ -> raise (Typing_error "Fixpoint has type A->B with A != B")
        end
      | TyVar v ->
        raise (Typing_error "VAR")
      | TyTimes(tyt1, tyt2) ->
        begin 
          match expr with
          | Pair((loc, expr1), (_, expr2)) ->
            type_of_expr env (Fix (loc, expr1))
          | _ -> failwith "Should never happed Typing FIX PAIR"
        end
      | t -> raise (Typing_error ("Fixpoint type should be of the form A->A | " ^ (string_of_type (t))))
    end
  | Int i -> env, TyInt
  | Bool b -> env, TyBool
  | Proj(Left ((_, expr))) as p -> type_of_proj env expr p true
  | Proj(Right ((_, expr))) as p -> type_of_proj env expr p false
  | Ite ((_, expr1), (_, expr2), (_, expr3)) ->
    let (env, ty1) = type_of_expr env expr1 in
    let (env, ty2) = type_of_expr env expr2 in
    let (env, ty3) = type_of_expr env expr3 in
    begin
      match ty1, ty2, ty3 with
      | TyBool, _, _ ->
        begin 
          match unify_and_type_equal env ty2 ty3 with
          | env, Some ty -> env, ty
          | _ -> raise (Typing_error "In 'if b then u else v', type(u) sould be equal to type(v)")
        end
      | TyVar v, _, _ ->
        let env = replace_type_var env v TyBool in
        begin
          match unify_and_type_equal env ty2 ty3 with
          | env, Some ty -> env, ty
          | _ -> raise (Typing_error "In 'if b then u else v', type(u) sould be equal to type(v)")
        end
      | _, _, _ -> raise (Typing_error ("In 'if b then u else v' type(b) = " ^ (string_of_type ty1)))
    end
  | Binop (binop, (_, expr1), (_, expr2)) ->
    type_of_binop env binop expr1 expr2
  | Unit -> env, TyUnit

and type_of_proj env expr p is_left =
  let env, ty = type_of_expr env expr in
  match ty with
  | TyTimes(ty1, ty2) ->
    if is_left then (env, ty1) else (env, ty2)
  | TyVar v ->
    let ty1 = TyVar (next_var_type()) in
    let ty2 = TyVar (next_var_type()) in
    let env = replace_type_var env v (TyTimes(ty1, ty2)) in
    if is_left then (env, ty1) else (env, ty2)
  | _ -> raise (Typing_error ("Projection (" ^ (if is_left then "fst" else "snd") ^ ") on something not a couple"))

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
          else raise (Typing_error "Int operation should be between two int")
        | TyBool ->
          if List.mem op ['&'; '|']
          then (env, TyInt)
          else raise (Typing_error "Operation between two bool should be && or ||")
        | _ -> raise (Typing_error "Operation on something not a bool or int")
      end
  | _ -> 
    raise (Typing_error ("Operation " ^ (String.make 1 op) ^ " between type " ^
                         (string_of_type ty1) ^ " and " ^ (string_of_type ty2)))
