open Ast
open Utils

type ('a, 'b) env = ('a * 'b) list

type typing_error = string

exception Typing_error of typing_error

let var_type_counter = ref (-1)

let var_types = 
  [|'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm';
    'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z'|]

(* TODO z -> aa *)
let next_var_type () =
  var_type_counter := (!var_type_counter + 1);
  (* Printf.printf "GET %s\n" (string_of_int (!var_type_counter)); *)
  try
    let c = Array.get var_types (!var_type_counter) in
    "'" ^ (String.make 1 c)
  with Invalid_argument _ ->
    var_type_counter := 0;
    let c = Array.get var_types (!var_type_counter) in
    "'" ^ (String.make 1 c)


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


let unify env ty1 ty2 =
  match ty1, ty2 with
  | TyVar v1, TyVar v2 -> env
  | TyVar v1, _ -> replace_type_var env v1 ty2
  | _, TyVar v2 -> replace_type_var env v2 ty1
  | _ -> env

let rec unify_and_type_equal env ty1 ty2 =
  match ty1, ty2 with
  | TyVar v1, TyVar v2 -> (replace_type_var env v2 (TyVar v1), true)
  | TyVar v1, _ -> (replace_type_var env v1 ty2, true)
  | _, TyVar v2 -> (replace_type_var env v2 ty1, true) (* (String.compare v1 v2) = 0 *)
  | TyInt, TyInt
  | TyBool, TyBool -> (env, true)
  | TyArrow(tya1, tya2), TyArrow(tyb1, tyb2)
  | TyTimes(tya1, tya2), TyTimes(tyb1, tyb2) ->
    let env, b1 = unify_and_type_equal env tya1 tyb1 in
    let env, b2 = unify_and_type_equal env tya2 tyb2 in
    (env, b1 && b2)
  | _ -> (env, false)

let rec string_of_type = function
  | TyInt   -> "int"
  | TyBool  -> "bool"
  | TyVar x -> x
  | TyArrow(tyl, tyr) -> (string_of_ty_wp tyl) ^ " -> " ^ (string_of_type tyr)
  | TyTimes(tyl, tyr) -> (string_of_ty_wp tyl) ^ " * " ^ (string_of_type tyr)
and string_of_ty_wp = function
  | TyInt | TyBool | TyVar _ as t -> string_of_type t
  | _ as ty -> "(" ^ (string_of_type ty) ^ ")"

let print_env env =
  Printf.printf "=== ENV ===\n";
  List.iter (fun e ->
      let var, ty = e in
      Printf.printf "%s : %s\n" var (string_of_type ty)) env;
  Printf.printf "===========\n"

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
    (bind env var ty, (loc, Let (var, Some ty, (loc, expr))))
  | LetRec (var, ty_opt, (loc, expr)) ->
    let env, ty = type_of_expr (bind env var (TyVar(next_var_type()))) expr in
    let env = bind env var ty in
    (env, (loc, LetRec (var, Some ty, (loc, expr))))

and type_of_expr env expr =
  (* print_env env; *)
  match expr with
  | Var var -> (env, value env var)
  | App ((_, expr1), (_, expr2)) as app ->
    let env, ty1 = type_of_expr env expr1 in
    let env, ty2 = type_of_expr env expr2 in
    begin
      match ty1 with
      | TyArrow(tya1, tya2) ->
        let env, equal = unify_and_type_equal env tya1 ty2 in
        Printf.printf "DEB %s\n" (string_of_type tya2);
        if equal then (env, tya2)
        else raise (Typing_error
                      ("Application " ^ (string_of_type tya1) ^ " != " ^ (string_of_type ty2)))
      | TyVar _ ->
        begin
          match expr1 with
          | Var var ->
            let env = bind env var (TyArrow ((TyVar (next_var_type())), (TyVar (next_var_type())))) in
            type_of_expr env app
          | _ -> failwith "Should not happend"
        end
      | _ -> raise (Typing_error ("Application " ^ (string_of_type ty1)))
    end
  | Lam (var,  ty_opt,  (loc, expr)) ->
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
  | Pair ((_, expr1), (_, expr2)) ->
    let env, ty1 = type_of_expr env expr1 in
    let env, ty2 = type_of_expr env expr2 in
    (env, TyTimes(ty1, ty2))
  | LetIn (var, (_, expr1), (_, expr2)) ->
    let env, ty1 = type_of_expr env expr1 in
    let env, ty2 = type_of_expr (bind env var ty1) expr2 in
    (env, ty2)
  | Fix (_, expr) ->
    begin
      match type_of_expr env expr with
      | (_, TyArrow(tya1, tya2)) ->
        let env, equal = unify_and_type_equal env tya1 tya2 in
        let (env, tya1) = type_of_expr env expr in
        if equal then (env, tya1)
        else raise (Typing_error "Fixpoint has type A->B with A != B")
      | _ -> raise (Typing_error "Fixpoint type should be of the form A->A")
    end
  | Int i -> env, TyInt
  | Bool b -> env, TyBool
  | Proj(Left ((_, expr))) -> type_of_proj env expr true
  | Proj(Right ((_, expr))) -> type_of_proj env expr false
  | Ite ((_, expr1), (_, expr2), (_, expr3)) ->
    let (env, ty1) = type_of_expr env expr1 in
    let (env, ty2) = type_of_expr env expr2 in
    let (env, ty3) = type_of_expr env expr3 in
    begin
      match ty1, ty2, ty3 with
      | TyBool, _, _ ->
        let env, equal = unify_and_type_equal env ty2 ty3 in
        let (env, ty2) = type_of_expr env expr2 in
        let (env, _) = type_of_expr env expr3 in
        if equal then (env, ty2)
        else raise (Typing_error "In 'if b then u else v', type(u) sould be equal to type(v)")
      | TyVar v, _, _ ->
        let env = replace_type_var env v TyBool in
        let env, equal = unify_and_type_equal env ty2 ty3 in
        let (env, ty2) = type_of_expr env expr2 in
        let (env, _) = type_of_expr env expr3 in
        if equal then (env, ty2)
        else raise (Typing_error "In 'if b then u else v', type(u) sould be equal to type(v)")
      | _, _, _ -> raise (Typing_error ("In 'if b then u else v' type(b) = " ^ (string_of_type ty1)))
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
  let env, ty1 = type_of_expr env expr1 in
  let env, ty2 = type_of_expr env expr2 in
  (* Printf.printf "DEBUG BEF %s %s\n" (string_of_type ty1) (string_of_type ty2);
     print_env env; *)
  let env, equal = unify_and_type_equal env ty1 ty2 in
  let env, ty1 = type_of_expr env expr1 in
  let env, ty2 = type_of_expr env expr2 in
  (* Printf.printf "DEBUG AFT %s %s\n\n" (string_of_type ty1) (string_of_type ty2); *)
  if op = '=' && equal then env, TyBool
  else if op = '=' then raise (Typing_error "Comparaison between two diffrents types")
  else
    begin
      match ty1, ty2 with
      | TyInt, TyInt ->
        if List.mem op ['+'; '-'; '*'; '/'] then (env, TyInt)
        else if List.mem op  ['>'] then (env, TyBool)
        else raise (Typing_error "Int operation should be between two int")
      | TyBool, TyBool ->
        if List.mem op ['&'; '|']
        then (env, TyInt)
        else raise (Typing_error "Operation between two bool should be && or ||")
      | t1, t2 ->
        raise (Typing_error ("Operation " ^ (String.make 1 op) ^ " between type " ^
                             (string_of_type t1) ^ " and " ^ (string_of_type t2)))
    end