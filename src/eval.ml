open Ast

let is_value expr =
  match expr with
  | Var _
  | Lam (_, _, _)
  | Pair (_, _)
  | Int _
  | Bool _ -> true
  | _ -> false

let rec eval_ast (cmds : Ast.t) : (Ast.var * Ast.expr) list =
  let cmds = List.map
      (fun cmd_l -> let _, cmd = cmd_l in cmd) cmds
  in
  let env = [] in
  let env = List.fold_left
      (fun env cmd -> eval_cmd env cmd)
      env cmds
  in
  env

and eval_cmd env cmd =
  match cmd with
  | Let (var, _, (_, expr)) -> []
  | LetRec (var, _, (_, expr)) -> []

(* TODO *)
and subst env t x u = t

and eval_expr env expr =
  match expr with
  | Var var -> List.assoc var env
  | App ((loc1, expr1), (loc2, expr2)) ->
    begin
      match expr1 with
      | Lam (var, _, (_, expr3)) ->
        if is_value expr2 then
          subst env expr3 var expr2
        else
          let expr2' = eval_expr env expr2 in
          eval_expr env (App((loc1, expr1), (loc2, expr2')))
      | _ -> 
        let expr1' = eval_expr env expr in
        eval_expr env (App((loc1, expr1'), (loc2, expr2)))
    end
  | Lam (_, _, _) as lam -> lam
  | Pair (_, _) as pair -> pair
  | LetIn (var, (loc1, expr1), (loc2, expr2)) ->
    if is_value expr1 then
      subst env expr2 var expr1
    else
      let expr1' = eval_expr env expr1 in
      eval_expr env (LetIn(var, (loc1, expr1'), (loc2, expr2)))
  | Fix (loc, expr) ->
    begin
      match expr with
      | Lam(var, ty, (loc1, expr1)) as lam ->
        subst env expr1 var (Fix((loc, lam)))
      | _ ->
        let expr' = eval_expr env expr in
        eval_expr env (Fix((loc, expr')))
    end
  | Int i -> Int i
  | Bool b -> Bool b
  | Proj p -> eval_proj env p
  | Ite ((loc1, expr1), (loc2, expr2), (loc3, expr3)) ->
    begin  
      match expr1 with
      | Bool b -> if b then eval_expr env expr2 else eval_expr env expr3
      | _ ->
        let expr1' = eval_expr env expr1 in
        eval_expr env (Ite((loc1, expr1'), (loc2, expr2), (loc3, expr3)))
    end
  | Binop (op, (_, expr1), (_, expr2)) ->
    eval_binop env op expr1 expr2

(* TODO *)
and eval_proj env p = Bool true

(* TODO *)
and eval_binop env op expr1 expr2 =
  expr1