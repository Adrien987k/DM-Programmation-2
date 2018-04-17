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

and subst env t x u =
  match t with
  | Var var as v->
    if (String.compare var x = 0) then u else v
  | App((loc1, expr1), (loc2, expr2)) ->
    App((loc1, subst env expr1 x u), (loc2, subst env expr2 x u))
  | Lam(var, ty, (loc, expr)) as lam ->
    if (String.compare var x = 0) then lam
    else Lam(var, ty, (loc, subst env expr x u)) 
  | Pair((loc1, expr1), (loc2, expr2)) ->
    Pair((loc1, subst env expr1 x u), (loc2, subst env expr2 x u))
  | LetIn (var, (loc1, expr1), (loc2, expr2)) as letIn ->
    if (String.compare var x = 0) then letIn
    else LetIn (var, (loc1, subst env expr1 x u), (loc2, subst env expr2 x u))
  | Fix (loc, expr) -> Fix (loc, subst env expr x u)
  | Int i -> Int i
  | Bool b -> Bool b
  | Proj (Left(loc, expr)) -> Proj (Left(loc, subst env expr x u)) 
  | Proj (Right(loc, expr)) -> Proj (Right(loc, subst env expr x u)) 
  | Ite ((loc1, expr1), (loc2, expr2), (loc3, expr3)) ->
    Ite ((loc1, subst env expr1 x u),
         (loc2, subst env expr2 x u),
         (loc3, subst env expr3 x u))
  | Binop (op, (loc1, expr1), (loc2, expr2)) ->
    Binop (op, (loc1, subst env expr1 x u), (loc2, subst env expr2 x u))


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
  | Proj (Left(_, expr)) -> eval_proj env expr true
  | Proj (Right(_, expr)) -> eval_proj env expr false 
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

and eval_proj env expr is_left =
  match expr with
  | Pair((_, expr1), (_, expr2)) ->
    if (is_left) 
    then eval_expr env expr1
    else eval_expr env expr2
  | _ ->
    let expr' = eval_expr env expr in
    if is_left 
    then eval_expr env (Proj(Left((Utils.dloc, expr'))))
    else eval_expr env (Proj(Right((Utils.dloc, expr'))))

and eval_binop env op expr1 expr2 =
  match op with
  | Plus -> eval_int_op env (+) expr1 expr2
  | Minus -> eval_int_op env (-) expr1 expr2
  | Times -> eval_int_op env ( *) expr1 expr2
  | Div -> eval_int_op env (/) expr1 expr2
  | And -> eval_bool_op env (&&) expr1 expr2
  | Or -> eval_bool_op env (||) expr1 expr2
  | Eq ->
    let expr1' = eval_expr env expr1 in
    let expr2' = eval_expr env expr2 in
    Bool (value_equal env expr1' expr2')
  | Gt ->
    match eval_expr env expr1, eval_expr env expr2 with
    | Int i1, Int i2 -> Bool(i1 <= i2)
    | _ -> failwith "SHould not happend"


and value_equal env expr1 expr2 =
  match expr1, expr2 with
  | Int i1, Int i2 -> i1 = i2
  | Bool b1, Bool b2 -> b1 = b2
  | Pair((loca1, expra1), (loca2, expra2)), 
    Pair((locb1, exprb1), (locb2, exprb2)) ->
    let expra1' = eval_expr env expra1 in
    let expra2' = eval_expr env expra2 in
    let exprb1' = eval_expr env exprb1 in
    let exprb2' = eval_expr env exprb2 in
    (value_equal env expra1' exprb1') && (value_equal env expra2' exprb2')
  | Lam(var1, ty1, (_, exprlam1)), Lam(var2, ty2, (_, exprlam2)) ->
    false
  | _ -> failwith "Should not happend"

and eval_int_op env op expr1 expr2 =
  match eval_expr env expr1, eval_expr env expr2 with
  | Int i1, Int i2 -> Int (op i1 i2)
  | _ -> failwith "Should never happend"

and eval_bool_op env op expr1 expr2 =
  match eval_expr env expr1, eval_expr env expr2 with
  | Bool b1, Bool b2 -> Bool (op b1 b2)
  | _ -> failwith "Should never happend"

