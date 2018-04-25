open Ast

let rec field n expr =
  match expr with
  | Pair ((_, expr1), (_, expr2)) ->
    if n = 0 then expr1
    else field (n-1) expr2
  | _ -> 
    if n = 0 then expr
    else failwith ("Try to acces field " 
                   ^ (string_of_int n) ^ " of expr")


let rec closure_ast (ast:Ast.t) : Ast.t =
  List.map
    (fun cmd -> closure_cmd cmd) ast

and closure_cmd cmd =
  match cmd with
  | (locl, Let(var, ty, (loce, expr))) ->
    locl, Let(var, ty, (loce, closure_expr expr))
  | (locl, LetRec (var, ty, (loce, expr))) ->
    locl, LetRec(var, ty, (loce, closure_expr expr))

and closure_expr expr =
  match expr with
  | Var var -> expr
  | App((loc1, expr1), (loc2, expr2)) ->
    let c = closure_expr expr1 in
    let f0 = field 0 c in
    LetIn("c", (loc1, c), 
          (loc2, 
           (App(((Utils.dloc, f0), 
                 ((Utils.dloc, 
                   Pair((Utils.dloc, c),
                        (Utils.dloc, closure_expr expr2))
                  ))))
           )))
  | Lam(var, ty_opt, (_, expr1)) as lam -> closure_lam lam
  | Pair((loc1, expr1), (loc2, expr2)) ->
    Pair((loc1, closure_expr expr1), (loc2, closure_expr expr2))
  | LetIn(var, (loc1, expr1), (loc2, expr2)) ->
    LetIn(var, (loc1, closure_expr expr1), (loc2, closure_expr expr2))
  | Fix((loc, expr1)) ->
    Fix((loc, closure_expr expr1))
  | Int i -> expr
  | Bool b -> expr
  | Proj(Left((loc1, expr1))) ->
    Proj(Left((loc1, closure_expr expr1)))
  | Proj(Right((loc1, expr1))) ->
    Proj(Right((loc1, closure_expr expr1)))
  | Ite((loc1, expr1), (loc2, expr2), (loc3, expr3)) ->
    Ite((loc1, closure_expr expr1), (loc2, closure_expr expr2), (loc3, closure_expr expr3))
  | Binop(op, (loc1, expr1), (loc2, expr2)) ->
    Binop(op, (loc1, closure_expr expr1), (loc2, closure_expr expr2))
  | Unit -> Unit

(* TODO Typing *)
and closure_lam lam =
  let rec make_lets vars expr c =
    let expr' = closure_expr expr in
    List.fold_left (fun lets_n var ->
        let lets, n = lets_n in
        (LetIn("x" ^ (string_of_int n), (Utils.dloc, field n c), (Utils.dloc, lets)), (n + 1))) (expr', 1) vars
  in
  let rec free_vars expr = match expr with
    | Var var -> [var]
    | App((loc1, expr1), (loc2, expr2)) -> (free_vars expr1) @ (free_vars expr2)

    | Lam(var, ty_opt, (_, expr1)) as lam ->
      List.filter (fun v -> String.compare v var = 0) (free_vars expr1)
    | Pair((loc1, expr1), (loc2, expr2)) -> (free_vars expr1) @ (free_vars expr2)
    | LetIn(var, (loc1, expr1), (loc2, expr2)) ->
      List.filter (fun v -> String.compare v var = 0) ((free_vars expr1) @ (free_vars expr2))
    | Fix((loc, expr1)) -> (free_vars expr1)
    | Int i -> []
    | Bool b -> []
    | Proj(Left((loc1, expr1))) -> (free_vars expr1)
    | Proj(Right((loc1, expr1))) -> (free_vars expr1)
    | Ite((loc1, expr1), (loc2, expr2), (loc3, expr3)) ->
      (free_vars expr1) @ (free_vars expr2) @ (free_vars expr3)
    | Binop(op, (loc1, expr1), (loc2, expr2)) -> (free_vars expr1) @ (free_vars expr2)
    | Unit -> [] 
  in
  let rec make_pairs vars = 
    match vars with
    | var :: [] -> Utils.dloc, Var var
    | var :: qvars -> Utils.dloc, Pair((Utils.dloc, Var var), make_pairs qvars)
    | _ -> failwith "Should not happend"
  in
  match lam with
  | Lam(var, ty_opt, (loc1, expr1)) as lam ->
    let expr1' = closure_expr expr1 in
    let vars = free_vars lam in
    let _, c = make_pairs vars in
    let body, _ = make_lets vars expr1' c in
    begin
      match vars with
      | [] -> Lam("c", None, (Utils.dloc, Lam(var, None, (Utils.dloc, body))))
      | vars ->
        Pair((Utils.dloc, Lam("c", None,
                              (Utils.dloc, Lam(var, None, (Utils.dloc, body))
                              ))), make_pairs vars)
    end
  | _ -> failwith "Should not happend"
