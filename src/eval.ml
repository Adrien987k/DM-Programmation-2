open Ast

let var_in env v =
  try 
    let _ = List.find (fun (var, _) -> String.compare v var = 0) env in
    true
  with Not_found -> false

let letters =
  [|'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm';
    'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z'|]

let word_pointer = ref []
let counter = ref 0

let fresh_var env =
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
  let next_word = 
    inc_pointer;
    List.fold_left (fun acc n -> acc ^ (String.make 1 (Array.get letters n))) ("") (!word_pointer)
  in
  let cont = ref true in
  let word = ref "" in
  while !cont do
    word := next_word;
    if var_in env (!word) then ()
    else cont := false;
  done;
  counter := !counter + 1;
  !word ^ "_" ^ (string_of_int !counter)

let rec rename_lam env lam =
  let rec rename_expr_under expr var_to_rep new_var =
    match expr with
    | Lam(var, ty, (loc, expr)) ->
      let lam = Lam(var, ty, (loc, rename_expr_under expr var_to_rep new_var)) in
      rename_lam env lam
    | Var var as v ->
      if String.compare var var_to_rep = 0
      then Var (new_var) else v
    | App((loc1, expr1), (loc2, expr2)) ->
      App((loc1, rename_expr_under expr1 var_to_rep new_var),
          (loc2, rename_expr_under expr2 var_to_rep new_var))
    | Pair((loc1, expr1), (loc2, expr2)) ->
      Pair((loc1,  rename_expr_under expr1 var_to_rep new_var), (loc2,  rename_expr_under expr2 var_to_rep new_var))
    | LetIn (var, (loc1, expr1), (loc2, expr2)) ->
      LetIn (var, (loc1, rename_expr_under expr1 var_to_rep new_var),
             (loc2, rename_expr_under expr2 var_to_rep new_var))
    | Fix(loc, expr) -> Fix(loc, rename_expr_under expr var_to_rep new_var)
    | Int i -> Int i
    | Bool b -> Bool b
    | Proj (Left(loc, expr)) -> 
      Proj(Left(loc, rename_expr_under expr var_to_rep new_var))
    | Proj (Right(loc, expr)) -> 
      Proj(Left(loc, rename_expr_under expr var_to_rep new_var))
    | Ite ((loc1, expr1), (loc2, expr2), (loc3, expr3)) ->
      Ite ((loc1, rename_expr_under expr1 var_to_rep new_var), 
           (loc2, rename_expr_under expr2 var_to_rep new_var), 
           (loc3, rename_expr_under expr3 var_to_rep new_var))
    | Binop (op, (loc1, expr1), (loc2, expr2)) ->
      Binop (op, (loc1, rename_expr_under expr1 var_to_rep new_var), 
             (loc2, rename_expr_under expr2 var_to_rep new_var))
    | Unit -> Unit
  in
  match lam with
  | Lam(var, ty, (loc, expr)) ->
    if var_in env var
    then
      let fresh = fresh_var env in
      let expr = rename_expr_under expr var fresh in
      Lam(fresh, ty, (loc, expr))
    else Lam(var, ty, (loc, rename_lam env expr))
  | _ -> lam

let rec eval_ast (cmds : Ast.t) : (Ast.var * Ast.expr) list =
  let cmds = List.map
      (fun cmd_l -> let _, cmd = cmd_l in cmd) cmds
  in
  let env = [] in
  let env = List.fold_left
      (fun env cmd ->
         let env = eval_cmd env cmd in
         env
      )
      env cmds
  in
  List.rev env

and eval_cmd env cmd =
  match cmd with
  | Let (var, _, (_, expr)) ->
    (var, eval_expr env expr) :: env
  | LetRec (var, ty, (loc, expr)) ->
    (var, eval_expr env expr) :: env

and subst env t x u =
  match t with
  | Var var as v ->
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
  | Unit -> Unit

and eval_expr env expr =
  match expr with
  | Var var ->
    begin
      try
        List.assoc var env
      with Not_found -> failwith ("Evaluation: Unbound value " ^ var)
    end
  | App ((loc1, expr1), (loc2, expr2)) ->
    let expr1' = eval_expr env expr1 in
    begin
      match expr1' with
      | Lam (var, _, (_, expr3)) ->
        let expr2' = eval_expr env expr2 in
        eval_expr env (subst env expr3 var expr2')
      | _ -> failwith "Should never happend APP"
    end
  | Lam(var, ty, (loc1, expr1)) as lam -> rename_lam env lam
  | Pair((loc1, expr1), (loc2, expr2)) ->
    Pair((loc1, eval_expr env expr1), (loc2, eval_expr env expr2))
  | LetIn (var, (loc1, expr1), (loc2, expr2)) ->
    let expr1' = eval_expr env expr1 in
    eval_expr env (subst env expr2 var expr1')
  | Fix (loc, expr) ->
    begin
      match expr with
      | Lam(var, ty, (loc1, expr1)) ->
        eval_expr env (subst env expr1 var (Fix((loc, expr))))
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
  | Unit -> Unit

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
  | Times -> eval_int_op env ( * ) expr1 expr2
  | Div -> eval_int_op env (/) expr1 expr2
  | And -> eval_bool_op env (&&) expr1 expr2
  | Or -> eval_bool_op env (||) expr1 expr2
  | Eq ->
    let expr1' = eval_expr env expr1 in
    let expr2' = eval_expr env expr2 in
    Bool (value_equal env expr1' expr2')
  | Gt ->
    match eval_expr env expr1, eval_expr env expr2 with
    | Int i1, Int i2 -> Bool(i1 > i2)
    | _ -> failwith "Should never happend BINOP"


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
  | _ -> failwith "Should never happend EQUAL"

and eval_int_op env op expr1 expr2 =
  match eval_expr env expr1, eval_expr env expr2 with
  | Int i1, Int i2 -> Int (op i1 i2)
  | _ -> failwith "Should never happend INT OP"

and eval_bool_op env op expr1 expr2 =
  match eval_expr env expr1, eval_expr env expr2 with
  | Bool b1, Bool b2 -> Bool (op b1 b2)
  | _ -> failwith "Should never happend BOOL OP"

