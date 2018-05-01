open Ast

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

let rec make_field n var =
  if n = 0 then
    Proj(Left(Utils.dloc, Var var))
  else if n = 1 then
    Proj(Right(Utils.dloc, Var var))
  else
    Proj(Right(Utils.dloc, make_field (n - 1) var))


let current_rec = ref ""

let rec closure_ast (ast:Ast.t) : Ast.t =
  List.map
    (fun cmd -> closure_cmd cmd) ast


and closure_cmd cmd =
  current_rec := "";
  match cmd with
  | (locl, Let(var, ty, (loce, expr))) ->
    current_rec := var; (* MODIFIE *)
    locl, Let(var, ty, (loce, closure_expr expr))
  | (locl, LetRec (var, ty, (loce, expr))) ->
    current_rec := var;
    locl, LetRec(var, ty, (loce, closure_expr expr))

and closure_expr expr =
  match expr with
  | Var var -> expr
  | App((loc1, expr1), (loc2, expr2)) ->
    let c = fresh_var () in
    LetIn(c, (loc1, closure_expr expr1),
          (loc2,
           (App(((Utils.dloc, make_field 0 c),
                 (Utils.dloc, Pair((Utils.dloc, Var c), (Utils.dloc, closure_expr expr2)))
                )))))
  | Lam(var, ty_opt, (_, expr1)) as lam -> closure_lam lam
  | Pair((loc1, expr1), (loc2, expr2)) ->
    Pair((loc1, closure_expr expr1), (loc2, closure_expr expr2))
  | LetIn(var, (loc1, expr1), (loc2, expr2)) ->
    LetIn(var, (loc1, closure_expr expr1), (loc2, closure_expr expr2))
  | Fix((loc, expr1)) ->
    begin
      match expr1 with
      | Pair((loc1, expr1'), (loc2, expr2')) ->
        failwith "OK"
      (* Pair((loc1, Fix((loc, expr1'))), (loc2, expr2')) *)
      | _ -> Fix((loc, closure_expr expr1))
    end
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

and closure_lam lam =
  let rec make_lets vars expr var =
    List.fold_left (fun lets_n v ->
        let lets, n = lets_n in
        (LetIn(v, (Utils.dloc, make_field n var), (Utils.dloc, lets)), (n - 1)))
      (expr, (List.length vars)) vars
  in
  let rec free_vars expr =
    match expr with
    | Var var -> [var]
    | App((loc1, expr1), (loc2, expr2)) -> (free_vars expr1) @ (free_vars expr2)
    | Lam(var, ty_opt, (_, expr1)) ->
      List.filter (fun v -> String.compare v var <> 0) (free_vars expr1)
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
    | _ -> failwith "Should never happend MAKE PAIRS"
  in
  match lam with
  | Lam(var, ty_opt, (loc1, expr1)) ->
    let expr1' = closure_expr expr1 in
    let vars =
      List.filter (fun v ->
          String.compare v !current_rec <> 0 &&
          String.compare v var <> 0
        ) (free_vars lam) 
    in
    begin
      match vars with
      | [] ->
        let c_x = fresh_var () in
        let c = fresh_var () in
        Pair((Utils.dloc, Lam(c_x, None, (Utils.dloc, (
            LetIn(c, (Utils.dloc, Proj(Left((Utils.dloc, Var c_x)))),
                  ((Utils.dloc,
                    LetIn(var, (Utils.dloc, Proj(Right(Utils.dloc, (Var c_x)))), (Utils.dloc, expr1'))
                   ))))))), (Utils.dloc, Unit))
      | _ ->
        let c_x = fresh_var () in
        let c = fresh_var () in
        let body, _ = make_lets vars expr1' c in
        Pair((Utils.dloc, Lam(c_x, None, (Utils.dloc, (
            LetIn(c, (Utils.dloc, Proj(Left((Utils.dloc, Var c_x)))),
                  ((Utils.dloc,
                    LetIn(var, (Utils.dloc, Proj(Right(Utils.dloc, (Var c_x)))), (Utils.dloc, body))
                   ))))))), make_pairs vars)
    end
  | _ -> failwith "Should never happend LAM"
