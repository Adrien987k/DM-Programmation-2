open Ast

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
  | App((_, expr1), (_, expr2)) -> expr
  | Lam(var, ty_opt, (_expr1)) -> expr
  | Pair((loc1, expr1), (loc2, expr2)) -> expr
  | LetIn(var, (loc1, expr1), (loc2, expr2)) -> expr
  | Fix((loc, expr1)) -> expr
  | _ -> expr
