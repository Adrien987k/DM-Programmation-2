open Ast

let rec eval_ast (cmds : Ast.t) : (Ast.var * Ast.expr) list =
  let cmds = List.map
      (fun cmd_l -> let _, cmd = cmd_l in cmd ) cmds
  in
  let values = [] in
  let values = List.fold_left
      (fun values cmd -> eval_cmd values cmd)
      values cmds
  in
  values

and eval_cmd values cmd_l = []