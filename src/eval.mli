(** [eval_ast t] computes each command one by one and returns each time a value. *)
val eval_ast : Ast.t -> (Ast.var * Ast.expr) list
