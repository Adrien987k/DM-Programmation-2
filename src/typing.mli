type typing_error

(** [print_error fmt err] prints a typing error.*)
val print_error : typing_error Utils.printer

(** typing_ast [ast] returns an ast where all the types are filled if the ast is well-typed, otherwise, returns a typing error *)
val typing_ast : Ast.t -> (Ast.t, typing_error) Utils.result
