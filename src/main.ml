open Utils

let no_eval = ref false

let no_typing = ref false

let apply_closure = ref false

(* Exception raised by the Parser module *)
exception Parsing of loc * string

(* Parse a channel and produce an AST *)
let read ic =
  let lex = Lexing.from_channel ic in
  try Parser.main Lexer.token lex
  with Parser.Error ->
    let loc = get_loc lex in
    let tok = Lexing.lexeme lex in
    let mesg = Format.sprintf "Unexpected token '%s'." tok in
    raise (Parsing(loc,mesg))
     | End_of_file -> exit 0

(* Type, then apply closure conversion and finally evaluate the ast *)
let process_ast ast =
  let ast' = if !no_typing then ast else
      begin
        match Typing.typing_ast ast with
        | Ok ast -> ast
        | Error(err) -> fail "%a@." Typing.print_error err
      end
  in
  let ast'' = if !apply_closure then Closure.closure_ast ast' else ast' in
  if not !no_eval then
    List.iter (fun (var,e) ->
        Format.printf "val %a = %a@."
          Printer.print_var var Printer.print_expr e) (Eval.eval_ast ast'');
  ast''


(* Process a file *)
let run_on_file printing_mode file =
  let input = open_in file in
  debug 1 "Processing file '%s' ..." file;
  let ast = read input in
  let ast' = process_ast ast in
  if printing_mode then
    Printer.print_ast Format.std_formatter ast';
  close_in input

(* Entry point *)
let _ =
  let printing_mode = ref false in
  let options = Arg.align
        [ ("-d"
        , Arg.Int set_debug_mode
        , "N sets the debuging level to N. Default is -1" )
        ; ("-p"
        , Arg.Set printing_mode
        , " Print the output on standard input")
        ; ("--no-eval"
        , Arg.Set no_eval
        , " Does not evaluate the program" )
        ; ("--no-typing"
        , Arg.Set no_typing
        , " Remove the typing" )
        ; ("--closure"
        , Arg.Set apply_closure
        , " Apply the closure conversion before evaluating the program" ) ;
      ]
  in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]...\n" in
  let usage = usage ^ "Available options:" in
  let files =
    let files = ref [] in
    Arg.parse options (fun f -> files := f :: !files) usage;
    List.rev !files
  in
  try
    List.iter (run_on_file !printing_mode) files;
  with
  | Lexer.Error(msg) ->
    Format.eprintf "Lexer error:\n %s\n" msg;
    exit 1
  | Parsing(loc, msg) ->
    Format.eprintf "Parser error at %a: %s\n" print_loc loc msg;
    exit 2
  | Exit -> exit 3
