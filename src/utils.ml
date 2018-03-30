open Format

type ('a,'b) result = Ok of 'a | Error of 'b

type 'a printer = Format.formatter -> 'a -> unit

type loc = int * int

let dloc = (-1,-1)

let loc_of_pos pos = let open Lexing in (pos.pos_lnum), (pos.pos_cnum - pos.pos_bol)

let get_loc lexbuf = let open Lexing in loc_of_pos lexbuf.lex_start_p

let print_loc fmt (l,c) = Format.fprintf fmt "(%d,%d)" l c


let dlevel = ref (-1)

let set_debug_mode i =
  dlevel := i

let debug i fmt =
    if !dlevel >= i
    then kfprintf (fun _ -> pp_print_newline err_formatter ()) err_formatter fmt
    else ifprintf err_formatter fmt

let color = ref true

let colored n s =
  if !color then "\027[3" ^ string_of_int n ^ "m" ^ s ^ "\027[m" else s

let red    = colored 1

let fail fmt =
  eprintf "%s" (red "ERROR ") ;
  kfprintf (fun _ -> pp_print_newline err_formatter () ; raise Exit) err_formatter fmt
