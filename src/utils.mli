(** used in typing.ast *)
type ('a,'b) result = Ok of 'a | Error of 'b

(** Locations are used to enhance error messages *)
type loc

(** type alias for printers. Useful for the Printer module *)
type 'a printer = Format.formatter -> 'a -> unit

(** [dloc] returns a dummy location *)
val dloc : loc

(** [get_loc lexbuf] returns a location from [lexbuf]. Useful for the Lexer module *)
val get_loc : Lexing.lexbuf -> loc

(** [loc_of_pos pos] returns a location from [pos]. Useful for the Lexer module *)
val loc_of_pos : Lexing.position -> loc

(** [pp_loc fmt loc] prints a location *)
val print_loc : loc printer

(** [debug i msg] prints the message [msg] if the debug level is greater than [i] *)
val debug : int -> ('a, Format.formatter, unit, unit) format4 -> 'a

(** [set_debug_mode i] sets the debug level *)
val set_debug_mode : int -> unit

(** [fail err_msg] prints an error message and then exits. *)
val fail : ('a, Format.formatter, unit, 'b) format4 -> 'a
