{
  open Lexing
  open Tokens
  open Utils

  exception Error of string
}

let space   = [' ' '\t' '\r']
let ident = ['a'-'z' '_']['a'-'z' 'A'-'Z' '0'-'9' '\'' ]*
let number = ['-']?(['1'-'9']['0'-'9']*|'0')
let tyident = ['\'']['a'-'z']*

rule token = parse
  | space     { token lexbuf }
  | "let"     { LET }
  | "fun"     { FUN }
  | "rec"     { REC }
  | "int"     { INT }
  | "bool"    { BOOL }
  | "in"      { IN }
  | "if"      { IF }
  | "then"    { THEN }
  | "else"    { ELSE }
  | "fst"     { FST }
  | "snd"     { SND }
  | "true"    { TRUE (get_loc lexbuf) }
  | "false"   { FALSE (get_loc lexbuf) }
  | "&&"      { AND }
  | "||"      { OR }
  | '\n'      { new_line lexbuf ; token lexbuf }
  | "(*"      { comment lexbuf }
  | ','       { COMMA }
  | ':'       { COLON }
  | '('       { LEFTPAR }
  | ')'       { RIGHTPAR }
  | '>'       { GT }
  | "->"      { ARROW }
  | "="       { EQUAL }
  | "+"       { PLUS }
  | "-"       { MINUS }
  | "*"       { TIMES }
  | "/"       { DIV   }
  | number as n { NUM(get_loc lexbuf, int_of_string n) }
  | ident as id { ID ( get_loc lexbuf , id ) }
  | tyident as tid { TYID tid }
  | _ as s
  { let loc = get_loc lexbuf in
  raise (Error (Format.asprintf "At position %a: Unexpected characters '%s'." print_loc
                 loc (String.make 1 s))) }
  | eof { EOF }


and comment = parse
  | "*)" { token lexbuf }
  | '\n' { new_line lexbuf ; comment lexbuf }
  | _    { comment lexbuf }
  | eof  { raise (Error ("Unexpected end of file."))  }