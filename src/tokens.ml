
type token =
  | TIMES
  | RIGHTPAR of Utils.loc
  | PLUS
  | MINUS
  | LEFTPAR of Utils.loc
  | EQUAL
  | EOF
  | DIV
  | COMMA
  | COLON
  | ARROW
  | ID of (Utils.loc * string)
  | LET
  | REC
  | FUN
  | IN
  | INT
  | BOOL
  | TYID of string
  | NUM of (Utils.loc * int)
  | TRUE of Utils.loc
  | FALSE of Utils.loc
  | FST
  | SND
  | IF
  | THEN
  | ELSE
  | AND
  | OR
  | GT
  | UNIT of Utils.loc
