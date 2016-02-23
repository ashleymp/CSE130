type token =
  | Num of (int)
  | Id of (string)
  | TRUE
  | FALSE
  | EOF
  | LET
  | LBRAC
  | SEMI
  | RBRAC
  | COLONCOLON
  | REC
  | EQ
  | IN
  | FUN
  | ARROW
  | IF
  | THEN
  | ELSE
  | BIN
  | PLUS
  | MINUS
  | MUL
  | DIV
  | LT
  | LE
  | NE
  | AND
  | OR
  | LPAREN
  | RPAREN

val exp :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Nano.expr
