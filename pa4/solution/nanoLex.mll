{
  open Nano        (* nano.ml *)
  open NanoParse   (* nanoParse.ml from nanoParse.mly *)
}

(* strings associated with token, being sirected to rule *)
rule token = parse
  | [' ' '\t' '\r' '\n']       { token lexbuf }
  | ['0'-'9']+ as l   { Num (int_of_string l)}
  | "("        { LPAREN }       (* Extra Credit part 1, start *)
  | ")"        { RPAREN }
  | "["        { LBRAC }
  | "]"        { RBRAC }
  | ";"        { SEMI }
  | "::"       { COLONCOLON }   (* Extra Credit part 1, end *)
  | "+"        { PLUS }
  | "-"        { MINUS }
  | "*"        { MUL }
  | "/"        { DIV }
  | "<"        { LT }
  | "<="       { LE }
  | "!="       { NE }
  | "&&"       { AND }
  | "||"       { OR }
  | "true"     { TRUE }
  | "false"    { FALSE }
  | "let"      { LET } 
  | "rec"      { REC } 
  | "="        { EQ } 
  | "in"       { IN } 
  | "fun"      { FUN } 
  | "->"       { ARROW } 
  | "if"       { IF } 
  | "then"     { THEN } 
  | "else"     { ELSE }
  (* Id must be in the bottom, it could take the string of another
     token and cause an error  *) 
  | [ 'A'-'Z' 'a'-'z'][ 'A'-'Z' 'a'-'z' '0'-'9' ]* as str { Id(str) }
  | _           { raise (MLFailure ("Illegal Character '"^(Lexing.lexeme lexbuf)^"'")) }
  |  eof         { EOF }
