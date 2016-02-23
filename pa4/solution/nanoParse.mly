%{
(* See this for a tutorial on ocamlyacc 
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/ *)
open Nano 
%}

%token <int> Num
%token <string> Id
%token  TRUE
%token  FALSE
%token EOF
/* Adding tokens */
%token LET 
%token LBRAC SEMI RBRAC
%token COLONCOLON
%token REC
%token EQ
%token IN
%token FUN
%token ARROW
%token IF
%token THEN
%token ELSE
%token BIN
%token PLUS
%token MINUS
%token MUL
%token DIV
%token LT
%token LE 
%token NE
%token AND
%token OR
%token LPAREN
%token RPAREN

/* Associativity of the tokens */
%nonassoc  LET FUN IF
%left OR
%left AND
%left EQ NE LT LE
%left PLUS MINUS
%left MUL DIV
%left APP
%right COLONCOLON

%start exp
%type <Nano.expr> exp

%%
/* Rules must be separated in this manner (different labels */
/*   to denote precedence among the operations */
/* Lower Priority */
exp:  LET Id EQ exp IN  exp    { Let ($2,$4,$6) }
    | LET REC Id EQ exp IN exp  { Letrec ($3,$5,$7) }
    | IF exp THEN exp ELSE exp  { If ($2,$4,$6)}
    | FUN Id ARROW exp          { Fun ($2,$4) }
    | expa                      {$1}
/* Binary operators */
expa: expa OR expb              { Bin($1, Or,$3) }
    | expb                      { $1 }

expb: expb AND expc             { Bin($1, And,$3) }
    | expc                      { $1 }
/* Using binary operators for this ones as well */
expc: expc LT expec              { Bin($1, Lt,$3) }
    | expc LE expec              { Bin($1, Le,$3) }
    | expc NE expec              { Bin($1, Ne,$3) }
    | expc EQ expec              { Bin($1,Eq,$3) }
    | expec                      { $1 }
/* Extra Credit for part 1 */
expec: LBRAC expec SEMI expec        { Bin($2,Cons,$4) }
    | expec RBRAC                    { Bin($1,Cons,NilExpr) }
    | expec SEMI expec               {Bin($1,Cons,$3)}
    | expec COLONCOLON expec         {Bin($1,Cons,$3)}
    | LBRAC RBRAC                    {NilExpr}
    | expd                           {$1} 

expd: expd PLUS expe           { Bin($1, Plus,$3) }
    | expd MINUS expe           { Bin($1, Minus ,$3) }
    | expe                      { $1 }

expe: expe MUL expf             { Bin($1, Mul,$3) }
    | expe DIV expf             { Bin($1, Div,$3) }
    | expf                      { $1 }
/* Higher Priority */
expf: expf expg                 { App ($1,$2) }
    | expg                      {$1}
/* base elements */
expg: Num                       { Const $1 }
    | Id                        { Var($1) }
    | TRUE                      { True }
    | FALSE                     { False }
    | LPAREN exp RPAREN         { $2 }





    
