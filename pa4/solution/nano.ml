exception MLFailure of string

type binop = 
  Plus  
| Minus 
| Mul 
| Div 
| Eq 
| Ne 
| Lt 
| Le 
| And 
| Or          
| Cons

type expr =   
  Const of int 
| True   
| False      
| NilExpr
| Var of string    
| Bin of expr * binop * expr 
| If  of expr * expr * expr
| Let of string * expr * expr 
| App of expr * expr 
| Fun of string * expr    
| Letrec of string * expr * expr
	
type value =  
  Int of int		
| Bool of bool          
| Closure of env * string option * string * expr 
| Nil                    
| Pair of value * value     

and env = (string * value) list

let binopToString op = 
  match op with
      Plus -> "+" 
    | Minus -> "-" 
    | Mul -> "*" 
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v = 
  match v with 
    Int i -> 
      Printf.sprintf "%d" i
  | Bool b -> 
      Printf.sprintf "%b" b
  | Closure (evn,fo,x,e) -> 
      let fs = match fo with None -> "Anon" | Some fs -> fs in
      Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
  | Pair (v1,v2) -> 
      Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2) 
  | Nil -> 
      "[]"

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
  "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
      Const i ->
        Printf.sprintf "%d" i
    | True -> 
        "true" 
    | False -> 
        "false"
    | Var x -> 
        x
    | Bin (e1,op,e2) -> 
        Printf.sprintf "%s %s %s" 
        (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) -> 
        Printf.sprintf "if %s then %s else %s" 
        (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) -> 
        Printf.sprintf "let %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 
    | App (e1,e2) -> 
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) -> 
        Printf.sprintf "fun %s -> %s" x (exprToString e) 
    | Letrec (x,e1,e2) -> 
        Printf.sprintf "let rec %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 

(*********************** Some helpers you might need ***********************)

let rec fold f base args = 
  match args with [] -> base
    | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) = 
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)

(* lookup: string * env -> value
 * lookup looks for a variable in an environment list. If it
 * findes the variable, it will return the value associated with the variable.
 * Else, it will return an MLFailure specifying that the variable is no
 * bound to the environment.
 * Implementation: used a match pattern to match the result of the call
 * to listAssoc with either the variable found or the failure if none was found.
 *)
let lookup (x,evn) = let inlist = (listAssoc (x,evn)) in
      match inlist with
      | Some v -> v
      | None -> raise (MLFailure ("Variable not bound: "^x));;

(* eval: env * expr -> value
 * eval evaluates an ML-nano expression in the environment it is passed.
 * It raises a failure if the expression finds an unbound variable.
 * Implementation: pattern matching is used to indentify each part of the 
 * the expression and it's validity. 
 *)
let rec eval (evn,e) = 
      match e with
      | Const x -> Int x
      | True -> Bool true
      | False -> Bool false
      | Var s   -> lookup (s, evn)
      | If (ex1, ex2, ex3) -> 
          (* If statements have multiple possible results, so 
             pattern match is used to determine what we should return *) 
           ( let b = (eval (evn,ex1)) in
            let e1 = (eval (evn,ex2)) in
            let e2 = (eval (evn,ex3)) in 
            match (b,e1,e2) with
            | (Bool true, Int i, Int j)    -> Int i
            | (Bool true, Bool b, Bool c)  -> Bool b
            | (Bool false, Int i, Int j)   -> Int j
            | (Bool false, Bool b, Bool c) -> Bool c)
      | Let (s, ex2, ex3) ->
           ( let evn = (s,(eval (evn,ex2)))::evn in (eval (evn,ex3)) )
      | Letrec (s, ex1, ex2) ->  (* can't figure this out, so no part 2e :( *)
           ( let evn = (s,(eval (evn,ex1)))::evn in (eval (evn,ex2)) )
      | App (e1,e2) -> 
           (* Applying e2 to e1, makes use of Closures *)
           ( let Closure (env,f,x,e) = (eval (evn,e1)) in
             match f with
             | None -> eval ((x, eval(evn,e2))::env, e)
             | Some n -> eval (((n, 
                             Closure (env,f,x,e))::(x,eval(evn,e2))::env),e)
           )
      | Fun (s,e1) -> Closure (evn, None, s, e1)
      | Bin (exp1, bin, exp2) ->
             (* Bin operations have multiple applications/outcomes. Using
                pattern matching to determine how to proceed *)
             let e1 = eval (evn, exp1) in 
             let e2 = eval (evn, exp2) in
             match (e1, bin, e2) with
             | (Int i, Plus, Int j)  -> Int (i + j)
             | (Int i, Minus, Int j) -> Int (i - j)
             | (Int i, Mul, Int j)   -> Int (i * j)
             | (Int i, Div, Int j)   -> Int(i / j)
             | (Int i, Eq, Int j)    -> Bool (i = j)
             | (Int i, Ne, Int j)    -> Bool (i != j)
             | (Int i, Lt, Int j)    -> Bool (i < j)
             | (Int i, Le, Int j)    -> Bool (i <= j)
             | (Bool b, Eq, Bool c)  -> Bool (b = c)
             | (Bool b, Ne, Bool c)  -> Bool (b != c)
             | (Bool b, And, Bool c) -> Bool (b && c)
             | (Bool b, Or, Bool c)  -> Bool (b || c)

(**********************     Testing Code  ******************************)
