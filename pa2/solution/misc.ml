(* CSE 130: Programming Assignment 2
 * misc.ml
 *)

(* ***** DOCUMENT ALL FUNCTIONS YOU WRITE OR COMPLETE ***** *)
(* cmp: ('a*'b)->'a->'b->'b
   Compars two strings, one inside a tuple and a passed in one. 
    returns either of the desired output depending on whether the 
    two strings are equal or not.
    Implementation: An if statement is used to make the decision of which
                  paramenter to return
*)
let cmp (a,b) c d =
   if a=c then b else d;;

(* assoc : a' * 'b * ('b * 'a) list -> 'a
   Associates a given name with a name in the list and returns
   the number representing that name. If such name is not in the
   list, then -1 is returned, which is not tied to any name.
   Implementation: Uses tail recursion by making use of a helper function.
                   Pattern matching is used to determine when recursion stops. 
*)
let rec assoc (d,k,l) = 
   let rec helper d k l =
       match l with
       | [] -> d
       | h::t -> helper (cmp h k d) k t
   in helper d k l;;


(* removeDuplicates : 'a list -> a' list
   Takes in a list and returns a new list with the same elements but
   withou duplicates.
   Implementation: Added if statement to determine if an element should by
                   added to the new list by checking if an element like it has 
                   already been added.
 *)
let removeDuplicates l = 
  let rec helper (seen,rest) = 
      match rest with 
        [] -> seen
      | h::t -> 
        let seen' = (if List.mem h seen then seen else h::seen) in
        let rest' = t in 
	  helper (seen',rest') 
  in
      List.rev (helper ([],l))


(* wwhile (int -> int * bool) * int -> int
   Repeately calls a fucntion on its own output until false is 
   returned.
   Implementation; a helper function is used to determine when
                   to stop calling the fucntion on its own output.
 *)
let rec wwhile (f,b) = 
    let rec helper f (b,c) = 
        if c then helper f (f b) else b
    in helper f (f b);;

(* fixpoint : ('a -> 'a) * 'a -> 'a 
   Repeately calls a function on its own output until its input is the 
   same as its output.
   Implementation: Uses wwhile to repeatly call function and passes in a 
   a function that returns false and an element when its input is the same
   as its output.
*)
let fixpoint (f,b) = wwhile ((fun x -> if (f x) = x then ((f x),false) else ((f x),true)),b) 


(* ffor: int * int * (int -> unit) -> unit
   Applies the function f to all the integers between low and high
   inclusive; the results get thrown away.
 *)

let rec ffor (low,high,f) = 
  if low>high 
  then () 
  else let _ = f low in ffor (low+1,high,f)
      
(************** Add Testing Code Here ***************)
