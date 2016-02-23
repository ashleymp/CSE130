(* CSE 130: Programming Assignment 3
 * misc.ml
 *)

(* For this assignment, you may use the following library functions:

   List.map
   List.fold_left
   List.fold_right
   List.split
   List.combine
   List.length
   List.append
   List.rev

   See http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html for
   documentation.
*)



(* Do not change the skeleton code! The point of this assignment is to figure
 * out how the functions can be written this way (using fold). You may only
 * replace the   failwith "to be implemented"   part. *)



(*****************************************************************)
(******************* 1. Warm Up   ********************************)
(*****************************************************************)

(* sqsum : int list -> int
 * sqsum takes a list of integers and squares and adds up all the elments
 *  of the list. The result is a single int.
 * Implementation: Uses the library function fold_left to apply a function
 * that adds an accumulator and an element squared.
 * Ex: # sqsum [1;2;3;4] ;;
 *     - : int = 30 
 *)
let sqsum xs = 
  let f a x = a + (x*x)  in
  let base = 0 in
    List.fold_left f base xs

(* pipe : ('a -> 'a) list -> ('a -> 'a)
 * pipe takes a list of functions and an int. The int is passed to all the 
 * first function and the result of that function is passed to the following
 * and so on until there are no more functions left. The result of these
 * nested functions is returned.
 * Implementation: fold_left is passed a function that applies a function
 * to an element. fold_left uses this function with all element fo the list.
 * Ex: # pipe [(fun x-> 2*x);(fun x -> x + 3)] 3 ;;
 *     - : int = 9 
 *)
let pipe fs = 
  let f a x = fun y -> x (a y)  in
  let base = fun x -> x in
    List.fold_left f base fs

(* sepConcat : string -> string list -> string
 * sepConcat concatanates a list of strings into a single string, where
 * each word is separated by sep
 * Implementation: fold_left is used with a function that concatanates two 
 * elements and sep. 
 * Ex: # sepConcat "" ["a";"b";"c";"d";"e"];;
 *     - : string = "abcde"
 *)
let rec sepConcat sep sl = match sl with 
  | [] -> ""
  | h :: t -> 
      let f a x = a^sep^x in
      let base = h in
      let l = t in
        List.fold_left f base l


(* stringOfList : ('a -> string) -> 'a list -> string
 * stringOfList uses a function to make a list into  
 * a string. 
 * Implementation: It uses map to apply the function that makes
 * elements in l into a string.
 * Ex: # stringOfList string_of_int [1;2;3;4;5;6];;
 *    - : string = "[1; 2; 3; 4; 5; 6]"
 *)
let stringOfList f l = "["^sepConcat "; " (List.map f l)^"]";; 
(*****************************************************************)
(******************* 2. Big Numbers ******************************)
(*****************************************************************)

(* clone : 'a -> int -> 'a   
 * clone repeats its first parameter the number of times of its
 * second parameter
 * Implementation: uses context matching to determine how many 
 * times is x repeated. It calls rec with x and n-1.
 * Ex: # clone 3 5;;
*      - : int list = [3; 3; 3; 3; 3]
 *)
let rec clone x n = match n with
	|0-> []
	|_ -> if n<0 then [] else x::(clone x (n-1));; 

(* padZero : int list -> int list -> int list * int list  
 * padZero pads the shortest list with zeroes so it becomes
 * the length of the longer list.
 * Implementation: uses List.length to determine the difference
 * in lengths and then uses that to clone as many zeroes as that
 * difference and appends them to the short list.
 * Ex: # padZero [9;9] [1;0;0;2];;
 *     - : int list * int list = ([0;0;9;9],[1;0;0;2]) 
 *)
let rec padZero l1 l2 = let length1 = (List.length l1) in
	let length2 = (List.length l2) in
	if length1 > length2 then (l1, (List.append (clone 0 (length1-length2)) l2)) 
		else ((List.append (clone 0 (length2-length1)) l1), l2);;

(* removeZero : int list -> int list -> int list * int list   
 * removeZero removes all the initial zeroes in a list of ints.
 * Implementation: recursively uses context matching to match elements
 * that are zero. If a nonzero element is encountered, then it returns.
 * Ex: # removeZero [0;0;0;0];;
 *   - : int list = [] 
 *)
let rec removeZero l = match l with
	|[] -> []
	|h::t -> if h = 0 then removeZero t else l;;
 
(* bigAdd : int list -> int list -> int list   
 * bigAdd adds two numbers (which are in list format so that every digit 
 * is an element of the list. 
 * Implementation : uses fold_left such that it returns a tuple (acc, result)
 * In this case, acc is the carry over of the addition of two previous elements
 * in the list. The list is reversed, so we can apply the carry overs corectly
 * The list is also List.combine. The function passed adds the two elements of 
 * tuple and the carry over, it also calculates the carry over.
 * Ex: # bigAdd [9;9] [1;0;0;2];;
 *     - : int list = [1;1;0;1] 
*)
let bigAdd l1 l2 = 
  let add (l1, l2) = 
    let f a x = let (carry, sum) = a in
		let (x1, x2) = x in
                (* Calculating carry over, calculating addition *)
		((x1+x2+carry)/10, (((x1+x2+carry) mod 10)::sum)) in
    let base = (0, []) in
    let args = let list1 = List.rev (0::l1) in
	       let list2 = List.rev (0::l2) in
	       List.combine list1 list2 in (* List of tuples *)
    let (_, res) = List.fold_left f base args in
      res
  in 
    removeZero (add (padZero l1 l2))

(* mulByDigit : int -> int list -> int list  
 * mulByDigit multiplies the number represented by the list by the int
 * given. 
 * Implementation: uses a helper function that recursively calls
 * bigAdd to add l i times. (Imitating multiplication)
 * Ex: # mulByDigit 9 [9;9;9;9];;
 *     - : int list = [8;9;9;9;1] 
 *)
let rec mulByDigit i l = 
	let rec helper acc mul li = 
	  match mul with
	   | 0 -> acc
	   | _ -> if mul>0 then helper (bigAdd li acc) (mul-1) li else acc
	in helper [] i l;; 
 
(* bigMul : int list -> int list -> int list   
 * bigMul takes two lists that represent  anumber each and multiplies them.
 * The resulting list in the multiplication of both digits.
 * Implementation: uses fold_left which receives a function that calls 
 *  bigAdd, append, mulByDigit and clone to multiply the two numbers being
 * represented by the lists. 
 * Ex: # bigMul [9;9;9;9] [9;9;9;9];;
 *     - : int list = [9;9;9;8;0;0;0;1] 
 *)
let bigMul l1 l2 = 
  let f a x = let (z, sum)= a in
	(z+1, (bigAdd (List.append (mulByDigit x l1) (clone 0 z)) sum))  in
  let base = (0,[])  in
  let args = List.rev l2 in
  let (_, res) = List.fold_left f base args in
    res
