(* CSE 130: Programming Assignment 1
 * misc.ml
 *)

(* sumList : int list -> int 
 * (sumList l) is the summation of all the int elements in 
 * list l.
 * Implementation: adding the head value of the list to the 
 *    recursive call of sumList with the list tail as parameter.
 *    This is correct, because we reach all the elements of the 
 *    list and once the tail is the empty list, we obtain 0 which
 *    makes no difference to the end result. 
 * e.g. (sumList [1;2;3;4]) is 10
 *      (sumList [5;6;-2;1]) is 10
*) 

let rec sumList l =
	match l with
	| [] -> 0         (* Giving [] value 0 *)
        | h::t -> h + sumList(t);; (* Recursively adding all elements *)


(* Helper function for digitsOfInt. It appends two lists
 * append: 'a list -> a' list -> a' list
 * (append l1 l2) appends the two lists l1 and l2 together, with the
 * elements of l1 before l2.
 * Implementation: The head of the l1 is put together to the beginning
 *    of the list. By calling recursively on append with l1's tail and l2
 *    we make sure that all of l1 is included in the final list in the 
 *    desired order. Once we reach the end of l1, we concatanate l2.
 * e.g. (append [1;2] [3;4]) is [1;2;3;4]
 *)
let rec append l1 l2 = 
     match l1 with
       | [] -> l2   (* Reached end of l1, l2 is good as it is*)
       | h::t -> h::(append t l2);;

(* digitsOfInt : int -> int list
 * (digitsOfInt n) is a list that contains all the digits of number
 * n in their appropriate order.
 * Implementation: Appending n mod 10 to the end of a list with a 
 *    recursive call to digitsOfInt on n/10. This assures that the 
 *    list contains all the digits of n in their appropriate order.
 * e.g. (digitsOfInt 9876) is [9;8;7;6]
 *      (digitsOfInt 2354) is [2;3;5;4]
*)
let rec digitsOfInt n = 
     if n > 0 then (append (digitsOfInt(n/10)) [n mod 10]) else [];; (*appending recursive call with tail to the front*)


(* digits : int -> int list
 * (digits n) is the list of digits of n in the order in which they appear
 * in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *      (digits (-23422) is [2,3,4,2,2]
 *)
 
let digits n = digitsOfInt (abs n)


(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits, 
 * then adding the digits of the number derived from it, etc., 
 * until the remaining number has only one digit. 
 * The number of additions required to obtain a single digit from a number n 
 * is called the additive persistence of n, and the digit obtained is called 
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so 
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)


(* additivePersistence : int -> int
 * (additivePersistence n) is the number of additions required to obtain a 
 *  single digit from a number n.
 * Implementation: Makes use of digitsOfInt and sumList to perform digit  
 *    summation of n. We add 1 everytime that addtivePersistence is called 
 *    recursively, therefore we obtain all the times that a summation of
 *    digits was performed before reaching the single digit.
 *
 *  e.g. (additivePersistence 9876) is 2
 *)

let rec additivePersistence n = 
   let q = digitsOfInt(n) in   (* Converting number to a digit list*)
	match q with
	| [] -> 0               (* avoiding warning*)
	| h::t -> if t != [] then 1 + additivePersistence(sumList(q)) (*recursive call on number resulting from digit summation*)
			     else 0;;                                   
(* digitalRoot : int -> int 
 * (digitalRoot n) is the digit obtained after adding all the digits of n
 *  until a single digit is reached.
 *  Implementation: Makes use of digitsOfInt and sumList to perform digit
 *    summation of n. We use recursion until we reach a list of 1 element
 *    The 1 element is the digital root of 1.
 *
 * e.g. (digitalRoot 9876) is 3
*)
let rec digitalRoot n =
   let q = digitsOfInt(n) in  (*converting number to digit list*)
	match q with
	| [] -> 0
	| h::t -> if t != [] then digitalRoot(sumList(q)) 
			     else sumList(q);;    (*reached digital root*)
(* listReverse : a' list -> a' list 
 * (listReverse l) is the reversed version of the list l.
 * Implementation: append is used to append the head of the 
 *    list with the recursive call on the tail. The head is
 *    always appended to the end of the list, which reverses
 *    the list by having the head(s) at the end.
 *
 * e.g. (listReverse [1;2;3;4;5]) is [5;4;3;2;1]
 *      (listReverse [-1;3;-5;6]) is [6;-5;3;-1]
*)
let rec listReverse l =
   match l with
   | [] -> []
   | h::t -> if t = [] then [h] else (append (listReverse(t)) [h]);; (*Using append to append recursive call with tail with head*)

(* explode : string -> char list 
 * (explode s) is the list of characters in the string s in the order in 
 *   which they appear
 * e.g.  (explode "Hello") is ['H';'e';'l';'l';'o']
 *)
let explode s = 
  let rec _exp i = 
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0

(* palindrome : string -> bool
 * (palindrome w) is the result of whether string w is a palindrome
 *  or not. If w is a palindrome, then true, else false.
 * Implementation: Uses explode and listReverse. Explode makes the string
 *    into a list and we save it into temp variable x, then we use x as a 
 *    parameter for listReverse and save the result to y. Then x and y are
 *    checked for equality. 
 * e.g. (palindrome "ashley") is false
 *      (palindrome "malayalam") is true
*)
let palindrome w =
   let x = (explode(w)) in   (*Using explode to get w as list*)
      let y = (listReverse(x)) in    (*Using listReverse to obtain reversed list*)
         if y = x then true else false;; (*Checking for equality*)

(************** Add Testing Code Here ***************)
