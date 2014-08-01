(* top-level expression and evaluation
1 + 3 ;;
2.0 +. 4.0 ;;
2.3 <= 3.4 ;;
24 == (23 + 1) ;;

*)

(* top-level declaration *)
let x = 1 
let y = x + x
let s = "this is a string"
let c = 'a'
let pi = 3.14 
let bvalue = (x > y) 

(* about if expression

if bvalue 
  then x
  else y

if (x > y) then y - 1
   else if (x < y) then x + 1
  else x + y  
*)

(* Simple function declarations *)
let f x = x + 1 
let g y = y - 1 


(* Recursive function declaration *)
let rec fac x =
  if (x > 1) then x * fac (x-1)
  else 1

let rec isOdd x =
  if (x == 0) then false
  else isEven (x-1) 
and isEven x =
  if (x == 0) then true
  else isOdd (x-1) 

(* Pattern matching *)

let rec fib n =
  match n with
    | 0 -> 1
    | 1 -> 1 
    | x -> fib(n-1) + fib(n-2) 

(* tuple .ie., cartesian product *)
let t1 : int * int = (3,5) 
let t2 = ("moo", true, 'q')
let t3 = () 

(* pattern matching again *)

let pairup  x y =
  match (x,y) with
    | (0,y) -> y-1
    | (x,0) -> x-1
    | (a,b) -> a+b

(* how many arguments does this take? *) 
let rec gcd (a,b) : int =
  if (b = 0) then a
  else gcd (b, a mod b) 
          

(* Local declarations *)
let dist (x1,y1) (x2,y2) =
  let y_axis = y2 -. y1 in 
  let x_axis = x2 -. x1 in 
  let z = x_axis *. x_axis +. y_axis *. y_axis in 
  sqrt(z)   

let dist1 (x1,y1) (x2,y2) =
  let hv a b = (b -. a) *. (b -. a)  in
  let z = (hv x1 x2) +. (hv y1 y2) in
  sqrt (z) 
 
(* list data structure
   [] is the empty list
   :: is the cons operator
   @  is the append operator
   [1; 2; 3] is three-element list
*)
let l1 = [1; 2; 3; 4; 5]
let l2 = List.map f l1 

let rec rev lst =
  match lst with
      [] -> []
    | hd :: tl -> rev(tl) @ [hd]

(* the following reverse function uses accumulator *) 
let rec rev1 lst =
  let rec rev l acc =
    match l with
        []  -> acc
      | hd :: tl -> rev tl (hd :: acc)
  in rev lst []

(* Please take a look at the List module in Ocaml *)

(* How to split a list into two lists which the odd numbered
   list elements stored in the first list, and the even numbered
   list elements stored in the second list.
*)

let split lst  =
  let rec split_acc l ac1 ac2 =
    match l with
      | a :: b :: ls ->
            split_acc ls (a::ac1) (b::ac2)
      | a :: ls -> split_acc ls (a :: ac1) ac2
      | [] -> (ac1, ac2)
  in let (l1,l2) = split_acc lst [] []
  in (l1,l2)

(* redefine factorial function using a reference cell and a for loop *)

let factorial n =
  let result = ref 1 in
  for i = 2 to n do
    result := i * !result
  done ;
  !result 

(* Anonymous functions
  fun x -> x + 1
  fun x y ->
   match (x,y) with
    | (0,y) -> 0
    | (x,0) -> 1
    | (x,y) -> max x y 
*)
(* higher order function *)
let compose f g x = f ( g (x))
let newf = compose f f 

(* The power of function f^n *)
let rec power f n =
  if n = 0 then fun x -> x
  else compose f (power f (n - 1)) 

(* Enumerated datatype *)

type suit = Spades | Diamonds | Hearts | Clubs
(* Note: type identifier begins with lowercase,
         data constructor begins with uppercase *)
let card = Clubs 

type btree = Leaf of int 
                | Branch of btree * btree 

type expression =
  | Num of int
  | Var of string
  | Let of string * expression * expression
  | Binop of string * expression * expression;;

(*
let rec eval env exp =
  match exp with 
      Num i -> i 
   ...
*)

let rec eval env = function
  | Num i -> i
  | Var x -> List.assoc x env
  | Let (x, e1, in_e2) ->
        let val_x = eval env e1 in
       eval ((x, val_x) :: env) in_e2
  | Binop (op, e1, e2) ->
        let v1 = eval env e1 in
        let v2 = eval env e2 in
       eval_op op v1 v2
and eval_op op v1 v2 =
    match op with
      | "+" -> v1 + v2
      | "-" -> v1 - v2
      | "*" -> v1 * v2
      | "/" -> v1 / v2
      | _ -> failwith ("Unknown operator: " ^ op) ;;

(* 
eval [] (Let ("x", Num 2, Binop ("+", Var "x", Var "x")))
*)
