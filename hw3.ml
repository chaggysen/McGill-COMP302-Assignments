(*--------------------------------------------------------------*)
(* Question 1 : String to Characters to String                  *)
(*--------------------------------------------------------------*)

(* 1.1 Turn a string into a list of characters. *)
let string_explode (s : string) : char list = 
  let idxs = tabulate (fun x -> x) (String.length s) in
  List.map (fun x -> String.get s x) idxs
  

(* 1.2 Turn a list of characters into a string. *)
let string_implode (l : char list) : string =
  List.fold_left (^) "" (List.map(fun c -> Char.escaped c) l)


(*--------------------------------------------------------------*)    
(* Question 2: unfolding is like folding in reverse             *)
(*--------------------------------------------------------------*)
                     
(* 2.1 Compute the even natural numbers up to an exclusive limit. *)
let evens (max : int) : int list = 
  if max = 0 then []
  else
    0 :: unfold (fun x -> (x + 2, x + 2)) (fun x -> (x + 2) >= max) 0

(* 2.2 Compute the fibonacci sequence up to an exclusive limit. *)
let fib (max : int) : int list =
  if max <= 1 then []
  else
    1 :: unfold (fun (a,b) -> (a + b, (b, (a + b)))) (fun (a,b) -> (a + b) >= max) (0, 1)

(* 2.3 Compute Pascal's triangle up to a maximum row length. *)
let next_row row =
  List.map2 (+) ([0] @ row) (row @ [0])
    
let pascal (max : int) : int list list = 
  if max = 0 then [] 
  else 
    unfold (fun (i, row) -> row, (i + 1, (next_row row))) (fun (i, row) -> i = max) (0, [1])

(* 2.4 Implement zip, which converts two lists into a list of tuples.
       e.g. zip [1; 2] ['a'; 'c'] = [(1, 'a'); (2, 'c')]
       Note that if one list is shorter than the other, then the 
       resulting list should have the length of the smaller list.     *)

let zip (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list = 
  unfold (fun (l1, l2) -> let (x::xs,y::ys) = (l1, l2) in ((x, y), (xs, ys))) (fun (l1, l2) -> match l1, l2 with | [], _ -> true | _, [] -> true | _ -> false)(l1, l2)

                  
(*--------------------------------------------------------------*)
(* Question 3 : Let's *safely* have cake!                       *)
(*--------------------------------------------------------------*)

(* 3. Return the cupcakes from the cupcake list that contain none of the 
      allergens.                                                         *)
  
let allergy_free (allergens : ingredient list) (cupcakes : cupcake list)
  : cupcake list = 
  if allergens = [] then cupcakes
  else List.filter (fun cup -> not (let Cupcake(a,b,c,d)=cup in List.exists(fun ing -> (
        List.for_all(fun i -> List.mem i allergens) [ing] 
      ))d)) cupcakes
      