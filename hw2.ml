(* Section 1 : Lists *)

(* Question 1.1 : Most common element of *sorted* list *)
let error () = 
  failwith "List is empty."

let mode_tests: (int list * int) list = [
  ([2; 2; 1; 1; 1; 3], 1);
  ([2; 3; 2; 3; 1; 1; 1; 1], 1);
  ([2; 2; 2; 1; 1; 1], 1); 
  ([3; 3; 3; 1; 1; 1; 2; 2; 2], 1);
  ([100; 100; 20; 20; 20 ; 1; 1; 1; 50; 50], 1); 
  ([100; 100; 100; 100; 20; 20; 20 ; 1; 1; 1; 50; 50], 100); 
  ([100; 100; 100; 20; 20; 20 ; 1; 1; 1; 50; 50], 1);
  ([2; 2; 2; 0; 0; 0; 0], 0);
  ([2], 2)
] ;;


let mode (l: 'a list) : 'a =
  let rec aux l ((cur_el, cur_num) : 'a * int) ((max_el, max_num) : 'a * int) =
    match l with 
    | [] -> if cur_num > max_num then cur_el else max_el
    | x :: xs -> (if x = cur_el then aux xs (cur_el, cur_num + 1) (max_el, max_num)
                  else (if cur_num > max_num then aux xs (x, 1) (cur_el, cur_num)
                        else aux xs (x, 1) (max_el, max_num))) 
  in
  match l with 
  | [] -> notimplemented ()
  | x :: xs -> aux (List.sort compare l) (x, 0) (x, 0)
;;

(* Question 1.2 : Most common consecutive pairing *)

let pair_mode_tests: (int list * (int * int) ) list = [
  ([1; 5; 8], (1, 5));
  ([8; 5; 1], (5, 1));
  ([2; 2; 1; 1; 1; 3], (1, 1))
] ;;


let rec insert_at_end l i =
  match l with
    [] -> [i]
  | h :: t -> h :: (insert_at_end t i)
                   
let pair_mode (l: 'a list) : 'a * 'a = 
  let rec aux old_l new_l prev = 
    match old_l with 
    | [] -> new_l
    | x :: xs -> aux xs (insert_at_end new_l (prev, x)) x
  in
  match l with
  | [] -> notimplemented () 
  | x :: xs -> mode (aux xs [] x)
;;


(* Section 2 : Custom data types *)

let convert_time ((from_unit, val_) : time_unit value) to_unit : time_unit value = 
  match from_unit with 
  | Second -> (if to_unit = Second then (Second, val_)
               else if to_unit = Hour then (Hour, val_ /. 3600.)
               else notimplemented())
  | Hour -> (if to_unit = Second then (Second, val_ *. 3600.)
             else if to_unit = Hour then (Hour, val_)
             else notimplemented())
  | _ -> notimplemented()
    
let convert_dist ((from_unit, val_) : dist_unit value) to_unit : dist_unit value =
  match from_unit with 
  | Foot -> (if to_unit = Foot then (to_unit, val_)
             else if to_unit = Meter then (to_unit, val_ *. 0.3048)
             else if to_unit = Mile then (to_unit, val_ /. 5280.)
             else notimplemented())
  | Meter -> (if to_unit = Foot then (to_unit, val_ /. 0.3048)
              else if to_unit = Meter then (to_unit, val_)
              else if to_unit = Mile then (to_unit, val_ /. 0.3048 /. 5280.)
              else notimplemented()) 
  | Mile -> (if to_unit = Foot then (to_unit, val_ *. 5280.)
             else if to_unit = Meter then (to_unit, val_ *. 5280. *. 0.3048)
             else if to_unit = Mile then (to_unit, val_)
             else notimplemented())
  | _ -> notimplemented()
;;

let convert_speed ((from_unit, val_) : speed_unit value) to_unit : speed_unit value =
  let distance = (convert_dist (fst from_unit, val_) (fst to_unit)) in 
  let time = (convert_time (snd from_unit, 1.) (snd to_unit)) in
  ((fst distance, fst time), (snd distance) /. (snd time) )
;;

let add_speed (a : speed_unit value) ((b_unit, b_val) : speed_unit value) : speed_unit value = 
  let a_unit = fst a in
  let a_val = snd a in 
  let speed = convert_speed (a_unit, a_val) b_unit in 
  (b_unit, b_val +. snd speed)
;;

let dist_traveled time ((speed_unit, speed_val) : speed_unit value) : dist_unit value = 
  let time_unit = fst time in 
  let time_val = snd time in 
  let traveled_time = convert_time (time_unit, time_val) (snd speed_unit) in
  (fst speed_unit, speed_val *. snd traveled_time)
;;

(* Section 3 : recursive data types/induction *)

let passes_da_vinci_tests : (tree * bool) list = [
  (Branch (5., [
       Branch (3., [Leaf; Leaf; Leaf]);
       Leaf;
       Branch (4., [])
     ]), true); 
  (Branch (5., [
       Branch (3., [Branch(2., []); Branch(4., []); Leaf]);
       Leaf;
       Branch (4., [])
     ]), false);
  (Branch (5., [
       Branch (8., [Leaf; Leaf; Leaf]);
       Leaf;
       Branch (4., [])
     ]), false); 
  (Branch (5., [
       Branch (3., [Branch (4., []); Leaf; Leaf]);
       Leaf;
       Branch (4., [])
     ]), false);
  (Branch (0., []), true); 
  (Leaf, true);
  (Branch(0., [Leaf]), true);
  (Branch(0., [Branch(1., [])]), false);
  (Branch(9., [Branch(1., [Branch(9., [])])]), false)
] ;;

let rec sum_of_squares list sum = 
  match list with
  | [] -> sum
  | Leaf :: tl -> sum_of_squares tl sum
  | Branch (width, subtree) :: tl -> sum_of_squares tl (sum +. (width *. width)) 
                                       
let rec checker list = 
  match list with 
  | [] -> true
  | Leaf :: tl -> checker tl
  | Branch (width, subtree) :: tl -> (
      let branch_width_square = width *. width in 
      let child_sum_of_square = sum_of_squares subtree 0. in 
      (if child_sum_of_square > branch_width_square or (not (checker subtree)) then false
       else checker tl)
    )
    
let rec passes_da_vinci t = 
  checker [t]
    
;;