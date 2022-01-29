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
  notimplemented ()
;;

let add_speed (a : speed_unit value) ((b_unit, b_val) : speed_unit value) : speed_unit value = 
  notimplemented ()
;;

let dist_traveled time ((speed_unit, speed_val) : speed_unit value) : dist_unit value = 
  notimplemented ()
;;

(* Section 3 : recursive data types/induction *)

let passes_da_vinci_tests : (tree * bool) list = [] ;;

let rec passes_da_vinci t = 
  notimplemented ()
;;