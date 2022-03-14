(* ------------------------------------------------------------------------*)
(* Q1 : Money in the Bank *)
(* ------------------------------------------------------------------------*)

let open_account (initial_pass: passwd) : bank_account =
  let password = ref initial_pass in 
  let balance = ref 0 in 
  let wrong_pass_count = ref 0 in
  { 
    update_pass = (fun oldpass newpass -> if oldpass = !password 
                    then password := newpass 
                    else (wrong_pass_count := !wrong_pass_count + 1; raise wrong_pass));
    deposit = (fun pass amount -> if !wrong_pass_count > 5 
                then raise too_many_failures else 
                if pass = !password then (wrong_pass_count:= 0; if amount < 0 then raise negative_amount else balance := !balance + amount) 
                else (wrong_pass_count := !wrong_pass_count + 1; raise wrong_pass)) ;
    retrieve = (fun pass amount -> if !wrong_pass_count > 5 
                 then raise too_many_failures 
                 else if pass = !password then (wrong_pass_count := 0; if amount < 0 then raise negative_amount else if !balance >= amount then balance := !balance - amount else raise not_enough_balance) else (wrong_pass_count := !wrong_pass_count + 1; raise wrong_pass)) ;
    show_balance = (fun pass -> if !wrong_pass_count > 5 then raise too_many_failures else 
                     if pass = !password then (!balance) else (wrong_pass_count := !wrong_pass_count + 1; raise wrong_pass))
  
  }
;;

(* ------------------------------------------------------------------------*)
(* Q2 : I Want to Travel *)
(* ------------------------------------------------------------------------*)
(* TODO: Write some tests for neighbours. Consider creating a graph first,
 and then writing your tests based on it *)

(* Reminder: If a test case requires multiple arguments, use a tuple:
let myfn_with_2_args_tests = [
  ((arg1, arg1), (expected_output))
]
*)

let g1 = {nodes = ["a"; "b"]; edges = [("a", "b", 1)]};;
let g2 = {nodes = ["a"; "b"]; edges = []};;

(* We've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let neighbours_tests: ((string graph * string) * (string * weight) list) list = [
  ((g1, "a"), [("b", 1)]);
  ((g2, "a"), [])
]

(* TODO: Implement neighbours. *)
let neighbours (g: 'a graph) (vertex: 'a) : ('a * weight) list = 
  List.map (fun (_, v2, w) -> (v2, w)) (List.filter (fun (v1, _, _) -> v1 = vertex) g.edges);;
  

(* TODO: Implement find_path. *)
let find_path (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) =
  let rec aux_node (node: 'a * weight) (visited : 'a list) : ('a list * weight) =
    notimplemented ()
  and aux_list (nodes: ('a * weight) list) (visited: 'a list) : ('a list * weight) =
    notimplemented ()
  in
  notimplemented ()

(* TODO: Implement find_path'. *)
let find_path' (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) =
  let rec aux_node (node: 'a * weight) (visited : 'a list) fc sc : ('a list * weight)=
    notimplemented ()
  and aux_list (nodes: ('a * weight) list) (visited: 'a list) fc sc : ('a list * weight) =
    notimplemented ()
  in
  notimplemented ()


(* TODO: Implement find_all_paths *)
let find_all_paths (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) list =
  let rec aux_node (node: 'a * weight) (visited : 'a list) : ('a list * weight) list =
    notimplemented ()
  and aux_list (nodes: ('a * weight) list) (visited: 'a list) : ('a list * weight) list =
    notimplemented ()
  in
  notimplemented ()


(* TODO: Implement find_longest_path *)
let find_longest_path (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) option =
  notimplemented ()