(* ------------------------------------------------------------------------*)
(* Q1 : Money in the Bank *)
(* ------------------------------------------------------------------------*)

let open_account (initial_pass: passwd) : bank_account =
  let password = ref initial_pass in 
  let balance = ref 0 in 
  let wrong_pass_count = ref 0 in
  let locked = ref false in 
  { 
    update_pass = (fun oldpass newpass -> if oldpass = !password 
                    then (locked := false; wrong_pass_count := 0; password := newpass)
                    else (wrong_pass_count := !wrong_pass_count + 1; raise wrong_pass));
    deposit = (fun pass amount -> if !wrong_pass_count >= 5 
                then (locked := true; raise too_many_failures )else 
                if (pass = !password && not !locked )then (wrong_pass_count:= 0; if amount < 0 then raise negative_amount else balance := !balance + amount) 
                else (wrong_pass_count := !wrong_pass_count + 1; raise wrong_pass)) ;
    retrieve = (fun pass amount -> if !wrong_pass_count >= 5 
                 then (locked := true; raise too_many_failures)
                 else if (pass = !password && not !locked ) then (wrong_pass_count := 0; if amount < 0 then raise negative_amount else if !balance >= amount then balance := !balance - amount else raise not_enough_balance) else (wrong_pass_count := !wrong_pass_count + 1; raise wrong_pass)) ;
    show_balance = (fun pass -> if !wrong_pass_count >= 5 then (locked := true; raise too_many_failures) else 
                     if (pass = !password && not !locked ) then (!balance) else (wrong_pass_count := !wrong_pass_count + 1; raise wrong_pass))
  
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

let find_all_paths (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) list =
  let rec aux_node (node: 'a * weight) (visited : 'a list) : ('a list * weight) list = 
    if (List.mem (fst node) visited) then []
    else if ((fst node) = b) then [([fst node], snd node)]
    else
      List.map (fun x -> (fst node :: fst x, (snd node + snd x))) (aux_list (neighbours g (fst node)) (visited @ [fst node]))
  and aux_list (nodes: ('a * weight) list) (visited: 'a list) : ('a list * weight) list =
    match nodes with
    | x :: xs -> aux_node x visited @ aux_list xs visited
    | _ -> []
  in
  aux_node (a, 0) [];; 

(* TODO: Implement find_path. *)
let find_path (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) =
  match (find_all_paths g a b) with
  | x :: xs -> x
  | _ -> raise Fail;;

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
    if (List.mem (fst node) visited) then []
    else if ((fst node) = b) then [([fst node], snd node)]
    else
      List.map (fun x -> (fst node :: fst x, (snd node + snd x))) (aux_list (neighbours g (fst node)) (visited @ [fst node]))
  and aux_list (nodes: ('a * weight) list) (visited: 'a list) : ('a list * weight) list =
    match nodes with
    | x :: xs -> aux_node x visited @ aux_list xs visited
    | _ -> []
  in
  aux_node (a, 0) [];; 
  


(* TODO: Implement find_longest_path *)
let find_longest_path (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) option =
  match List.rev (List.sort (fun a b -> if snd a > snd b then 1 else if snd b > snd a then -1 else 0) (find_all_paths g a b)) with
  | x :: xs -> Some(x)
  | _ -> None