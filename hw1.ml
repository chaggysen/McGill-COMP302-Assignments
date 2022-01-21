(* Question 1 *)
(* TODO: Write your own tests for the fact function.
         See the provided tests for double, above, for how to write test cases.
         Remember that you should NOT test cases for n < 0.
*)
(* TODO: Correct these tests for the fact function. *)
let fact_tests = [
  (0, 1.);
  (1, 1.);
  (2, 2.);
  (5, 120.);
  (10, 3628800.);
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let rec fact (n: int): float =
  if n < 0 
  then domain () 
  else match n with
    | 0 -> 1.
    | _ -> (float_of_int n) *. fact (n - 1) 


(* TODO: Write your own tests for the binomial function.
         See the provided tests for fact, above, for how to write test cases.
         Remember that we assume that  n >= k >= 0; you should not write test cases where this assumption is violated.
*)
let binomial_tests = [
  (* Your test cases go here. Correct the incorrect test cases for the function. *)
  ((0, 0), 1.);
  ((1, 0), 1.);
  ((2, 0), 1.);
  ((10, 1), 10.);
  ((10, 2), 45.)
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let binomial (n: int) (k: int) =
  if n < 0
  then domain ()
  else (if k > n
        then domain ()
        else (fact n) /. (fact k *. fact (n - k)))


(* TODO: Write a good set of tests for distance. *)
let distance_tests = [ 
  (* Your test cases go here *)
  (((1, 1), (1, 1)), 0.);
  (((7, 6), (17, 6)), 10.);
  (((0, 0), (0, 0)), 0.);
  (((8, 8), (17, 6)), 9.21954445729288707);
]
;;

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let distance ((x1, y1): (int * int)) ((x2, y2): (int * int)) : float =
  let dx = x2 - x1 in
  let dy = y2 - y1 in
  sqrt ((float_of_int dx *. float_of_int dx) +. (float_of_int dy *. float_of_int dy))
;;


(* Question 2: is_prime *)

(* TODO: Write a good set of tests for is_prime. *)
let is_prime_tests = [
(* Your tests go here *)
  (2, true);
  (3, true);
  (10, false);
  (21, false)

]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let is_prime n =
  if n <= 1
  then domain() 
  else (let rec notDivisibleBy (x : int) : bool =
          if x * x > n then true
          else (n mod x != 0 && notDivisibleBy (x + 1))
        in notDivisibleBy 2)


(* Question 3: Riemann Zeta function 
    Implement the `approx_zeta` function.
    You do not need to modify any other parts inside the `zeta` function
*)

let zeta_tests = [
    (* Your tests go here *)
  (3., 1.20205690314079017);
  (5., 1.03692775514329338);
  (10., 1.00099457512781753);
  (20., 1.00000095396203381)
] 
;;
  
let zeta (k: float) : float = 
  let rec approx_zeta k acc n sum_so_far = 
    if (n ** -.k) < acc then sum_so_far
    else approx_zeta k acc (n +. 1.) (sum_so_far +. (n ** -.k))
  in
      (*  Note that we put < 2. while the function still works 
          to evaluate any smaller arguments *)
  if k < 2. 
  then domain () 
  else approx_zeta k epsilon_float 1. 0.
;;


(* Question 4: Fibonacci*)

(* TODO: Write a good set of tests for fib_tl. *)
let fib_tl_tests = [
  (0, 1);
  (1, 1);
  (2, 2);
  (3, 3);
  (4, 5);
  (5, 8)
]

(* TODO: Implement a tail-recursive helper fib_aux. *)
let rec fib_aux n a b =
  if n = 0 then a 
  else if n = 1 then b
  else fib_aux (n - 1) b (b + a)
        

(* TODO: Implement fib_tl using fib_aux. *)
let fib_tl n = fib_aux n 1 1

