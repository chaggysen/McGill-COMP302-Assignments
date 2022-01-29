(* Question 1 *)
let fact_tests = [
  (0, 1.);
  (1, 1.);
  (2, 2.);
  (5, 120.);
  (10, 3628800.);
]

let rec fact (n: int): float =
  if n < 0 
  then domain () 
  else match n with
    | 0 -> 1.
    | _ -> (float_of_int n) *. fact (n - 1) 


let binomial_tests = [
  (* Your test cases go here. Correct the incorrect test cases for the function. *)
  ((0, 0), 1.);
  ((1, 0), 1.);
  ((2, 0), 1.);
  ((10, 1), 10.);
  ((10, 2), 45.)
]

let binomial (n: int) (k: int) =
  if n < 0
  then domain ()
  else (if k > n
        then domain ()
        else (fact n) /. (fact k *. fact (n - k)))


let distance_tests = [ 
  (* Your test cases go here *)
  (((1, 1), (1, 1)), 0.);
  (((7, 6), (17, 6)), 10.);
  (((0, 0), (0, 0)), 0.);
  (((8, 8), (17, 6)), 9.21954445729288707);
]
;;


let distance ((x1, y1): (int * int)) ((x2, y2): (int * int)) : float =
  let dx = x2 - x1 in
  let dy = y2 - y1 in
  sqrt ((float_of_int dx *. float_of_int dx) +. (float_of_int dy *. float_of_int dy))
;;


(* Question 2: is_prime *)

let is_prime_tests = [
(* Your tests go here *)
  (2, true);
  (3, true);
  (10, false);
  (21, false)

]


let is_prime n =
  if n <= 1
  then domain() 
  else (let rec notDivisibleBy (x : int) : bool =
          if x * x > n then true
          else (n mod x != 0 && notDivisibleBy (x + 1))
        in notDivisibleBy 2)


(* Question 3: Riemann Zeta function *)

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

let fib_tl_tests = [
  (0, 1);
  (1, 1);
  (2, 2);
  (3, 3);
  (4, 5);
  (5, 8)
]

let rec fib_aux n a b =
  if n = 0 then a 
  else if n = 1 then b
  else fib_aux (n - 1) b (b + a)
        

let fib_tl n = fib_aux n 1 1

