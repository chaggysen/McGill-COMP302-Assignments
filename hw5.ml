(* TODO: Write a good set of tests for unused_vars. *)
let unused_vars_tests = [
  (* An example test case.
     Note that you are *only* required to write tests for Rec, Fn, and Apply!
  *)
  (Let ("x", I 1, I 5), ["x"]);
  (Fn ([("x", Int); ("y", Int)], Primop(Plus, [Var "y"; I 5])), ["x"]);
  (Let ("g", ex2, Apply (Var "g", [])), []);
  (Rec ("f", Int, ex2), ["f"]);
  (Apply(Let ("g", I 8, I 9), [I 7; I 8]), ["g"]);
  (Apply(Let ("g", I 8, Var "g"), [I 7]), []);
  (Rec ("f", Int, Primop(Plus, [Var "f"; I 5])), []);
  
]

(* TODO: Implement the missing cases of unused_vars. *)
let rec unused_vars =
  function
  | Var _ | I _ | B _ -> []
  | If (e, e1, e2) -> unused_vars e @ unused_vars e1 @ unused_vars e2
  | Primop (_, args) ->
      List.fold_left (fun acc exp -> acc @ unused_vars exp) [] args
  | Let (x, e1, e2) ->
      let unused = unused_vars e1 @ unused_vars e2 in
      if List.mem x (free_variables e2) then
        unused
      else
        x :: unused

  | Rec (x, _, e) -> 
      let unused = unused_vars e in
      if List.mem x (free_variables e) then
        unused
      else
        x :: unused 
        
  | Fn (xs, e) -> 
      let unused = unused_vars e in 
      delete (free_variables e) (List.map (fun (x, _) -> x) xs) @ unused

  | Apply (e, es) -> 
      let unused = unused_vars e in 
      List.concat((List.map(fun e -> unused_vars e) es)) @ unused

(* TODO: Write a good set of tests for subst. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let subst_tests : (((exp * name) * exp) * exp) list = [
  (* An example test case. If you have trouble writing test cases of the
     proper form, you can try copying this one and modifying it.
     Note that you are *only* required to write tests for Rec, Fn, and Apply!
  *)
  (((I 1, "x"), (* [1/x] *)
    (* let y = 2 in y + x *)
    Let ("y", I 2, Primop (Plus, [Var "y"; Var "x"]))),
   (* let y = 2 in y + 1 *)
   Let ("y", I 2, Primop (Plus, [Var "y"; I 1]))); 
  (((I 1, "x"), ex1), ex1); 
  (((I 1, "x"), Rec("x", Int, Var "x")), Rec("x", Int, Var "x"));
  (((I 1, "x"),Rec ("f", Int, Primop(Plus, [Var "x"; I 5]))),
   Rec ("f", Int, Primop(Plus, [I 1; I 5])));
  (((I 1, "x"),Apply (Var "f", [I 3])), Apply (Var "f", [I 3])); 
  (((Let("y", I 2, Primop (Times, [Var "x"; Var "y"])), "y"), Fn ([("x", Int)], Primop (Plus, [Var "y"; Var "x"]))),
   Fn ([("x1", Int)], Primop (Plus,[Let("y", I 2, Primop (Times, [Var "x"; Var "y"])); Var "x1"]))); 
  (((Let("y", I 2, Primop (Plus, [Var "x"; Var "y"])), "y"), Rec ("x", Int, Fn ([("x", Int)], Primop (Plus, [Var "y"; Var "x"])))),
   Rec ("x1", Int, Fn ([("x1", Int)], Primop (Plus, [Let("y", I 2, Primop (Plus, [Var "x"; Var "y"])); Var "x1"]))));
  (((Let("y", I 2, Primop (Times, [Var "x"; Var "y"])), "y"), Apply (Fn ([("x", Int)], Primop (Plus, [Var "y"; Var "x"])), [Var "x"; Var "y"])),
   Apply (Fn ([("x1", Int)], Primop (Plus, [Let("y", I 2, Primop (Times, [Var "x"; Var "y"])); Var "x1"])), [Var "x"; Let("y", I 2, Primop (Times, [Var "x"; Var "y"]))]))
  
]
  
      
(* TODO: Implement the missing cases of subst. *)
let rec subst ((e', x) as s) exp =
  match exp with
  | Var y ->
      if x = y then e'
      else Var y
  | I n -> I n
  | B b -> B b
  | Primop (po, args) -> Primop (po, List.map (subst s) args)
  | If (e, e1, e2) ->
      If (subst s e, subst s e1, subst s e2)
  | Let (y, e1, e2) ->
      let e1' = subst s e1 in
      if y = x then
        Let (y, e1', e2)
      else
        let (y, e2) =
          if List.mem y (free_variables e') then
            rename y e2
          else
            (y, e2)
        in
        Let (y, e1', subst s e2)

  | Rec (y, t, e) -> 
      if x = y then Rec (y, t, e)
      else 
      if List.mem y (free_variables e') then
        let (y, e1) = rename y e in 
        Rec (y, t, subst s e1)
      else
        Rec (y, t, subst s e) 
          
  | Apply (e, es) -> Apply (subst s e, (List.map(fun e -> subst s e) es) )

  | Fn (xs, e) -> 
      let (new_names, new_e) = rename_all (List.map (fun (x, _) -> x) xs) e in
      Fn((List.map2(fun a b -> (a, snd b)) new_names xs), subst s new_e) 

and rename x e =
  let x' = freshVar x in
  (x', subst (Var x', x) e)

and rename_all names exp =
  List.fold_right
    (fun name (names, exp) ->
       let (name', exp') = rename name exp in
       (name' :: names, exp'))
    names
    ([], exp)

(* Applying a list of substitutions to an expression, leftmost first *)
let subst_list subs exp =
  List.fold_left (fun exp sub -> subst sub exp) exp subs
    
(* TODO: Write a good set of tests for eval. *)
let eval_tests = [
  (* An example test case.
     Note that you are *only* required to write tests for Rec and Apply!
  *)
  (Let ("x", I 1, Primop (Plus, [Var "x"; I 5])), I 6);
  (Rec ("x", Int, Primop (Plus, [I 1; I 1])), I 2);
  (Apply (Fn ([("x", Int)], Primop (Plus, [I 1; I 1])), [I 1]), I 2);
  (Apply (Fn ([("x", Int); ("y", Int)], Primop (Plus, [I 1; I 1])), [I 1; I 1]), I 2);
  (Apply (Fn ([("x", Int); ("y", Int)], Primop (Plus, [Var "x"; Var "y"])), [I 2; I 3]), I 5);
  (Apply (Let ("x", I 1, Fn ([("y", Int)], Primop (Plus, [I 1; I 1]))), [I 2]), I 2);
  (Apply (Fn ([], Primop (Plus, [I 1; I 1])), []), I 2);
  (Let ("y", I 2, Rec ("x", Int, Primop (Plus, [Var "y"; I 1]))), I 3)
  

]

(* TODO: Implement the missing cases of eval. *)
let rec eval exp =
  match exp with
  (* Values evaluate to themselves *)
  | I _ -> exp
  | B _ -> exp
  | Fn _ -> exp

  (* This evaluator is _not_ environment-based. Variables should never
     appear during evaluation since they should be substituted away when
     eliminating binding constructs, e.g. function applications and lets.
     Therefore, if we encounter a variable, we raise an error.
*)
  | Var x -> raise (Stuck (Free_variable x))

  (* Primitive operations: +, -, *, <, = *)
  | Primop (po, args) ->
      let args = List.map eval args in
      begin
        match eval_op po args with
        | None -> raise (Stuck Bad_primop_args)
        | Some v -> v
      end

  | If (e, e1, e2) ->
      begin
        match eval e with
        | B true -> eval e1
        | B false -> eval e2
        | _ -> raise (Stuck If_non_true_false)
      end

  | Let (x, e1, e2) ->
      let e1 = eval e1 in
      eval (subst (e1, x) e2)

  | Rec (f, t, e) -> eval (subst (Rec (f, t, e), f) e)

  | Apply (e, es) ->
      match (eval e) with 
      | Fn (xs, f_exp) -> let vs =  List.map(fun a_exp -> eval a_exp) es in
          let ns = List.map(fun (n, _) -> n) xs in
          if List.length vs = List.length ns then let sub_rules = List.combine vs ns in eval (subst_list sub_rules f_exp) 
          else raise (Stuck Arity_mismatch)
      | _ -> raise (Stuck Apply_non_fn)

(* TODO: Write a good set of tests for infer. *)
let infer_tests = [
  (* An example test case.
     Note that you are *only* required to write tests for Rec, Fn, and Apply!
  *)
  (([("x", Int)], Var "x"), Int);
  (([("y", Int)], Rec ("x", Int, Primop (Plus, [Var "x"; Var "y"]))), Int);
  (([], Fn([("x", Int)], Primop(Plus, [Var "x"; Var "x"]))), Arrow ([Int], Int));
  (([], Fn([("x", Int); ("y", Int)], Primop(Plus, [Var "x"; Var "y"]))), Arrow ([Int; Int], Int)); 
  (([], Fn([], Primop(Plus, [I 1; I 1]))), Arrow ([], Int));
  (([], Apply(Fn ([("x", Int)], Primop (Equals, [Var "x"; I 3])), [I 3])), Bool);
  (([], Apply(Fn ([("x", Int);("y", Int)], Primop (Equals, [Var "x"; Var "y"])), [I 3; I 3])), Bool);
  (([], Apply(Fn ([], Primop (Equals, [I 1; I 1])), [])), Bool)
]

(* TODO: Implement the missing cases of infer. *)
let rec infer ctx e =
  match e with
  | Var x ->
      begin
        try lookup x ctx
        with Not_found -> raise (TypeError (Free_variable x))
      end
  | I _ -> Int
  | B _ -> Bool

  | Primop (po, exps) ->
      let (domain, range) = primopType po in
      check ctx exps domain range

  | If (e, e1, e2) ->
      begin
        match infer ctx e with
        | Bool ->
            let t1 = infer ctx e1 in
            let t2 = infer ctx e2 in
            if t1 = t2 then t1
            else type_mismatch t1 t2
        | t -> type_mismatch Bool t
      end

  | Let (x, e1, e2) ->
      let t1 = infer ctx e1 in
      infer (extend ctx (x, t1)) e2

  | Rec (f, t, e) -> let tstar = (infer (extend ctx (f, t)) e) in
      if t = tstar then t else type_mismatch t tstar
  | Fn (xs, e) -> let tstar = (infer (extend_list ctx xs) e) in
      Arrow(List.map(fun (_, t) -> t) xs, tstar)

  | Apply (e, es) -> let fnType = (infer ctx e) in 
      let argsType = List.map(fun exp -> (infer ctx exp)) es in
      match fnType with
      | Arrow(t, tstar) -> 
          if (List.compare_lengths t argsType) = 0 then (
            let pairs = List.combine t argsType in
            let equalTypes = List.map (fun (t1, t2) -> t1 = t2) pairs in
            let looking_for (a, b) = (a != b) in 
            if List.exists (fun x -> x = false) equalTypes then 
              let (expectedt, actualt) = List.find looking_for pairs in 
              type_mismatch expectedt actualt
            else tstar
          )    
          else raise (TypeError (Arity_mismatch))
      | _ -> raise (TypeError (Apply_non_arrow (fnType)))

and check ctx exps tps result =
  match exps, tps with
  | [], [] -> result
  | e :: es, t :: ts ->
      let t' = infer ctx e in
      if t = t' then check ctx es ts result
      else type_mismatch t t'
  | _ -> raise (TypeError Arity_mismatch)

(* TODO: Implement type unification. *)
let unify : utp -> utp -> utp UTVarMap.t =
  let rec unify (substitution : utp UTVarMap.t) (t1 : utp) (t2 : utp) : utp UTVarMap.t =
    match t1, t2 with
    (* Unifying identical concrete types does nothing *)
    | UInt, UInt
    | UBool, UBool -> substitution
    | UTVar a, UTVar a' when a = a' -> substitution

    (* For type constructors, recursively unify the parts *)
    | UArrow (t1, t1'), UArrow (t2, t2') ->
        let newsub = unify substitution t1 t2 in
        unify newsub t1' t2';

    | UTVar a, _ -> unifyVar substitution a t2
    | _, UTVar b -> unifyVar substitution b t1
    (* All other cases are mismatched types. *)
    | _, _ -> unif_error @@ UnifMismatch (t1, t2)
  
  (* Unify a variable with a type *)
  and unifyVar (substitution : utp UTVarMap.t) (a : string) (t : utp) : utp UTVarMap.t =
    let rec occurs : utp -> bool = function
      | UInt | UBool -> false
      | UArrow (t1, t2) -> occurs t1 || occurs t2
      | UTVar b ->
          if a = b
          then true
          else
            match UTVarMap.find_opt b substitution with
            | None -> false
            | Some t' -> occurs t'
    in 
    if occurs t then unif_error UnifOccursCheckFails 
    else
      match UTVarMap.find_opt a substitution with 
      | None -> UTVarMap.add a t substitution 
      | Some t' -> unify substitution t t';

  in fun t1 t2 -> unify UTVarMap.empty t1 t2