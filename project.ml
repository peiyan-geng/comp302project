(** Part 1: Parsing *)
let parse_exp_tests : (string * exp option) list = [
  ("", None);
  ("1", Some (ConstI 1));
  ("1 + 1", Some (PrimBop (ConstI 1, Plus, ConstI 1)));
  ("-1", Some (PrimUop (Negate, ConstI 1)));
  
  ("true", Some (ConstB true)); 
  ("if true then 1 else 2", Some (If ((ConstB true), (ConstI 1), (ConstI 2))));
  
  ("1, 2", Some (Comma (ConstI 1, ConstI 2))); 
  ("1 2", Some (Apply (ConstI 1, ConstI 2))); 
  ("rec x => 1", Some (Rec ("x", None, ConstI 1)));
  
  ("let x = 1 in x end", Some (Let ("x", ConstI 1, Var "x"))); 
  ("x", Some (Var "x")); 
]

let rec exp_parser input =
  let open Parser in
  
  let identifier = identifier_except ["true"; "false"; "let"; "in"; "end"; "if"; "then"; "else"; "fn"; "rec"] in
  let keyword = keyword_among ["true"; "false"; "let"; "in"; "end"; "if"; "then"; "else"; "fn"; "rec"] in
  
  let rec atomic_exp_parser input = first_of [
      map (fun _ -> ConstB true) (keyword "true");
      map (fun _ -> ConstB false) (keyword "false");
      map (fun i -> ConstI i) int_digits;
      map (fun id -> Var id) identifier;
      between (symbol "(") (symbol ")") exp_parser
    ] input
  and rec_binding_parser =
    keyword "rec" |>> 
    identifier |*> fun ident ->
      symbol "=>" |>> 
      exp_parser |*> fun expr ->
        of_value (Rec(ident, None, expr))  


      
  and let_binding_parser =
    let open Parser in
    fun input -> 
      map3 (fun ident expr1 expr2 -> Let(ident, expr1, expr2))
        (keyword "let" |>> identifier)
        (symbol "=" |>> exp_parser)
        ((keyword "in" |>> exp_parser) |*> fun expr2 -> keyword "end" |>> of_value expr2)
        input
  and fn_binding_parser =
    keyword "fn" |>> 
    identifier |*> fun ident ->
      optional (symbol ":" |>> typ_parser) |*> fun opt_typ ->  
        symbol "=>" |>> 
        exp_parser |*> fun expr ->
          of_value (Fn(ident, opt_typ, expr))  

  and if_then_else_parser =
    let open Parser in
    fun input ->
      map3 (fun cond thenBranch elseBranch -> If(cond, thenBranch, elseBranch))
        (keyword "if" |>> exp_parser) 
        (keyword "then" |>> exp_parser)
        (keyword "else" |>> exp_parser)
        input 
  and comma_separated_exp_parser =
    let open Parser in 
    fun input -> 
      map2 (fun expr1 expr2 -> Comma(expr1, expr2))
        (atomic_exp_parser) 
        (symbol "," |>> exp_parser) 
        input 
  and applicative_exp_parser =
    let open Parser in
    fun input -> 
      (atomic_exp_parser |*> fun initial_func ->
          many atomic_exp_parser |*> fun args ->
            of_value (List.fold_left (fun acc arg -> Apply(acc, arg)) initial_func args))
        input
  in
  let rec negatable_exp_parser input = first_of [ 
      let_binding_parser;
      if_then_else_parser; 
      
     
      fn_binding_parser;
      rec_binding_parser;
      
      
      comma_separated_exp_parser;
      applicative_exp_parser 
    ] input
  and negation_exp_parser =
    let open Parser in
    fun input ->
      prefix_op (optional (symbol "-")) negatable_exp_parser (fun negate e ->
          match negate with
          | Some _ -> PrimUop(Negate, e)  
          | None -> e  
        ) input
  in
  let rec multiplicative_exp_parser input = 
    left_assoc_op (symbol "*") negation_exp_parser (fun e1 _ e2 -> PrimBop(e1, Times, e2)) input
  in
  let rec additive_exp_parser input = 
    left_assoc_op (first_of [map (fun _ -> "+") (symbol "+"); map (fun _ -> "-") (symbol "-")])
      multiplicative_exp_parser (fun e1 op e2 -> match op with
          | "+" -> PrimBop(e1, Plus, e2)
          | "-" -> PrimBop(e1, Minus, e2)
          | _ -> raise (Failure "Unexpected operator")) input
  in
  let rec comparative_exp_parser =
    let open Parser in
    first_of [
      non_assoc_op (first_of [map (fun _ -> "=") (symbol "="); map (fun _ -> "<") (symbol "<")])
        additive_exp_parser (fun e1 op e2 -> match op with
            | "=" -> PrimBop(e1, Equals, e2)
            | "<" -> PrimBop(e1, LessThan, e2)
            | _ -> raise (Failure "Unexpected operator"));
      additive_exp_parser
    ]

  in 
  comparative_exp_parser input

let parse_exp : string -> exp option =
  let open Parser in
  run (between spaces eof exp_parser)

(** Part 2: Type Inference *)
let typ_infer_test_helper_tests : ((Context.t * exp) * typ option) list = [
  ((Context.empty, ConstB true), Some Bool)
]

let rec typ_infer (ctx : Context.t) (e : exp) : typ =
  match e with
  | ConstI _ -> Int
  | PrimBop (e1, bop, e2) ->
      let ((t1, t2), t3) = bop_type bop in
      if typ_infer ctx e1 = t1 && typ_infer ctx e2 = t2
      then t3
      else raise TypeInferenceError
  | PrimUop (uop, e') ->
      let (t1, t2) = uop_type uop in
      if typ_infer ctx e' = t1
      then t2
      else raise TypeInferenceError

  | ConstB _ -> Bool
  | If (e', e1, e2) -> 
      if typ_infer ctx e' = Bool 
      then 
        let t1 = typ_infer ctx e1 in
        if typ_infer ctx e2 = t1
        then t1
        else raise TypeInferenceError
      else raise TypeInferenceError

  | Comma (e1, e2) -> Pair (typ_infer ctx e1, typ_infer ctx e2)
  | LetComma (x, y, e1, e2) -> 
      begin
        match typ_infer ctx e1 with
        | Pair (typ_x, typ_y) ->
            let ext_ctx1 = Context.extend ctx (x, typ_x) in
            let ext_ctx2 = Context.extend ext_ctx1 (y, typ_y) in
            typ_infer ext_ctx2 e2
        | _ -> raise TypeInferenceError
      end

  | Fn (x, Some t, e') -> 
      let ext_ctx = Context.extend ctx (x, t) in
      let t' = typ_infer ext_ctx e' in
      Arrow (t, t')
      
  | Apply (e1, e2) ->
      begin
        match typ_infer ctx e1 with
        | Arrow (t1, t2) ->
            if t1 = typ_infer ctx e2
            then t2
            else raise TypeInferenceError
        | _ -> raise TypeInferenceError
      end

  | Rec (f, Some t, e') -> 
      let ext_ctx = Context.extend ctx (f, t) in
      let t' = typ_infer ext_ctx e' in
      if t' = t then t
      else raise TypeInferenceError

  | Let (x, e1, e2) -> 
      let t1 = typ_infer ctx e1 in
      let ext_ctx = Context.extend ctx (x, t1) in 
      typ_infer ext_ctx e2
      
  | Var x ->
      begin
        match Context.lookup ctx x with
        | Some t -> t
        | None -> raise TypeInferenceError
      end

  (** You can ignore these cases for Part 2 *)
  | Fn (_, None, _) -> raise IgnoredInPart2
  | Rec (_, None, _) -> raise IgnoredInPart2

(** DO NOT Change This Definition *)
let typ_infer_test_helper ctx e =
  try
    Some (typ_infer ctx e)
  with
  | TypeInferenceError -> None

(** Part 3: Substitution & Evaluation *)
let free_vars_test_helper_tests : (exp * ident list) list = [
  (ConstI 5, []);
  (Var "x", ["x"])
]

let rec free_vars (e : exp) : IdentSet.t =
  match e with
  | ConstI _ -> IdentSet.empty
  | PrimBop (e1, _, e2) -> IdentSet.union (free_vars e1) (free_vars e2)
  | PrimUop (_, e') -> free_vars e'

  | ConstB _ -> IdentSet.empty
  | If (e', e1, e2) -> 
      let set = IdentSet.union (free_vars e1) (free_vars e2) in
      IdentSet.union (free_vars e') set

  | Comma (e1, e2) -> IdentSet.union (free_vars e1) (free_vars e2)
  | LetComma (x, y, e1, e2) ->
      let set = IdentSet.union (free_vars e1) (free_vars e2) in
      let set = IdentSet.remove x set in
      let set = IdentSet.remove y set in
      set

  | Fn (x, tOpt, e') ->
      let set = free_vars e' in
      let set = IdentSet.remove x set in
      set
        
  | Apply (e1, e2) ->
      IdentSet.union (free_vars e1) (free_vars e2)

  | Rec (f, tOpt, e') ->
      IdentSet.remove f (free_vars e')

  | Let (x, e1, e2) -> 
      let set1 = free_vars e1 in
      let set2 = IdentSet.remove x (free_vars e2) in
      IdentSet.union set1 set2
        
  | Var x -> IdentSet.singleton x

(** DO NOT Change This Definition *)
let free_vars_test_helper e = IdentSet.elements (free_vars e)

let subst_tests : (((exp * ident) * exp) * exp) list = [
  (((ConstI 5, "x"), PrimBop (ConstI 2, Plus, Var "x")), PrimBop (ConstI 2, Plus, ConstI 5))
]

let rec subst ((d, z) : exp * ident) (e : exp) : exp =
  (** [rename (x, e)] replace [x] appears in [e] with a fresh identifier
      and returns the fresh identifier and updated expression *)
  let rename ((x, e) : ident * exp) : ident * exp =
    let x' = fresh_ident x in
    (x', subst (Var x', x) e)
  in
  match e with
  | ConstI _ -> e
  | PrimBop (e1, bop, e2) -> PrimBop (subst (d, z) e1, bop, subst (d, z) e2)
  | PrimUop (uop, e') -> PrimUop (uop, subst (d, z) e')

  | ConstB _ -> e
  | If (e', e1, e2) ->
      If (subst (d, z) e', subst (d, z) e1, subst (d, z) e2)

  | Comma (e1, e2) -> Comma (subst (d, z) e1, subst (d, z) e2) 
  | LetComma (x, y, e1, e2) ->
      let e1' = subst (d, z) e1 in
      let (x', e2') = rename (x, e2) in
      let (y', e2'') = rename (y, e2') in
      LetComma (x', y', e1', subst (d, z) e2'')

  | Fn (x, tOpt, e') ->
      if x = z
      then e
      else
        let x' = fresh_ident "x'" in
        let e' = subst ((Var x'), x) e' in
        Fn (x', tOpt, subst (d, z) e')
          
  | Apply (e1, e2) ->
      Apply (subst (d, z) e1, subst (d, z) e2)

  | Rec (f, tOpt, e') -> 
      let (f', e'') = rename (f, e') in
      Rec (f', tOpt, subst (d, z) e'')

  | Let (x, e1, e2) ->
      let e1' = subst (d, z) e1 in
      let (x', e2') = rename (x, e2) in
      Let (x', e1', subst (d, z) e2')
      
  | Var x ->
      if x = z
      then d
      else e

let eval_test_helper_tests : (exp * exp option) list = [
  (Var "x", None);
  (ConstI 5, Some (ConstI 5));
  (PrimBop (ConstI 5, Minus, ConstI 5), Some (ConstI 0))
]

let rec eval (e : exp) : exp =
  match e with
  | ConstI _ -> e
  | PrimBop (e1, bop, e2) ->
      begin
        match eval e1, eval e2 with
        | ConstI n1, ConstI n2 ->
            begin
              match bop with
              | Equals -> ConstB (n1 = n2)
              | LessThan -> ConstB (n1 < n2)
              | Plus -> ConstI (n1 + n2)
              | Minus -> ConstI (n1 - n2)
              | Times -> ConstI (n1 * n2)
            end
        | _ -> raise EvaluationStuck
      end
  | PrimUop (_, e) ->
      begin
        match eval e with
        | ConstI n -> ConstI (- n)
        | _ -> raise EvaluationStuck
      end

  | ConstB _ -> e
  | If (e', e1, e2) -> 
      begin
        match eval e' with
        | ConstB true -> eval e1
        | ConstB false -> eval e2
        | _ -> raise EvaluationStuck
      end

  | Comma (e1, e2) -> Comma (eval e1, eval e2)
  | LetComma (x, y, e1, e2) -> 
      begin
        let e1' = eval e1 in
        match e1' with
        | Comma (vx, vy) ->
            let e2' = subst (vy, y) e2 in 
            let e2'' = subst (vx, x) e2' in 
            eval e2''
        | _ -> raise EvaluationStuck
      end

  | Fn (x, tOpt, e') ->
      Fn (x, tOpt, e')
          
  | Apply (e1, e2) -> 
      begin
        let e1' = eval e1 in
        match e1' with
        | Fn (ident, typ, exp) ->
            let e2' = eval e2 in
            let exp' = subst (e2', ident) exp in
            eval exp'
        | _ -> raise EvaluationStuck
      end
      
  | Rec (f, tOpt, e') ->
      begin
        let f' = eval (Fn (f, tOpt, e')) in
        match f' with
        | Fn (ident, _, exp) ->
            let exp' = subst (Rec (f, tOpt, e'), ident) exp in
            eval exp'
        | _ -> raise EvaluationStuck
      end

  | Let (x, e1, e2) ->
      let e1' = eval e1 in
      let e2' = subst (e1', x) e2 in
      eval e2' 
  | Var _ -> raise EvaluationStuck

(** DO NOT Change This Definition *)
let eval_test_helper e =
  try
    Some (eval e)
  with
  | EvaluationStuck -> None

(** Part 4: Unification & Advanced Type Inference *)
let unify_test_case1 () =
  let x = new_tvar () in
  let y = new_tvar () in
  y := Some Int;
  (TVar x, TVar y)

let unify_test_case2 () =
  let x = new_tvar () in
  (TVar x, Arrow (TVar x, TVar x))

let unify_test_helper_tests : ((unit -> typ * typ) * bool) list = [
  ((fun () -> (Int, Int)), true);
  ((fun () -> (Int, Bool)), false);
  (unify_test_case1, true);
  (unify_test_case2, false)
]

let rec unify : typ -> typ -> unit =
  let rec occurs_check (x : typ option ref) (t : typ) : bool =
    let t = rec_follow_tvar t in
    match t with
    | Int -> false
    | Bool -> false
    | Pair (t1, t2) -> occurs_check x t1 || occurs_check x t2
    | Arrow (t1, t2) -> occurs_check x t1 || occurs_check x t2
    | TVar y -> is_same_tvar x y
  in
  fun ta tb ->
    let ta = rec_follow_tvar ta in
    let tb = rec_follow_tvar tb in
    match ta, tb with
    | Int, Int -> ()
    | Bool, Bool -> ()
    | Pair (ta1, ta2), Pair (tb1, tb2) -> unify ta1 tb1; unify ta2 tb2
    | Arrow (ta1, ta2), Arrow (tb1, tb2) -> unify ta1 tb1; unify ta2 tb2
    | TVar xa, TVar xb when is_same_tvar xa xb -> ()
    | TVar xa, _ -> 
        if occurs_check xa tb then raise OccursCheckFailure
        else xa := Some tb
    | _, TVar xb -> unify tb ta
    | _, _ -> raise UnificationFailure

(** DO NOT Change This Definition *)
let unify_test_helper f =
  let ta, tb = f () in
  try
    unify ta tb; true
  with
  | UnificationFailure -> false
  | OccursCheckFailure -> false

let adv_typ_infer_test_case1 =
  let x = new_tvar () in
  ((Context.empty, Fn ("y", None, Var "y")), Some (Arrow (TVar x, TVar x)))

let adv_typ_infer_test_helper_tests : ((Context.t * exp) * typ option) list = [
  adv_typ_infer_test_case1
] 

let rec adv_typ_infer (ctx : Context.t) (e : exp) : typ =
  let new_deref_tvar () : typ =
    let t_ref = new_tvar () in
    match !t_ref with
    | None -> TVar (ref None)
    | Some t -> t 
  in
  match e with
  | ConstI n -> Int
  | PrimBop (e1, bop, e2) -> 
      let t1 = adv_typ_infer ctx e1 in
      let t2 = adv_typ_infer ctx e2 in
      unify t1 Int;
      unify t2 Int;
      Int
  | PrimUop (uop, e') -> 
      let t = adv_typ_infer ctx e' in
      unify t Int;
      Int
  | ConstB b -> Bool
  | If (e', e1, e2) -> 
      let t' = adv_typ_infer ctx e' in
      let t1 = adv_typ_infer ctx e1 in
      let t2 = adv_typ_infer ctx e2 in
      unify t' Bool;
      unify t1 t2;
      t1
  | Comma (e1, e2) -> adv_typ_infer ctx e2
  | LetComma (x, y, e1, e2) -> 
      begin
        let t1 = adv_typ_infer ctx e1 in
        match t1 with
        | Pair (tx, ty) ->
            let ctx' = Context.extend (Context.extend ctx (x, tx)) (y, ty) in
            adv_typ_infer ctx' e2
        | _ -> raise TypeInferenceError
      end
      
  | Fn (x, Some t, e') -> 
      let ctx' = Context.extend ctx (x, t) in
      let t' = adv_typ_infer ctx' e' in
      Arrow (t, t')
  | Fn (x, None, e') -> 
      let t_ref = new_deref_tvar () in
      let ctx' = Context.extend ctx (x, t_ref) in
      let t' = adv_typ_infer ctx' e' in
      unify t_ref t';
      Arrow (t_ref, t') 
  | Apply (e1, e2) -> 
      let t1 = adv_typ_infer ctx e1 in
      let t2 = adv_typ_infer ctx e2 in
      let t_ref = new_deref_tvar () in
      unify t1 (Arrow (t2, t_ref));
      t_ref
      
  | Rec (f, Some t, e') -> 
      let ctx' = Context.extend ctx (f, t) in
      let t' = adv_typ_infer ctx' e' in
      unify t t';
      t
  | Rec (f, None, e') -> 
      let t_ref = new_deref_tvar () in
      let ctx' = Context.extend ctx (f, t_ref) in
      let t' = adv_typ_infer ctx' e' in
      unify t_ref t';
      t_ref
      
  | Let (x, e1, e2) -> 
      let t1 = adv_typ_infer ctx e1 in
      let ctx' = Context.extend ctx (x, t1) in
      adv_typ_infer ctx' e2
  | Var x -> 
      begin
        match Context.lookup ctx x with
        | Some typ -> typ
        | None -> raise TypeInferenceError
      end

(** DO NOT Change This Definition *)
let adv_typ_infer_test_helper ctx e =
  try
    Some (adv_typ_infer ctx e)
  with
  | UnificationFailure -> None
  | OccursCheckFailure -> None
  | TypeInferenceError -> None

(**
 ************************************************************
 You Don't Need to Modify Anything After This Line
 ************************************************************

 Following definitions are the helper entrypoints
 so that you can do some experiments in the top-level.
 Once you implement [exp_parser], [typ_infer], and [eval],
 you can test them with [infer_main] in the top-level.
 Likewise, once you implement [exp_parser], [adv_typ_infer], and [eval],
 you can test them with [adv_infer_main] in the top-level.
 *)
let infer_main exp_str =
  match parse_exp exp_str with
  | None -> raise ParserFailure
  | Some e ->
      print_string "input expression       : "; print_exp e; print_newline ();
      let t = typ_infer Context.empty e in
      print_string "type of the expression : "; print_typ t; print_newline ();
      print_string "evaluation result      : "; print_exp (eval e); print_newline ()

let adv_infer_main exp_str =
  match parse_exp exp_str with
  | None -> raise ParserFailure
  | Some e ->
      print_string "input expression       : "; print_exp e; print_newline ();
      let t = adv_typ_infer Context.empty e in
      print_string "type of the expression : "; print_typ t; print_newline ();
      print_string "evaluation result      : "; print_exp (eval e); print_newline ()
