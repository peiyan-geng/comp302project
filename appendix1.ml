(** Exercise 1: Parse a Tree *)
let parse_tree_tests : (string * tree option) list = [
  ("", None);
  ("((. < 5 > .) < 4 > .)", Some (Node (Node (Leaf, 5, Leaf), 4, Leaf)))
]

let rec tree_parser i =
  let open Parser in
  let tree_parser_impl =
    first_of_2
      (const_map Leaf (symbol "."))
      (between 
         (symbol "(")
         (symbol ")")
         (tree_parser |*> fun l ->
             symbol "<" |*> fun _ ->
               int_digits |*> fun v ->
                 symbol ">" |*> fun _ ->
                   tree_parser |*> fun r ->
                     const_map (Node (l, v, r)) spaces)) 
  in
  tree_parser_impl i

(** DO NOT Change This Definition *)
let parse_tree : string -> tree option =
  let open Parser in
  run (between spaces eof tree_parser)

(** Part 1: Parse an Arithmetic Expression *)
let parse_arith_tests : (string * arith option) list = [
  ("", None);
  ("5 + 4", Some (Bop (Const 5, Plus, Const 4)))
]

let rec arith_parser i =
  let open Parser in
  let atomic_exp_parser =
    first_of_2
      (map (fun i -> Const i) int_digits)
      (between (symbol "(") (symbol ")") arith_parser)
  in
  let power_exp_parser =
    right_assoc_op
      (symbol "^")
      atomic_exp_parser
      (fun a _ b -> Bop (a, Power, b))
  in
  let multiplicative_exp_parser =
    left_assoc_op
      (map (fun _ -> Times) (symbol "*"))
      power_exp_parser
      (fun a op b -> Bop (a, op, b))
  in
  let arith_exp_parser_impl =
    left_assoc_op
      (first_of_2 (map (fun _ -> Plus) (symbol "+")) (map (fun _ -> Minus) (symbol "-")))
      multiplicative_exp_parser
      (fun a op b -> Bop (a, op, b))
  in
  arith_exp_parser_impl i

(** DO NOT Change This Definition *)
let parse_arith : string -> arith option =
  let open Parser in
  run (between spaces eof arith_parser)
