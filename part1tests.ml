let parse_exp_tests : (string * exp option) list = [
  ("", None);
  ("1", Some (ConstI 1));
  ("1 + 1", Some (PrimBop (ConstI 1, Plus, ConstI 1)));
  ("-1", Some (PrimUop (Negate, ConstI 1)));
  
  ("true", Some (ConstB true)); 
  ("if true then 1 else 2", Some (If ((ConstB true), (ConstI 1), (ConstI 2))));
  
  ("1, 2", Some (Comma (ConstI 1, ConstI 2)));
  ("let (x, y) = 1 in 2 end", 
   Some (LetComma (("x"), ("y"), (ConstI 1), (ConstI 2))));
  
  ("fn x : int => 1", Some (Fn ("x", Some Int, ConstI 1)));
  ("fn x => 1", Some (Fn ("x", None, ConstI 1)));
  ("1 2", Some (Apply (ConstI 1, ConstI 2)));
  
  ("rec x : int => 1", Some (Rec ("x", Some Int, ConstI 1)));
  ("rec x => 1", Some (Rec ("x", None, ConstI 1)));
  
  ("let x = 1 in x end", Some (Let ("x", ConstI 1, Var "x"))); (* fail *)
  ("x", Some (Var "x")); 
]
