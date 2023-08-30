(* Dust *)
Dust(2) * 0.25

(* Dust ; decreasing density *)
Dust(XLn(20000, 2, 10)) * 0.15

(* Dust ; arrayed *)
(Dust([4, 14, 40]) * [1, 0.4, 0.1]).Splay2
