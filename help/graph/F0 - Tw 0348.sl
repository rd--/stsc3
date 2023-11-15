(* https://sccode.org/1-4Qy ; f0 ; 0348 *)
var b = [9 4 3 6] / 4;
var c = VarSaw(0.1 / b, 0, 0.5) + 1 / 9;
var o = 9 ^ c * VarSaw((VarSaw(c / 3, 0, 0.5) + b).RoundTo(b) * 99, 0, c);
AllpassC(o, 1, c + 9 / 99, 9).Tanh.Splay / 2
