(* SelectX ; as sequencer *)
var n = 10;
var a = [517, 403, 89, 562, 816, 107, 241, 145, 90, 224];
var c = n / 2;
var f = SelectX(LfSaw(0.5, 0) * c + c, a);
Saw(f) * 0.1
