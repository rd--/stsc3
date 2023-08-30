(* Cc ; continuous controller ; one indexed *)
var f = Lag(Cc(1), 0.1) * 110 + 110;
var nh = Lag(Cc(2), 0.2) * 9 + 1;
var a = Lag(Cc(3), 0.3) * 0.1;
var p = Lag(Cc(4), 0.4) * 2 - 1;
Pan2(Blip(f, nh) * a, p, 1)
