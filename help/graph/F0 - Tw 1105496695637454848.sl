(* f0 ; https://twitter.com/redFrik/status/1105496695637454848 *)
var b = (1 .. 15) + 1 / 151;
var w = (VarSaw(b, b, 1 / 5.15) * 1 + 1.5).Ceiling;
var s = BufRd(1, [51 * 1.5, 51, 151].asLocalBuf, (VarSaw(1/15, 1/5, b) * 5).Ceiling + 5 / 5, 0, 1);
var x = Lag(s, b);
var y = VarSaw(5 + b, 0, 0.5);
var z = VarSaw(b, b, b) * b / 5 + [1.5, 5, 1];
var m = VarSaw(5 - b, b, b) * 5 + 5;
var o = VarSaw(w * x + y * z, b, VarSaw(b, 0, 0.5) + 5 / 15) * (m > 1.515);
var f = 1515 ^ (VarSaw(1 - b / 5, 0, 0.5) / 15 + 1 + b);
var rq = 1.5 ^ VarSaw(b, 0, 0.5) / 5;
BLowPass(o, f, rq).Splay / 5
