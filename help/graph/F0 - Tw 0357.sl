(* https://sccode.org/1-4Qy ; f0 ; 0357 ; requires=BLowPass4 *)
var c = 1 / [31, 23];
var b = VarSaw(c, 0, 0.5);
var o = VarSaw((VarSaw([7, 6], 0, 0.5) < 0 + b) > 1 + 1 * 99, 0, 1 - b / 2) % b;
Hpf(BLowPass4(o, 4 ^ VarSaw(c, 0, 0.5) * 666, 0.08).Sin / 2, 9)
