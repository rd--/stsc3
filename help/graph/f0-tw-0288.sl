;; https://sccode.org/1-4Qy ; f0 ; 0288
var b = [1 .. 4];
var c = b + 8;
var f = SelectX(SinOscFB(0.1, 0) % (SinOscFB(b, 0)) * c, DC([58, 46, 85, 79, 68, 68, 46, 69, 100].midiCps));
Splay2(SinOscFB(f, SinOscFB(1 / c, 0))) / 2

(* ---- ; calculations
':.UODD.Ed'.ascii == [58, 46, 85, 79, 68, 68, 46, 69, 100]
*)
