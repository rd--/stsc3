(* https://sccode.org/1-4Qy ; f0 ; 0288 *)
var b = [1 .. 4];
var c = b + 8;
var f = SelectX(
	SinOscFb(0.1, 0) % (SinOscFb(b, 0)) * c,
	Dc([58 46 85 79 68 68 46 69 100].MidiCps)
);
SinOscFb(f, SinOscFb(1 / c, 0)).Splay / 3

(* ---- ; calculations
':.UODD.Ed'.ascii == [58, 46, 85, 79, 68, 68, 46, 69, 100]
*)
