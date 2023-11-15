(* Saw ; amplitude modulation *)
Saw(440) * SinOsc(MouseX(1, 880, 1, 0.2), 0) / 5

(* Saw ; https://sccode.org/1-4Qy ; f0 ; 0287 *)
var c = [58 62 65 69 72 46];
var m = c - (2 ^ LfSaw([1 .. 5] / 32, 0)).Ceiling;
var o = Saw(m.MidiCps);
o.Splay / 2

(* ---- ; calculations
':>AEH.'.ascii = [58, 62, 65, 69, 72, 46]
*)
