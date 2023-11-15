(* https://sccode.org/1-4Qy ; f0 ; 0287 *)
var o = Saw(([58 62 65 69 72 46] - (2 ^ LfSaw([1 .. 5] / 32, 0)).Ceiling).MidiCps);
var e = o % LfSaw([1 1 6], 0) % (LfSaw(2, [1 2] / 8) * 2);
Hpf(e.Splay, 9) / 2

(* ---- ; calculations
':>AEH.'.ascii
*)
