(* https://sccode.org/1-4Qy ; f0 ; 0353 *)
(0 .. 7).collect { :i |
	var o = Saw(LfSaw(1 / [99 100], 0).RoundTo(1 / 8) ^ 2 * 8);
	AllpassN(Rhpf(o, 2 ^ i * [99 50], 0.01), 1, i + 1 / 9, 9) / 9
}.Mix * 0.5
