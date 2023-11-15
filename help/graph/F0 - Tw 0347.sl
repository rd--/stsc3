(* https://sccode.org/1-4Qy ; f0 ; 0347 *)
(0 .. 29).collect { :i |
	var c = i / 48;
	var b = c / 72;
	VarSaw(
		(LfSaw(c, 0) * LfSaw(b, 0) * 8 + 9).RoundTo(i % 9 + 1) * 25 + c,
		c,
		LfSaw(3, i) + 1 / 3
	) * (LfSaw(b, i / pi) % 1) / 2
}.Splay
