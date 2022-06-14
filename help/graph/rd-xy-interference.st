"xy interference (rd)"
| x y nd |
	x := MouseX minval: 20 maxval: 22000 warp: 0 lag: #(0.005 0.025).
	y := MouseY minval: 20 maxval: 22000 warp: 0 lag: #(0.005 0.075).
	nd := [
		| n a b |
		n := LFNoise0 freq: #(5 9).
		a := SinOsc freq: x + n kr phase: 0.
		b := SinOsc freq: y phase: 0.
		a * b ].
(nd dup: 3) sum * 0.35
