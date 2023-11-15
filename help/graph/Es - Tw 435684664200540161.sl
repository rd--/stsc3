(* https://twitter.com/sluyterrific_sc/status/435684664200540161 (es) *)
(1 .. 99).collect { :i |
	var m = i % (LfNoise2(0.01) * 50 + 50) + (LfNoise2(0.1) * 10 + 40);
	SinOsc(m.MidiCps, 0) * (LfNoise2(1) * 0.1 + 0.1)
}.Splay
