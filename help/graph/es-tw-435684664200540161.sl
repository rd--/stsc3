;; https://twitter.com/sluyterrific_sc/status/435684664200540161 (es)
var vc = { :i |
	var m = i % (LfNoise2(0.01) * 50 + 50) + (LfNoise2(0.1) * 10 + 40);
	SinOsc(m.MidiCps, 0) * (LfNoise2(1) * 0.01 + 0.01)
};
(1 .. 99).collect(vc).Splay2
