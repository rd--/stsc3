;; https://twitter.com/sluyterrific_sc/status/435684664200540161 (es)
var vc = { :i |
	var m = i % (LFNoise2(0.01) * 50 + 50) + (LFNoise2(0.1) * 10 + 40);
	SinOsc(m.midiCps, 0) * (LFNoise2(1) * 0.01 + 0.01)
};
(1 .. 99).collect(vc).Splay2
