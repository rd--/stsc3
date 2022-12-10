;; https://twitter.com/alln4tural/status/99846300173991936 ; graph rewrite
OverlapTexture({ :tr |
	var h = TChoose(tr, [33, 38, 40]).MidiCps * (2 ** TChoose(tr, [0 .. 5]));
	{ SinOsc(TExpRand(h - (h / 256), h + (h / 256), tr), 0) * 0.025 } !^ 16 (* udp: 64 -> 16 *)
}, 1, 9, 5)

;; https://twitter.com/alln4tural/status/99846300173991936 ; graph rewrite
OverlapTexture({ :tr |
	var h = TChoose(tr, [33, 38, 40]).MidiCps * (2 ** TChoose(tr, [0 .. 4]));
	{ SinOsc(TExpRand(h - (h / 64), h + (h / 64), tr), 0) * 0.025 } !^ 8
}, 1, 9, 10) (* udp: 40 -> 10 *)
