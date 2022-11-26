;; hard sync sawtooth with lfo (jmcc) #6 ; graph-rewrite
var txt = OverlapTexture({ :tr |
	var f = (30 + TRand(0, 50, tr)).midiCps;
	SyncSaw([f, f + 0.2], SinOsc(0.2, { TRand(0, 2 * pi, tr) } ! 2) * 2 * f + (3 * f)) * 0.05
}, 4, 4, 4);
CombN(txt, 0.3, 0.3, 4) + txt.reversed

;; hard sync sawtooth with lfo (jmcc) #6 ; graph-rewrite ; left-to-right
var txt = OverlapTexture({ :tr |
	var f = tr.TrRand(0, 50).MulAdd(1, 30).MidiCps;
	SyncSaw([f, f + 0.2], SinOsc(0.2, { tr.TrRand(0, pi).Mul(2) } ! 2).Mul(2).MulAdd(f, f * 3)).Mul(0.05)
}, 4, 4, 4);
txt.CombN(0.3, 0.3, 4) + txt.reversed
