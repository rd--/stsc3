;; zizle (jmcc) #SC3d1.5 ; graph rewrite
OverlapTexture({ :tr |
	var a = { :f | (SinOsc(f * [TRand(0.7, 1.3, tr), 1], { TRand(0, 2 * pi, tr) } ! 2) * 0.1).sum };
	var o = SinOsc(TRand(24, 108, tr).MidiCps, TRand(0, 2 * pi, tr));
	var s = o * a(TExpRand(0.3, 8, tr)).max(0) * a(TExpRand(6, 24, tr)).abs;
	Pan2(s, TRand(-1, 1, tr), 1)
}, 4, 4, 12)

;; zizle (jmcc) #SC3d1.5 ; texture=overlap,4,4,12,inf
var a = { :f | (SinOsc(f * [Rand(0.7, 1.3), 1], { Rand(0, 2 * pi) } ! 2) * 0.1).sum };
var o = SinOsc(Rand(24, 108).MidiCps, Rand(0, 2 * pi));
var s = o * a(ExpRand(0.3, 8)).max(0) * a(ExpRand(6, 24)).abs;
Pan2(s, Rand(-1, 1), 1)
