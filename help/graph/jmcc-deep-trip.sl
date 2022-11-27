;; deep trip (jmcc) #9 ; texture=overlap,12,4,4,inf
var f = (LFNoise1(Rand(0, 0.3)) * 60 + 70).midiCps;
var a = LFNoise2(f * Rand(0, 0.5)) * (LFNoise1(Rand(0, 8)) * SinOsc(Rand(0, 40), 0) * 0.1).max(0);
var s = Pan2(SinOsc(f, 0) *  a, LFNoise1(Rand(0, 5)), 1);
var c = { CombN(s, 0.5, { Rand(0, 0.2) + 0.3 } ! 2, 20) };
s + c.dup(2).sum

;; deep trip (jmcc) #9 ; graph rewrite
OverlapTexture({ :tr |
	var f = (LFNoise1(TRand(0, 0.3, tr)) * 60 + 70).midiCps;
	var a = LFNoise2(f * TRand(0, 0.5, tr)) * (LFNoise1(TRand(0, 8, tr)) * SinOsc(TRand(0, 40, tr), 0) * 0.1).max(0);
	var s = Pan2(SinOsc(f, 0) *  a, LFNoise1(TRand(0, 5, tr)), 1);
	var c = { CombN(s, 0.5, { TRand(0, 0.2, tr) + 0.3 } ! 2, 20) };
	s + c.dup(2).sum
}, 12, 4, 4)

;; deep trip (jmcc) #9 ; graph rewrite ; left-to-right
OverlapTexture({ :tr |
	var f = LFNoise1(tr.TrRand(0, 0.3)).MulAdd(60, 70).MidiCps;
	var a = LFNoise2(f.Mul(tr.TrRand(0, 0.5))).Mul((LFNoise1(tr.TrRand(0, 8)).Mul(SinOsc(tr.TrRand(0, 40), 0)).Mul(0.1)).Max(0));
	var s = SinOsc(f, 0).Mul(a).Pan2(LFNoise1(tr.TrRand(0, 5)), 1);
	var c = { s.CombN(0.5, { tr.TrRand(0, 0.2) + 0.3 } ! 2, 20) };
	s + c.dup(2).sum
}, 12, 4, 4)
