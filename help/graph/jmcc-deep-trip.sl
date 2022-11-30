;; deep trip (jmcc) #9 ; texture=overlap,12,4,4,inf
var f = (LfNoise1(Rand(0, 0.3)) * 60 + 70).MidiCps;
var a = LfNoise2(f * Rand(0, 0.5)) * (LfNoise1(Rand(0, 8)) * SinOsc(Rand(0, 40), 0) * 0.1).max(0);
var s = Pan2(SinOsc(f, 0) *  a, LfNoise1(Rand(0, 5)), 1);
var c = { CombN(s, 0.5, { Rand(0, 0.2) + 0.3 } ! 2, 20) };
s + c.dup(2).sum

;; deep trip (jmcc) #9 ; graph rewrite
OverlapTexture({ :tr |
	var f = (LfNoise1(TRand(0, 0.3, tr)) * 60 + 70).MidiCps;
	var a = LfNoise2(f * TRand(0, 0.5, tr)) * (LfNoise1(TRand(0, 8, tr)) * SinOsc(TRand(0, 40, tr), 0) * 0.1).max(0);
	var s = Pan2(SinOsc(f, 0) *  a, LfNoise1(TRand(0, 5, tr)), 1);
	var c = { CombN(s, 0.5, { TRand(0, 0.2, tr) + 0.3 } ! 2, 20) };
	s + c.dup(2).sum
}, 12, 4, 4)

;; deep trip (jmcc) #9 ; graph rewrite ; left-to-right
OverlapTexture({ :tr |
	var trRand = { :lo :hi | TRand(lo, hi, tr) };
	var f = LfNoise1(trRand(0, 0.3)).MulAdd(60, 70).MidiCps;
	var a = LfNoise2(f.Mul(trRand(0, 0.5))).Mul((LfNoise1(trRand(0, 8)).Mul(SinOsc(trRand(0, 40), 0)).Mul(0.1)).Max(0));
	var s = SinOsc(f, 0).Mul(a).Pan2(LfNoise1(trRand(0, 5)), 1);
	var c = { s.CombN(0.5, { trRand(0, 0.2) + 0.3 } ! 2, 20) };
	s + c.dup(2).sum
}, 12, 4, 4)
