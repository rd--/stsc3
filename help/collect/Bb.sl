(* http://earslap.com/weblog/music-release-laconicism.html *)
var k = DmdFor(6.4, 0, Dseq(inf, [0.05, Drand(1, [0.04, 0.08])]));
Splay2(Integrator((LfNoise0([5, 5, 5]) * k).RoundTo(k / 10), 1).Sin.Sqrt.Tanh, 0.3, 1, 0, true)

(* http://earslap.com/weblog/music-release-laconicism.html ; wait *)
var x = LfNoise1(0.5 * LocalIn(1, 0) + 0.1) * 0.5 + 0.5;
var s = PitchShift(PitchShift(Pulse([90, 90.01], x), 10, x * 4, x, 0), 10, 4 - (x * 4), 1 - x, 0);
s <! LocalOut(x)

(* http://earslap.com/weblog/music-release-laconicism.html ; wait *)
var q = [0 3 5 7 10];
var t = Impulse(4, 0) * LfNoise0(500) > 0;
var f = Demand(t, 0, Drand(inf, (q + 12 ++ q + 33).MidiCps));
PitchShift(Saw(f) * Decay(t, 3), 7, 2, 0, 0)

(* http://earslap.com/weblog/music-release-laconicism.html *)
var snd = Hpf(
	Pulse([[0.1, 0.11], [0.12, 0.13]], 0.6) * 0.005,
	99
);
Hpf(
	MidEq(
		Limiter(GVerb(snd, [[1, 5 / 4], [1.5, 2]], 99, 0.5, 0.5, 15, 1, 0,7, 0.5) * 300, 1, 0.01) * 0.1,
		9000,
		0.9,
		9
	),
	200
).Sum.transposed.Mix

(* http://earslap.com/weblog/music-release-laconicism.html ; wait *)
var f = LocalIn(2, 0).Tanh;
var k = Latch(f.first.Abs, Impulse(0.5, 0));
f <! LocalOut(f + AllpassN(Pulse([2, 3], k * 0.01 + 0.000001) * 0.9, 1, k * 0.3, 100 * k))

(* http://earslap.com/weblog/music-release-laconicism.html *)
var a = Bpf(Saw([40, 40.001]), LfNoise0(128) + 1 * 4000 + 146, LfNoise1(1) + 1 * 0.05 + 0.01).Tanh;
CombC(a, 9, a.Abs.Lag(2) * 9, a.Abs.Lag(1) * 100)

(* http://earslap.com/weblog/music-release-laconicism.html *)
var f = LocalIn(2, 0).Tanh;
var k = Latch(f.first.Abs, Impulse(1 / 4, 0));
f <! LocalOut(f + CombC(Blip([4, 6], 100 * k + 50) * 0.9, 1, k * 0.3, 50 * f))

(* http://earslap.com/weblog/music-release-laconicism.html *)
var tr = Impulse(8, 0) * LfNoise1(2);
CombL(
	(Saw([3, 4]) * Decay(tr, 0.1)).Tanh,
	1,
	TRand(0, 0.01, tr).RoundTo(0.00015),
	TRand(-30, 30, tr)
)

(* http://earslap.com/weblog/music-release-laconicism.html *)
var f = [60, 61];
var l = LfNoise0(6);
var o = (LfNoise0(4).Max(l).Max(SinOsc(f * (l * 9).Ceiling.Lag(0.1), 0) * 0.7));
(BBandPass(o, f, LfNoise0(1).Abs / 2) * 700 * l.Lag(1)).Tanh

(* http://earslap.com/weblog/music-release-laconicism.html *)
var t = [0 0 0 1 5 7 10 12 12 12] + 30;
var a = DmdFor(1/8, 0, Drand(inf, t + 24 ++ t ++ t));
(BHiPass(LfNoise1(8) ^ 6, [a, a + 7].MidiCps, a / 3000) * (67 - a)).Tanh

(* http://earslap.com/weblog/music-release-laconicism.html *)
AllpassL(SinOsc(55,0).Tanh, 0.4, TExpRand(0.0002, 0.4, Impulse(8, 0)).RoundTo([0.002, 0.004]), 2)

(* http://earslap.com/weblog/music-release-laconicism.html *)
var i = { :freq | Impulse(freq, 0) };
var ph = Integrator(Integrator(i(64).Lag(LfNoise1([2, 2]) * 2 + 2) * 99, 0.9), 0.99).Fold2(pi);
SinOsc(LagUd(Impulse(2, 0), 0, 0.4) * 360, ph) / 3

(* http://earslap.com/weblog/music-release-laconicism.html *)
var t = [0 3 5 7 10 12] + 40;
var p = DmdFor(1 / 4, 0, Drand(inf, (t + 12 ++ t).MidiCps));
var b = TRand(1500, 2000, Impulse(16, 0)).Lag(0.1);
Blip([b, b + p], 1).mean ^ 2

(* http://earslap.com/weblog/music-release-laconicism.html *)
var i = Impulse(8, 0).Lag(0.3);
10.timesRepeat {
	i := LeakDc(AllpassC(i, 1, LfNoise0(8).Range(0.00001, 0.2), -0.15) * LfNoise0(8).Range(1, 3), 0.995).Tanh
};
i

(* http://earslap.com/weblog/music-release-laconicism.html *)
var v = Blip([20000, 20000 - 9], 1) * (LfNoise0(16) * 0.5 + 0.5 ^ 9);
42.timesRepeat {
	v := LeakDc(AllpassC(v, 1, LfNoise0(5) * 0.05 + (0.05 + 0.001), 100), 0.995)
};
(v * 99).Tanh

(* http://earslap.com/weblog/music-release-laconicism.html *)
(Hpf(LfNoise1(2), [10, 10.1]) * 100).Tanh

(* http://earslap.com/weblog/music-release-laconicism.html ; requires=kr *)
var x = DmdFor(1/8, 0, Drand(inf, [0, Drand(1, [0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1])]));
LeakDc(Brf(Saw(8) * Decay2(x, 0.01, 0.3).kr ^ 1.5, x * 20 + [45.1, 45], 0.1), 0.995).Tanh

(* http://earslap.com/weblog/music-release-laconicism.html ; wait *)
var v = Blip([60, 61], 5) * (LfNoise0(4).MulAdd(1, 0) ^ 8);
12.timesRepeat {
	v := LeakDc(CombC(v, 1, LfNoise0(1).MulAdd(0.05, 0.06).Lag(5000), 9), 0.995)
};
Limiter(v, 0.9, 1)

(* http://earslap.com/article/recreating-the-thx-deep-note.html ; 30 oscillators together, distributed across the stereo field *)
var numVoices = 30;
var fundamentals = { Rand(200, 400) } ! numVoices;
fundamentals.collect { :freq |
	EqPan(
		Saw(freq),
		Rand(-1, 1)
	) * numVoices.reciprocal
}.Sum

(* http://earslap.com/article/recreating-the-thx-deep-note.html ; adding random wobbling to freqs, sorting randoms, lowpassing ; fundamentals are sorted, so higher frequencies drift more *)
var numVoices = 30;
var fundamentals = { 200.randomFloat(400) }.duplicate(numVoices).sorted;
fundamentals.withIndexCollect { :freq0 :index |
	var freq = freq0 + (LfNoise2(0.5) * 3 * index);
	EqPan(
		BLowPass(Saw(freq), freq * 5, 0.5),
		Rand(-1, 1)
	) * numVoices.reciprocal
}.Sum

(* http://earslap.com/article/recreating-the-thx-deep-note.html ; inverting init sort, louder bass, final volume envelope, some little tweaks ; requires=CurveGen *)
var numVoices = 30;
var fundamentals = { 200.randomFloat(400) }.duplicate(numVoices).sorted.reversed;
var finalPitches = ((1 .. numVoices).collect { :each |
	(each / (numVoices / 6)).RoundTo(1) * 12
} + 14.5).MidiCps;
var outerEnv = CurveGen(1, [0, 0.1, 1], [8, 4], [2, 4]);
var ampEnvelope = CurveGen(1, [0, 1, 1, 0], [3, 21, 3], [2, 0, -4]);
var snd = (1 .. numVoices).collect { :numTone |
	var initRandomFreq = fundamentals[numTone] + (LfNoise2(0.5) * 6 * (numVoices - numTone));
	var destinationFreq = finalPitches[numTone] + (LfNoise2(0.1) * numTone / 3);
	var sweepEnv = CurveGen(1, [0, Rand(0.1, 0.2), 1], [Rand(5.5, 6), Rand(8.5, 9)], [Rand(2, 3), Rand(4, 5)]);
	var freq = ((1 - sweepEnv) * initRandomFreq) + (sweepEnv * destinationFreq);
	EqPan(
		BLowPass(Saw(freq), freq * 6, 0.6),
		Rand(-1, 1)
	) * (1 - (1 / numTone)) * 1.5 / numVoices
}.Sum;
Limiter(BLowPass(snd, 2000 + (outerEnv * 18000), 0.5) * (2 + outerEnv) * ampEnvelope, 1, 0.01)

(* http://earslap.com/article/sctweeting.html *)
var a = LocalIn(1, 0);
var x = SinOsc((Decay(Impulse([4, 4.005], 0), 1000 * a.Abs) * 50), a).Distort;
x <! LocalOut(x.Sum)

(* http://earslap.com/article/sctweeting.html ; wait to start *)
var f = LocalIn(2, 0).Tanh;
var k = Latch(f.first.Abs, Impulse(0.5, 0));
f <! LocalOut(f + AllpassN(Pulse([2, 3], k * 0.01 + 0.000001) * 0.9, 1, k * 0.3, 100 * k))

(* http://earslap.com/article/sctweeting.html *)
var f = LocalIn(2, 0).Tanh;
var k = Latch(f.first.Abs, Impulse(1 / 4, 0));
f <! LocalOut(f + CombC(Blip([4, 6], 100 * k + 50) * 0.9, 1, k * 0.3, 50 * f))

(* http://earslap.com/article/sctweeting.html *)
{
	var a = LfNoise1(Rand(0, 0.2));
	DelayC(
		Bpf(
			WhiteNoise() * Dust2(a * a * 4 ^ 2).Lag(0.008),
			IRand(0, 10000) + 300,
			0.09
		),
		3,
		a * 1.5 + 1.5
	) * 45
} !+ 80

(* http://earslap.com/article/sctweeting.html *)
AllpassC(
	SinOsc(55, 0).Tanh,
	0.4,
	TExpRand(0.0002, 0.4, Impulse(8, 0)).RoundTo([0.002, 0.0004]),
	2
)
