;; pkt 07 (f0) ; https://fredrikolofsson.com/f0blog/pact-februari/ ; helper
var sinosc = { :f :l :r | LinExp(SinOsc(f, 0), -1, 1, l, r) };
GVerb(
	LeakDc(
		SinOsc(
			SinOsc(
				SinOsc(
					SinOsc(
						SinOsc(
							SinOsc(
								SinOsc(1, 0) *
								2 +
								sinosc(1/2, 1, 2),
								0) *
							8 +
							sinosc(1/4, 4, 8),
							0) *
						32 +
						sinosc(1/8, 16, 32),
						0) *
					128 +
					sinosc(1/16, 64, 128),
					0) *
				512 +
				sinosc(1/32, 256, 512),
				0) *
			2048 +
			sinosc(1/64, 1024, 2048),
			0) * 0.1,
		0.995),
	16, 8, 0.75, 0.5, 15, 1, 0.7, 0.5, 300)
* 0.25

;; red frik (f0)
var tr = Impulse(0.1, 0);
var o1 = SinOsc(TRand(0.3, 5, tr), 0) * TRand(0, 0.5, tr) + TRand(0.49, 0.56, tr);
var o2 = SinOsc(o1, 0) * TRand(0.3, 0.6, tr) + TRand(0.3, 0.5, tr);
Rhpf(
	{ BrownNoise() } ! 2,
	TRand(0.3, 3, tr),
	o2
) * 0.1

;; https://sccode.org/1-4S6 ; f0 ; 't'
var p = Saw([3, 4]) * (Saw(1) * 32 + 128) + DmdFor(1, 0, (Dseq(1, [0, 8, 1, 5]) * [1, 4, 8]).flop);
var o = SinOsc(Saw(3) * 64 + 99, p) / 9;
CombN(o, 1 / 4, 1 / 2.125, SinOsc(0.005, 1.5 * pi).Range(0, 6)).transpose.sum

;; tw 0134 (f0)
var n = 50;
var z = { :i |
	Ringz(
		Blip(LfSaw(i + 1 / [3, 4], 0) > (LfSaw(i + 1 / 8, 0) + 1) * (n / 2) + n, i + [2, 3]) *
		LfSaw(i + 1 / n, i / (n / 2)), i + 1 * (n * 2 - 1),
		0.1
	)
};
(1 .. n).collect(z).mean / 5

;; https://sccode.org/1-4Qy ; f0 ; 0246 ; requires=DynRingzBank
var b = [1 .. 9] * 1.5;
var d = [2 / b, 3 / b];
var e = LfPulse(d, 0, 0.5);
CombC(
	DynRingzBank(e, (LfPulse(1 / b, 0, 0.5) * b + 50).MidiCps, 0.0002, b / 9),
	2,
	2 - Lpf(e, 50),
	1
).transpose.sum

;; https://sccode.org/1-4Qy ; f0 ; 0318
var b = [1 .. 3];
var o1 = SinOscFb(13 * 13 * b, 1 / 3);
var o2 = SinOscFb(b / 13, 1);
var o3 = SinOscFb(1 / (13 .. 3), 0) + 133 * b;
var o4 = SinOscFb(b, 1) % 1;
var o5 = SinOscFb(b / 333, o4) % 1;
var o6 = SinOscFb(o3, o5);
Splay2(o1 * o2 / 13 + o6) / 3

;; https://sccode.org/1-4Qy ; f0 ; 0335 ; with keywords
var o = GrainFm(
	numChan: 1,
	trigger: LfSaw([0.5, 0.6], 0),
	dur: 16,
	carfreq: LfSaw(5, 0) * LfSaw(0.015, 0) + 1 * 98,
	modfreq: (2 ** LfSaw(4, 0)).RoundTo(0.5) * 99,
	index: 2 ** LfSaw(1 / [8, 9], 0) * 8,
	pan: 0,
	envbufnum: -1,
	maxGrains: 512
);
(o / 2).Tanh

;; https://twitter.com/redFrik/status/1254441448327479299 ; f0 ; rd (edit)
var b = [1, 3, 5, 8, 10];
var e = [3, 2 / 3, 4, 3 / 2, 2];
var c = 0.021;
var d = LfTri(b / 999, 0) % 1;
var m = LfTri(b * c, 0);
var l = m * 7 + 20 + Dseq(inf, b % m * 5 + 6);
var j = DmdFor(e / (12 ** m), 0, l);
var k = DegreeToKey(b.asLocalBuf, j, 12);
var o = SinOscFb(k.MidiCps, LfTri(c / b + 1 / 3, Decay2(Impulse([2 / 3, 1.5, 3, 1.5, 3], 0), c, d)) * d);
FreeVerb(Splay2(o), 0.1, 1, 0.5) * 0.1

;; <https://twitter.com/redFrik/status/1452954849885163525> ; f0
var i = Rand(1, 64);
var x = (SinOsc(i % 9.33, 0) * 5 + 5).Ceil;
var t = SinOsc(2 ** (i % 11) * 150 / x, 0);
var y = Hpz1(x).Abs > 0;
var f = LinExp(t, -1, 1, Latch(LinExp(SinOsc(i % 4.4, 0), -1, 1, 9, 999), y), Latch(LinExp(SinOsc(i % pi, 0), -1, 1, 99, 9000), y));
Pan2(Blip(f, t + 2) * (1 - t), SinOsc(0.1, i), Line(0.2, 0, 9, 2).Min(0.6) ** 2)
