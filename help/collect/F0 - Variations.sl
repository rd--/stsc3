(* pkt 07 (f0) ; https://fredrikolofsson.com/f0blog/pact-februari/ ; helper *)
var sinOsc = { :f :l :r | LinExp(SinOsc(f, 0), -1, 1, l, r) };
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
								sinOsc(1/2, 1, 2),
								0) *
							8 +
							sinOsc(1/4, 4, 8),
							0) *
						32 +
						sinOsc(1/8, 16, 32),
						0) *
					128 +
					sinOsc(1/16, 64, 128),
					0) *
				512 +
				sinOsc(1/32, 256, 512),
				0) *
			2048 +
			sinOsc(1/64, 1024, 2048),
			0) * 0.1,
		0.995),
	16, 8, 0.75, 0.5, 15, 1, 0.7, 0.5, 300)
* 0.25

(* red frik (f0) *)
var tr = Impulse(0.1, 0);
var o1 = SinOsc(TRand(0.3, 5, tr), 0) * TRand(0, 0.5, tr) + TRand(0.49, 0.56, tr);
var o2 = SinOsc(o1, 0) * TRand(0.3, 0.6, tr) + TRand(0.3, 0.5, tr);
Rhpf(
	{ BrownNoise() } ! 2,
	TRand(0.3, 3, tr),
	o2
) * 0.1

(* https://sccode.org/1-4S6 ; f0 ; 't' *)
var p = Saw([3, 4]) * (Saw(1) * 32 + 128) + DmdFor(1, 0, (Dseq(1, [0, 8, 1, 5]) * [1, 4, 8]).flop);
var o = SinOsc(Saw(3) * 64 + 99, p) / 9;
CombN(o, 1 / 4, 1 / 2.125, SinOsc(0.005, 1.5 * pi).Range(0, 6)).transposed.Sum

(* tw 0120 (f0) ; requires=kr *)
var z = LfTri(1 / [7, 8], 0) * LfTri(1 / 9, 0) * 99;
var l = (60 .. 79).MidiCps;
var f = Select(z, l);
var w = LfTri(1 / [3, 4], 0) % 1;
var o = VarSaw(f.kr, 0, w.kr);
CombN(o, 1, 1 / [5, 6], 8) / 8

(* tw 0121 (f0) ; requires=kr *)
var z = SinOsc(1 / [8, 7], 0) * SinOsc(1 / 30, 0) * 9;
var l = (0 .. 7) * 6 + 56;
var m = SinOsc(1 / [4, 3], 0);
var o = SinOsc(Select(z, l).kr.MidiCps, 0) * m;
CombN(o, 1, 1 / [6, 5], 9).Tanh * 0.1

(* tw 0134 (f0) *)
var n = 50;
(1 .. n).collect { :i |
	Ringz(
		Blip(LfSaw(i + 1 / [3, 4], 0) > (LfSaw(i + 1 / 8, 0) + 1) * (n / 2) + n, i + [2, 3]) *
		LfSaw(i + 1 / n, i / (n / 2)), i + 1 * (n * 2 - 1),
		0.1
	)
}.mean / 5

(* https://sccode.org/1-4Qy ; f0 ; 0233 ; requires=kr *)
var b = 1 / [1 4 6 8 11];
var c = LfTri(b / 98, 0);
var q = Dseq(inf, Select(LfTri(b / 99, 0) + c * 5, 1 / b + 59).kr).MidiCps;
Splay(LfTri(DmdFor(b, c, q) + c, 0) / 3)

(* https://sccode.org/1-4Qy ; f0 ; 0246 ; requires=DynRingzBank *)
var b = [1 .. 9] * 1.5;
var d = [2 / b, 3 / b];
var e = LfPulse(d, 0, 0.5);
CombC(
	DynRingzBank(e, (LfPulse(1 / b, 0, 0.5) * b + 50).MidiCps, 0.0002, b / 9),
	2,
	2 - Lpf(e, 50),
	1
).transposed.Sum

(* https://sccode.org/1-4Qy ; f0 ; 0318 *)
var b = [1 .. 3];
var o1 = SinOscFb(13 * 13 * b, 1 / 3);
var o2 = SinOscFb(b / 13, 1);
var o3 = SinOscFb(1 / (13 .. 3), 0) + 133 * b;
var o4 = SinOscFb(b, 1) % 1;
var o5 = SinOscFb(b / 333, o4) % 1;
var o6 = SinOscFb(o3, o5);
Splay(o1 * o2 / 13 + o6) / 3

(* https://sccode.org/1-4Qy ; f0 ; 0333 ; Splay/Stereo *)
(0 .. 7).collect { :i |
	var b = i + 2 * 99;
	var f = SinOscFb(i + 1 / 150, 0).RoundTo(1) + 1 + i * 99 + SinOscFb([3, 2], 0);
	(Formant(f, b, b) * SinOscFb(i + 1 / 130, 0).Max(0)).Tanh
}.Sum.Splay / 7

(* https://sccode.org/1-4Qy ; f0 ; tweet0350 ; Splay *)
var b = (9 .. 1) / 99;
var o = LfSaw(LfSaw(b, b) + 1 * 99, b) * (LfSaw(LfSaw(b, 0) > b, 0) > 0.9);
CombN(
	GVerb(o, 99, 1, b * 9, b, 15, 1, 0.7, 0.5, 300) / 19,
	1,
	b / 9.9,
	9
).transposed.Sum.Splay * 0.9

(* f0 ; https://twitter.com/redFrik/status/1105496695637454848 ; requires=kr *)
var b = (1 .. 15) + 1 / 151;
var w = (VarSaw(b, b, 1 / 5.15) * 1 + 1.5).Ceiling;
var s = Select((VarSaw(1/15, 1/5, b) * 5).Ceiling + 5 / 5, [51 * 1.5, 51, 151]).kr;
var x = Lag(s, b);
var y = VarSaw(5 + b, 0, 0.5);
var z = VarSaw(b, b, b) * b / 5 + [1.5, 5, 1];
var m = VarSaw(5 - b, b, b) * 5 + 5;
var o = VarSaw(w * x + y * z, b, VarSaw(b, 0, 0.5) + 5 / 15) * (m > 1.515);
var f = 1515 ^ (VarSaw(1 - b / 5, 0, 0.5) / 15 + 1 + b);
var rq = 1.5 ^ VarSaw(b, 0, 0.5) / 5;
BLowPass(o, f, rq).Splay / 5

(* https://twitter.com/redFrik/status/1254441448327479299 ; f0 ; rd (edit) *)
var b = [1 3 5 8 10];
var e = [3, 2 / 3, 4, 3 / 2, 2];
var c = 0.021;
var d = LfTri(b / 999, 0) % 1;
var m = LfTri(b * c, 0);
var l = m * 7 + 20 + Dseq(inf, b % m * 5 + 6);
var j = DmdFor(e / (12 ^ m), 0, l);
var k = DegreeToKey(b.asLocalBuf, j, 12);
var o = SinOscFb(k.MidiCps, LfTri(c / b + 1 / 3, Decay2(Impulse([2 / 3, 1.5, 3, 1.5, 3], 0), c, d)) * d);
FreeVerb(o.Splay, 0.1, 1, 0.5) * 0.1

(* <https://twitter.com/redFrik/status/1452954849885163525> ; f0 *)
var i = Rand(1, 64);
var x = (SinOsc(i % 9.33, 0) * 5 + 5).Ceiling;
var t = SinOsc(2 ^ (i % 11) * 150 / x, 0);
var y = Hpz1(x).Abs > 0;
var f = LinExp(t, -1, 1, Latch(LinExp(SinOsc(i % 4.4, 0), -1, 1, 9, 999), y), Latch(LinExp(SinOsc(i % pi, 0), -1, 1, 99, 9000), y));
EqPan(
	Blip(f, t + 2) * (1 - t),
	SinOsc(0.1, i)
) * (Line(0.2, 0, 9).Min(0.6) ^ 2)

(* ---- https://sccode.org/1-4Qy ; f0 ; 0335 ; with keywords *)
var o = GrainFm(
	numChan: 1,
	trigger: LfSaw([0.5, 0.6], 0),
	dur: 16,
	carfreq: LfSaw(5, 0) * LfSaw(0.015, 0) + 1 * 98,
	modfreq: (2 ^ LfSaw(4, 0)).RoundTo(0.5) * 99,
	index: 2 ^ LfSaw(1 / [8, 9], 0) * 8,
	pan: 0,
	envbufnum: -1,
	maxGrains: 512
);
(o / 2).Tanh
