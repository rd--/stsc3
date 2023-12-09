(* alien froggies (jmcc) #1 ; left-to-right *)
{ :tr |
	var TrRand = { :tr :lo :hi | TRand(lo, hi, tr) };
	var TrExpRand = { :tr :lo :hi | TExpRand(lo, hi, tr) };
	var r = tr.TrRand(-0.2, [0.1, 0.2]).Exp.Mul(11).Fold(1, 30);
	r.Formant(
		tr.TrExpRand([200, 300], 3000),
		tr.TrRand([0, 1], 9).MulAdd(r, r)
	).Mul(0.05)
}.OverlapTexture(0.5, 0.25, 5).Mix

(* alien meadow (jmcc) #6 ; left-to-right *)
{ :tr |
	var trRand = { :lo :hi | TRand(lo, hi, tr) };
	var z = trRand(0, 5000);
	var f = SinOsc(trRand(0, 20), 0).MulAdd(0.1 * z, z);
	var a = SinOsc(trRand(0, 20), 0).MulAdd(0.05, 0.05);
	SinOsc(f, 0).Pan2(trRand(-1, 1), a)
}.OverlapTexture(6, 2, 6).Mix

(* analog bubbles (jmcc) ; method notation *)
0.4
	.LfSaw(0)
	.MulAdd(24, [8, 7.23]
		.LfSaw(0)
		.MulAdd(3, 80))
	.MidiCps
	.SinOsc(0)
	.Mul(0.04)
	.CombN(0.2, 0.2, 4)
	.Mul(0.1)

(* analog bubbles (jmcc) ; as above ; one line *)
0.4.LfSaw(0).Mul(24).Add([8, 7.23].LfSaw(0).MulAdd(3, 80)).MidiCps.SinOsc(0).Mul(0.04).CombN(0.2, 0.2, 4).Mul(0.1)

(* analog bubbles (jmcc) ; alternate linearisation *)
[8, 7.23].LfSaw(0)
	.MulAdd(3, 80)
	.Add(0.4
		 .LfSaw(0)
		 .Mul(24))
	.MidiCps
	.SinOsc(0)
	.Mul(0.05)
	.CombN(0.2, 0.2, 4)
	.Mul(0.1)

(* analog bubbles (jmcc) ; as above ; one line *)
[8, 7.23].LfSaw(0).MulAdd(3, 80).Add(0.4.LfSaw(0).Mul(24)).MidiCps.SinOsc(0).Mul(0.05).CombN(0.2, 0.2, 4).Mul(0.1)

(* analog bubbles (jmcc) #1 ; variable bindings *)
var o = LfSaw([8, 7.23], 0) * 3 + 80;
var m = LfSaw(0.4, 0) * 24 + o;
CombN(SinOsc(m.MidiCps, 0) * 0.04, 0.2, 0.2, 4) * 0.1

(* analog bubbles (jmcc) #1 ; applicative order *)
Mul(
	CombN(
		MulAdd(
			SinOsc(
				MulAdd(
					LfSaw(0.4, 0),
					24,
					MulAdd(
						LfSaw([8, 7.23], 0),
						3,
						80
					)
				).MidiCps,
				0),
			0.05,
			0),
		0.2,
		0.2,
		4
	),
	0.1
)

(* analog bubbles (jmcc) ; as above ; one line *)
Mul(CombN(Mul(SinOsc(MidiCps(MulAdd(LfSaw(0.4, 0), 24, MulAdd(LfSaw([8, 7.23], 0), 3, 80))), 0), 0.05), 0.2, 0.2, 4), 0.1)

(* berlin 1977 (jmcc) #4 ; event control *)
Voicer(16) { :e |
	var freq = (e.x * 24 + 48).MidiCps;
	var env = Decay2(Trig(e.w, 0.001), 0.05 * e.y, 2 * e.y);
	var amp = env * e.z + 0.02;
	var filt = env * (FSinOsc(0.17, 0) * 800) + 1400;
	var pw = SinOsc(0.08, [0, 0.5 * pi]) * 0.45 + 0.5;
	var s = Pulse(freq, pw) * amp;
	CombC(Rlpf(s, filt, 0.15), 0.2, [0.2, 0.17], 1.5) * LagUd(e.w, 0, 2 + e.y)
}.Mix

(* berlin 1977 (jmcc) #4 ; var syntax *)
var sequ = { :s :tr | Demand(tr, 0, Dseq(inf, s)) };
var sequR = { :s :tr | Demand(tr, 0, Dshuf(inf, s)) };
var clockRate = MouseX(5, 20, 1, 0.2);
var clockTime = 1 / clockRate;
var clock = Impulse(clockRate, 0);
var patternList = [55, 60, 63, 62, 60, 67, 63, 58];
var note = sequ(patternList, clock);
var clock16 = PulseDivider(clock, 16, 0);
var noteTrs = sequR([-12, -7, -5, 0, 2, 5], clock16) + note;
var freq = noteTrs.MidiCps;
var env = Decay2(clock, 0.05 * clockTime, 2 * clockTime);
var amp = env * 0.1 + 0.02;
var filt = env * FSinOsc(0.17, 0) * 800 + 1400;
var pw = SinOsc(0.08, [0, 0.5 * pi]) * 0.45 + 0.5;
var s = Pulse(freq, pw) * amp;
CombC(Rlpf(s, filt, 0.15), 0.2, [0.2, 0.17], 1.5)

(* bowed string (jmcc) ; voicer *)
Voicer(16) { :e |
	var f = (e.x * 24 + 48).MidiCps;
	var k = DynRingzBank(
		BrownNoise() * e.z * LagUd(e.w, e.y * 0.1, e.y * 4),
		12.arithmeticSeries(f, f),
		12.geometricSeries(1, Rand(0.7, 0.9)),
		{ Rand(1, 3) } ! 12
	);
	(k * 0.1).SoftClip.EqPan2(e.i * 2 - 1)
}.Mix * 0.1

(* bowed string (jmcc) ; Rand *)
var root = 5;
var scale = [0 2 4 5 7 9 11] + root;
var oct = [24 36 48 60 72 84];
var f = (scale.atRandom + oct.atRandom).MidiCps;
var x = { BrownNoise() } ! 2 * 0.007 * (LfNoise1(ExpRand(0.125, 0.5)) * 0.6 + 0.4).Max(0);
var k = RingzBank(x, 12.arithmeticSeries(f, f), 12.geometricSeries(1, Rand(0.7, 0.9)), { Rand(1, 3) } ! 12);
(k * 0.1).SoftClip

(* bowed string (jmcc) ; .randomFloat *)
var root = 5;
var scale = [0, 2, 4, 5, 7, 9, 11] + root;
var oct = [24, 36, 48, 60, 72, 84];
var f = (scale.atRandom + oct.atRandom).MidiCps;
var x = { BrownNoise() } ! 2 * 0.007 * (LfNoise1(ExpRand(0.125, 0.5)) * 0.6 + 0.4).Max(0);
var k = RingzBank(
	x,
	12.arithmeticSeries(f, f),
	12.geometricSeries(1, 0.7.randomFloat(0.9)),
	{ 1.randomFloat(3) } ! 12
);
(k * 0.1).SoftClip

(* bowed string (jmcc) ; klank *)
var root = 5;
var scale = [0 2 4 5 7 9 11] + root;
var oct = [24 36 48 60 72 84];
var f = (scale.atRandom + oct.atRandom).MidiCps;
var x = { BrownNoise() } ! 2 * 0.007 * (LfNoise1(ExpRand(0.125, 0.5)) * 0.6 + 0.4).Max(0);
var d = [
	12.arithmeticSeries(f, f),
	12.geometricSeries(1, Rand(0.7, 0.9)),
	{ Rand(1, 3) } ! 12
].transposed.concatenation;
var k = Klank(x, 1, 0, 1, d);
(k * 0.1).SoftClip

(* coolant (jmcc) *)
{ RingzBank(OnePole(BrownNoise() * 0.002, 0.95), { Rand(40, 2040) } ! 10, [0.1], [1]) } ! 2

(* coolant (jmcc) ; default values *)
{ RingzBank(OnePole(BrownNoise() * 0.002, 0.95), { Rand(40, 2040) } ! 10, nil, nil) } ! 2 * 0.1

(* coolant (jmcc) ; Rand->rand *)
{ RingzBank(OnePole(BrownNoise() * 0.002, 0.95), { 40 + 2000.randomFloat } ! 10, [0.1], [1]) } ! 2

(* coolant (jmcc) ; Ringz *)
var o = OnePole(BrownNoise() * 0.01, 0.95);
{ Ringz(o, Rand(40, 2040), 1) * 0.1 } !^ 10

(* deep trip (jmcc) #9 ; texture=overlap,12,4,4,inf *)
var f = (LfNoise1(Rand(0, 0.3)) * 60 + 70).MidiCps;
var a = LfNoise2(f * Rand(0, 0.5)) * (LfNoise1(Rand(0, 8)) * SinOsc(Rand(0, 40), 0) * 0.1).Max(0);
var s = Pan2(SinOsc(f, 0) * a, LfNoise1(Rand(0, 5)), 1);
{ CombN(s, 0.5, { Rand(0, 0.2) + 0.3 } ! 2, 20) } !> 2 + s

(* deep trip (jmcc) #9 ; graph rewrite ; left-to-right *)
{ :tr |
	var trRand = { :lo :hi | TRand(lo, hi, tr) };
	var f = LfNoise1(trRand(0, 0.3)).MulAdd(60, 70).MidiCps;
	var a = LfNoise2(f.Mul(trRand(0, 0.5))).Mul((LfNoise1(trRand(0, 8)).Mul(SinOsc(trRand(0, 40), 0)).Mul(0.1)).Max(0));
	var s = SinOsc(f, 0).Mul(a).Pan2(LfNoise1(trRand(0, 5)), 1);
	{ s.CombN(0.5, { trRand(0, 0.2) + 0.3 } ! 2, 20) } !+ 2 + s
}.OverlapTexture(12, 4, 4).Mix

(* hard sync sawtooth with lfo (jmcc) #6 ; graph-rewrite ; left-to-right *)
var txt = { :tr |
	var f = TRand(0, 50, tr).MulAdd(1, 30).MidiCps;
	SyncSaw(
		[f, f + 0.2],
		SinOsc(0.2, { TRand(0, pi, tr).Mul(2) } ! 2).Mul(2).MulAdd(f, f * 3)
	).Mul(0.05)
}.OverlapTexture(4, 4, 4).Mix;
txt.CombN(0.3, 0.3, 4) + txt.reversed

(* harmonic swimming (jmcc) #1 *)
var l = Line(0, -0.02, 60);
(1 .. 20).collect { :h |
	var n = LfNoise1({ Rand(-4, 4) } ! 2 + 6) * 0.02 + l;
	SinOsc(50 * (h + 1), 0) * n.Max(0)
}.Sum

(* harmonic tumbling (jmcc) #1 *)
var t = XLine([10, 11], 0.1, 60);
(0 .. 10).collect { :h |
	var e = Decay2(Dust(t) * 0.02, 0.005, Rand(0, 0.5));
	SinOsc(80 * (h + 1), 0) * e
}.Mix

(* hell is busy (jmcc) #1 ; graph rewrite ; left-to-right *)
{ :tr |
	var trRand = { :lo :hi | TRand(lo, hi, tr) };
	var e = LfPulse(trRand(1, 11), 0, trRand(0, 0.7)) * 0.04;
	SinOsc(trRand(400, 2400), 0).Pan2(trRand(-1, 1), e)
}.OverlapTexture(4, 4, 8).Mix

(* lfo modulation (jmcc) #1 *)
var o = SinOsc(0.05, 0) * 80 + 160;
var p = SinOsc([0.6, 0.7], 0) * 3600 + 4000;
var s = Rlpf(LfPulse(o, 0, 0.4) * 0.05, p, 0.2);
CombL(s, 0.3, [0.2, 0.25], 2)

(* moto rev (jmcc) #1 *)
var f = SinOsc(0.2, 0) * 10 + 21;
var s = LfPulse(f, [0, 0.1], 0.1);
Rlpf(s, 100, 0.1).Clip2(0.4)

(* police state (jmcc) #2 *)
var e = LfNoise2(LfNoise2([0.4, 0.4]) * 90 + 620) * (LfNoise2([0.3, 0.3]) * 0.15 + 0.18);
CombL(
	{
		var f = SinOsc(Rand(0.02, 0.12), Rand(0, 2.pi)) * Rand(600, 1600) + Rand(-300, 300);
		Pan2(SinOsc(f, 0) * LfNoise2(100 + Rand(-20, 20)) * 0.1, Rand(-1, 1), 1)
	} !> 4 + e,
	0.3,
	0.3,
	3
) * 0.5

(* pond life (jmcc) #1 ; texture=overlap,8,8,4,inf ; requires=kr *)
var f = SinOsc(Rand(20, 50), 0) * Rand(100, 400) + LinRand(500, 2500, 0);
var o = SinOsc(f.kr, 0) * LfPulse(3 / Rand(1, 9), 0, Rand(0.2, 0.5)).kr * 0.04;
EqPan(o, Rand(-1, 1))

(* pulsing bottles (jmcc) #2 *)
var n = 6;
{
	var a = LfPulse(4 + Rand(0, 10), 0, Rand(0, 0.7)) * 0.8 / n;
	var l = SinOsc(0.1 + Rand(0, 0.4), Rand(0, 2 * pi));
	Pan2(Resonz(WhiteNoise(), 400 + LinRand(0, 7000, 0), 0.01), l, 1) * a
} !> n

(* scratchy (jmcc) #1 *)
var n = { BrownNoise() } ! 2 * 0.5 - 0.49;
var f = n.Max(0) * 20;
Rhpf(f, 5000, 1)

(* sprinkler (jmcc) #1 *)
Bpz2(WhiteNoise() * LfPulse(LfPulse(0.09, 0, 0.16) * 10 + 7, 0, 0.25) * 0.1)

(* theremin (jmcc) ; event control *)
Voicer(16) { :e |
	var freq = Lag(LinExp(e.y, 0, 1, 4000, 200), 0.8);
	var a = SinOsc(freq + (freq * SinOsc(4 + 3 * e.j, 0) * 0.02), 0) * e.x * 0.6 * Lag(e.w, 0.2);
	Pan2(a, e.i * 0.25, 0.5 + e.z)
}.Mix * 0.5

(* tremulate (jmcc) ; event control ; requires=voicer *)
var s = Voicer(16) { :e |
	var s = SinOsc(e.x * 400 + 500 * [1 1.2 1.5 1.8], 0); (* just minor seventh chord, 1:1 5:4 3:2 9:5 *)
	var a = LfNoise2({ Rand(30, 90) } ! 4 * (0.75 + e.j)).Max(0) * e.z;
	Pan2(s, { Rand(-1, 1) } ! 4 + (e.i * 2 - 1), a * LagUd(e.w, 0, e.k * 2)).Sum
}.Mix * 0.5;
CombN(s, 0.1, 0.1, 1)

(* uplink (jmcc) #2 ; texture=overlap,4,1,5,inf *)
var osc = {
	var e = LfPulse(Rand(0, 4), 0, Rand(0, 1)) * Rand(0, 8000) + Rand(0, 2000);
	LfPulse(Rand(0, 20), 0, Rand(0, 1)) * e
};
Pan2(LfPulse(osc() + osc(), 0, 0.5) * 0.04, Rand(0, 0.8), 1)

(* what was I thinking? ; jmcc *)
var z = Rlpf(
	Pulse(
		MulAdd(SinOsc(4, 0), 1, 80).Max(
			Decay(LfPulse(0.1, 0, 0.05) * Impulse(8, 0) * 500, 2)
		),
		MulAdd(LfNoise1(0.157), 0.4, 0.5)
	) * 0.04,
	MulAdd(LfNoise1(0.2), 2000, 2400),
	0.2
);
var y = z * 0.6;
z + [
	CombL(y, 0.06, MulAdd(LfNoise1(0.3.Rand), 0.025, 0.035), 1) +
	CombL(y, 0.06, MulAdd(LfNoise1(0.3.Rand), 0.025, 0.035), 1)
	,
	CombL(y, 0.06, MulAdd(LfNoise1(0.3.Rand), 0.025, 0.035), 1) +
	CombL(y, 0.06, MulAdd(LfNoise1(0.3.Rand), 0.025, 0.035), 1)
]

(* what was i thinking? (jmcc) #2 *)
var i = LfPulse(0.1, 0, 0.05) * Impulse(8, 0) * 500;
var f = (SinOsc(4, 0) + 80).Max(Decay(i, 2));
var p = Pulse(f, LfNoise1(0.157) * 0.4 + 0.5) * 0.04;
var z = Rlpf(p, LfNoise1(0.2) * 2000 + 2400, 0.2) * 0.25;
var y = z * 0.6;
{
	[y, y].collect { :i |
		CombL(i, 0.06, LfNoise1(0.3.Rand) * 0.025 + 0.035, 1)
	}.Sum
} ! 2 + z

(* wind metals (jmcc) ; texture=overlap,5,2,12,inf *)
var n = 6;
var exc = { BrownNoise() } ! 2 * 0.007 * (LfNoise1(ExpRand(0.125, 0.5)) * 0.75 + 0.25).Max(0);
var f = { Rand(0, Rand(500, 8000)) + ExpRand(60, 4000) } ! n;
var dt = { Rand(0.1, 2) } ! n;
var s = RingzBank(exc, f, nil, dt) * 0.1;
s.SoftClip

(* zizle (jmcc) #SC3d1.5 ; texture=overlap,4,4,12,inf *)
var a = { :f | (SinOsc(f * [Rand(0.7, 1.3), 1], { Rand(0, 2 * pi) } ! 2) * 0.1).Sum };
var o = SinOsc(Rand(24, 108).MidiCps, Rand(0, 2 * pi));
var s = o * a(ExpRand(0.3, 8)).Max(0) * a(ExpRand(6, 24)).Abs;
EqPan(s, Rand(-1, 1))

(* ---- analog bubbles (jmcc) #1 ; requires=keywords *)
var o = LfSaw(
	freq: [8, 7.23],
	iphase: 0
) * 3 + 80;
var m = LfSaw(
	freq: 0.4,
	iphase: 0
) * 24 + o; (* glissando function *)
var s = SinOsc(
	freq: m.MidiCps,
	phase: 0
) * 0.04;
CombN(
	in: s,
	maxdelaytime: 0.2,
	delaytime: 0.2,
	decaytime: 4
) * 0.1 (* echoing sine wave *)

(* ---- analog bubbles (jmcc) #1 ; left-to-right *)
0.4.LfSaw(0).Mul(24).Add([8, 7.23].LfSaw(0).MulAdd(3, 80)).MidiCps.SinOsc(0).Mul(0.04).CombN(0.2, 0.2, 4)

(* ---- analog bubbles (jmcc) #1 ; left-to-right *)
var o = LfSaw([8, 7.23], 0).MulAdd(3, 80);
var m = LfSaw(0.4, 0).MulAdd(24, o);
SinOsc(m.MidiCps, 0).Mul(0.04).CombN(0.2, 0.2, 4)

(* ---- babbling brook (jmcc) #SC3 ; left-to-right *)
var b = { :f :m :a :g |
	BrownNoise()
		.OnePole(0.99)
		.Rhpf(BrownNoise()
			.Lpf(f)
			.MulAdd(m, a), 0.03)
		.Mul(g)
};
[
	{ b(14, 400, 500, 0.06) } ! 2,
	{ b(20, 800, 1000, 0.10) } ! 2
].Sum

(* ---- birdies (jmcc) #6 ; muladd *)
{
	var p1 = MulAdd(LfPulse(0.4 + 1.Rand, 0, 0.8.Rand + 0.1), 3.Rand + 4, 2);
	var p2 = MulAdd(LfPulse(0.4 + 1.Rand, 0, 0.8.Rand + 0.1), 3.Rand + 4, 0);
	var p3 = MulAdd(LfPulse(0.2 + 0.5.Rand, 0, 0.4), 0.02, 0);
	var sw = MulAdd(LfSaw(p1 + p2, 0), (1000 + 800.Rand).Neg, 4000 + 1200.Rand2);
	var freq = Lag(sw, 0.05);
	var amp = Lag(p3, 0.3);
	Pan2(SinOsc(freq, 0) * amp, 1.Rand2, 1)
}.overlap(7, 4, 4)

(* ---- coolant (jmcc) ; filtermethods *)
{ BrownNoise().Mul(0.002).OnePole(0.95).RingzBank({ 40 + 2000.Rand } ! 10, [0.1], [1]) } ! 2

(* ---- modal space (jmcc) #8 ; left-to-right *)
var b = [0 2 3.2 5 7 9 10].asLocalBuf;
var k = DegreeToKey(b, MouseX(0, 15, 0, 0.1), 12);
var c = { :n :r |
	var o = SinOsc((k + r + (n * 0.04)).MidiCps, 0).Mul(0.1);
	var t = LfPulse([48, 55].MidiCps, 0, 0.15);
	var f = SinOsc(0.1, 0).MulAdd(10, r).MidiCps;
	var d = t.Rlpf(f, 0.1).Mul(0.1);
	var m = o + d;
	m.CombN(0.31, 0.31, 2) + m
};
var n = LfNoise1([3, 3]);
c(n, 48) + c(n, 72) * 0.25

(* ---- jmcc - ostinoodles ; requires=keywords *)
var z = { :tr |
	var sequ = { :s :tr |
		Demand(
			trig: tr,
			reset: 0,
			demandUGens: Dseq(
				repeats: inf,
				list: s
			)
		)
	};
	var root = 81 + TRand(
		lo: -6,
		hi: 6,
		trig: tr
	);
	var major = [0, 2, 4, 5, 7, 9, 11].asLocalBuf;
	var offset = TRand(
		lo: -16,
		hi: 16,
		trig: tr
	);
	var sequence = DegreeToKey(
		bufnum: major,
		in: TScramble(
			trigger: tr,
			inputs: [0, 1, 2, 3] + offset
		),
		octave: 12
	) + root;
	var f = TxLine(
		start: TExpRand(
			lo: 4,
			hi: 24,
			trig: tr
		),
		end: TExpRand(
			lo: 4,
			hi: 24,
			trig: tr
		),
		dur: 12,
		trig: tr
	);
	var trig = Impulse(
		freq: f,
		phase: 0
	);
	var freq = sequ(
		value: sequence.MidiCps,
		value: trig
	);
	var sig = LfTri(
		freq: freq.kr,
		iphase: 0
	) * Decay2(
		in: trig,
		attackTime: 0.004,
		decayTime: 0.3
	).kr * 0.1;
	Pan2(
		in: sig,
		pos: TRand(
			lo: -1,
			hi: 1,
			trig: tr
		),
		level: 1
	)
}.OverlapTexture(6, 3, 6).Mix;
6.timesRepeat {
	z := AllpassN(
		in: z,
		maxdelaytime: 0.04,
		delaytime: {
			Rand(
				lo: 0,
				hi: 0.04
			)
		} ! 2,
		decaytime: 16)
};
z

(* ---- scratchy ; jmcc ; left-to-right *)
{ BrownNoise() }.duplicate(2).MulAdd(0.5, -0.49).Max(0).Mul(20).Rhpf(5000, 1)

(* ---- slow beating sines (jmcc) #7 ; texture=xfade,4,4,inf *)
var n = 20;
var d = 5;
var p = []; (* OrderedCollection() *)
var q = []; (* OrderedCollection() *)
n.timesRepeat {
	var freq = Rand(24, 84).MidiCps;
	p.add(freq);
	{ p.add(freq + d.Rand2) } ! 2;
	{ q.add(freq + d.Rand2) } ! 3
};
[p, q].collect { :freq |
	SinOscBank(freq, [1], { Rand(0, 2 * pi) } ! (3 * n))
} * 0.1 / n

(* ---- police state ; jmcc ; keywords *)
var n = 4; (* number of sirens *)
var node = {
	Pan2(
		in: SinOsc(
			freq: SinOsc(
				freq: 0.1.Rand + 0.02,
				phase: 2 * pi.Rand
			) * 600.Rand + 1000 + 300.Rand2,
			phase: 0
		),
		pos: 1.Rand2,
		level: LfNoise2(freq: 100 + 20.Rand2) * 0.1
	)
};
var e = LfNoise2(
	freq: LfNoise2(
		freq: [0.4, 0.4]
	) * 90 + 620
) * (
	LfNoise2(
		freq: [0.3, 0.3]
	) * 0.15 + 0.18
);
CombL(
	in: node:/0 !+ 4 + e,
	maxdelaytime: 0.3,
	delaytime: 0.3,
	decaytime: 3
) * 0.5

(* ---- repeating harmonic klank (jmcc) ; requires=Klank *)
{ :tr |
	var p = 8;
	var s = Decay(Dust(0.8) * 0.01, 3.4) * LfSaw(TRand(0, 40), 0); (* linrand *)
	var f = Choose(tr, [400, 500, 600, 700, 800, 900, 1000, 1200, 1400, 1500, 1600]);
	{
		Klank(
			s, 1, 0, 1,
			[
				{ f * TRand(1, 13) } ! p,
				[1],
				{ TRand(0.4, 3.4) } ! p
			].asKlankSpec
		)
	} ! 2
}.OverlapTexture(8, 2, 4).Mix

(* ---- sample and hold liquidities (jmcc) #4 ; requires=keywords *)
var r = MouseX(
	minval: 1,
	maxval: 200,
	warp: 1,
	lag: 0.1
);
var t = r.Recip;
var c = Impulse(
	freq: r,
	phase: 0
) * 0.4;
var cf = MouseY(
	minval: 100,
	maxval: 8000,
	warp: 1,
	lag: 0.1
);
var f = Latch(
	in: WhiteNoise() * cf * 0.5 + cf,
	trig: c
);
var p = Latch(
	in: WhiteNoise(),
	trig: c
);
var i = EqPan2(
	in: SinOsc(
		freq: f,
		phase: 0
	),
	pos: p
) * Decay2(
	in: c,
	attackTime: 0.1 * t,
	decayTime: 0.9 * t
);
CombN(
	in: i,
	maxdelaytime: 0.3,
	delaytime: 0.3,
	decaytime: 2
)

(* ---- strummable metals ; use mouse to strum strings ; jmcc ; requires=Klank *)
var mouseX = MouseX(0, 1, 0, 0.2);
var strings = (1 .. 8).collect { :i |
	var trigger = Hpz1(mouseX > (0.25 + (i * 0.07))).Abs;
	var pluck = PinkNoise() * Decay(trigger, 0.05).kr * 0.04;
	var n = 15;
	var z = [{ 300 * i + LinRand(0, 8000, 0) } ! n, nil, { Rand(1, 4) } ! n].asKlankSpec;
	Pan2(Klank(pluck, 1, 0, 1, z), i * 0.2 - 0.5, 1)
}.Sum;
LeakDc(
	Lpf(string, 12000),
	0.995
)

(* ---- why supercollider (jmcc) #0 ; requires=keywords *)
var s = {
	Resonz(
		in: Dust(
			density: 0.2
		) * 50,
		freq: Rand(
			lo: 200,
			hi: 3200
		),
		bwr: 0.003
	)
} !+ 10;
var z = DelayN(
	in: s,
	maxdelaytime: 0.048,
	delaytime: 0.048
);
var x = {
	CombL(
		in: z,
		maxdelaytime: 0.1,
		delaytime: LfNoise1(
			freq: Rand(
				lo: 0,
				hi: 0.1
			)
		) * 0.04 + 0.05,
		decaytime: 15
	)
} !+ 7;
4.timesRepeat {
	x := AllpassN(
		in: x,
		maxdelaytime: 0.050,
		delaytime: {
			Rand(
				lo: 0,
				hi: 0.05
			)
		} ! 2,
		decaytime: 1
	)
};
s + (x * 0.2)
