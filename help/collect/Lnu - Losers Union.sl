(* Feedback loop ; https://github.com/lukiss/Losers-Union-SC-Research *)
var i = LocalIn(2, [0 0]);
var o = Splay2(
	LeakDc(
		DelayC(
			i + ((1 + i) ^ 0.999),
			1,
			SinOsc(0.01 / (1 .. 8), 0).ExpRange(0.01, 1)
		),
		0.995
	).Tanh
);
Lpf(o, 5000) / 5 <! LocalOut(o)

(* Broken Saws ; https://github.com/lukiss/Losers-Union-SC-Research *)
var c = [49 175 98 147 65 233];
var r = 3 / 4 / (1 .. c.size);
Splay(
	Bpf(
		VarSaw(
			VarSaw(r + 6, 0, 1 / 2) * 2 + c,
			0,
			VarSaw(1 / r, 0, 0.5) ^ 2
		) ^ (VarSaw(0.001 / r + 8, 0, VarSaw(1 / 3, 0, 0.5).Abs) * 12 + 13),
		VarSaw(
			0.003 / r,
			0,
			VarSaw(r * 8, 0, 0.5)
		).Lag3(0.1).Abs * 4000 + 80,
		1.1 - VarSaw(8 * r, 0, 0.5) ^ 2
	) * VarSaw(r / c, 0, 0.5) / 2,
	VarSaw(1, 0, VarSaw(1, 0, 0.5))
) * Line(0, 1, 20)

(* ChaosGen writes melodies ; https://github.com/lukiss/Losers-Union-SC-Research *)
var f = DegreeToKey(
	[0 2 3 5 7 8 10].asLocalBuf,
	GbmanN([8 4 6 2], 1.2, 2.1).Range(8, 32).Ceiling,
	12
).MidiCps;
var m = SinOsc([8 4 0.5 1], 0);
var e = Perc(m, 0.001, 1 / 8, -4);
Splay(
	DelayC(
		FreeVerb(
			SinOsc(
				f,
				SinOsc(3 * f, 0) * e ^ 2 * (SinOsc(1.1 ^ (0 .. 3) / 16, 0) * 2 + 3)
			) * e,
			m.Abs / 2,
			1 - m ^ 2 * 2,
			1 - m.ExpRange(0.01, 1)
		),
		0.2,
		m.Abs / 2048
	),
	m[3]
) / 2

(* Stretching again ; https://github.com/lukiss/Losers-Union-SC-Research *)
var c = 16;
var f = 41;
var t = (0 .. c).collect { :n |
	PulseDivider(Impulse(f, 0), c, 15 - n)
};
Splay(
	PlayBuf(
		1,
		SfAcquireMono('floating_1'),
		LfNoise2(1 ! c) * 0.1 / c + 1,
		t,
		Sweep(0, MouseX(0.001, 1 / 5, 1, 0.2)) % 1 * 180000,
		0,
		0
	) * Sine(t, c / f),
	7 / 8
) / 2

(* KaosSprinkler ; https://github.com/lukiss/Losers-Union-SC-Research *)
var c = 1.5 ^ [0, 4 .. 12];
var e = StandardN(c, 1, 0.5, 0).ExpRange(0.01, 1);
Splay(
	LeakDc(
		-12.DbAmp * StandardN(
			e * c * 220,
			LfdNoise3(1 ! 4) * 0.1 + 0.97215,
			1 / 4,
			1 / 4
		) * e.reversed,
		0.995
	),
	StandardN(e.last * 14, 1, 0.5, 0)
)

(* Phasing Patterns ; https://github.com/lukiss/Losers-Union-SC-Research *)
var f = (48 + (0 .. 2).collect { :n |
	[0 -5 15 10] + (n * 12)
}.concatenation).MidiCps;
Splay(
	SinOsc(
		f,
		SinOsc(f * 2, 0) * (SinOsc(1 / f.arCosh / (SinOsc(1 / f, 0) * f), 0) * 8 + 8)
	) * (SinOsc(1.2 ^ f.normalizeSum, 0) ^ 1.5 * SinOsc(f.arcTan, 0)),
	SinOsc(1 / 8, 0)
) / 3

(* Mistakes were made ; https://github.com/lukiss/Losers-Union-SC-Research *)
var p = 9 / 7 ^ (0 .. 16);
var c = { :freq |
	LinCongC(freq, 1.1, 0.13, 1, 0)
};
var d = (c(c(p.arcTan) ^ 2 * 4) ^ 4 * 8).Abs;
var t = c(c(d / p.arcTan).RoundTo(1 / d) ^ 4 * d * 8).Sin;
var f = TRand(0, 64, t).Ceiling.MidiCps;
Splay(
	LeakDc(
		Pluck(
			(SinOsc(
				f * p * p.Log,
				0
			) * SinOsc(f * p * p.log / 2, 0) * 2 * d).Sin,
			t,
			0.1,
			t.ExpRange(0.01, 1) / d / p,
			(t ^ 1.5 * f) * (d / p.scramble),
			(p * (1 - t.ExpRange(0.01, 1))).Sin.ExpRange(0.9999, 0.5)
		),
		0.995
	)
).Tanh / 3

(* Grains, Daily ; https://github.com/lukiss/Losers-Union-SC-Research *)
var m = { :lo :hi |
	StandardL(LfdNoise1(1).ExpRange(0.01, 1) * 8 + 0.1, 2, 0.5, 0).ExpRange(lo, hi)
};
var f = {
	StandardL(m(1, 128), m(1, 3), 0.5, 0).ExpRange(40, 5000)
};
var n = 8;
LeakDc(
	GrainFm(
		n,
		Impulse(f() / m(0.5, 8), 0),
		m(0.1, 4) / f(),
		f(),
		m(1 / 4, 4) * f(),
		m(1 / 4, 4),
		StandardL(f() / 2, 1, 0.5, 0) / 2,
		-1,
		1024
	).Splay,
	0.995
).SoftClip / 2

(* Spa Saw Shower Wash ; https://github.com/lukiss/Losers-Union-SC-Research *)
var c = 4096.sineTable(1 / (1 .. 128), [0]).normalize.asWavetable.asLocalBuf;
var w = { :freq |
	LfdNoise3(freq).LinLin(-1, 1, 80, 6880) (* LinCurve *)
};
var f = SinOsc(
	{ Rand(3.3, 4.4) } ! 6,
	0
) * 0.01 + 1 * [82 123 196 147 41 55];
var o = Splay(
	Osc(
		c,
		f,
		Osc(
			c,
			f * 3,
			0
		) * SinOsc(1 / Rand(33, 45), 0) * pi
	) / 5,
	SinOsc(1 / pi, 0)
);
o := BHiPass4(o, w(1 / 7), 1) + Bpf(o, w(1 / 5), 1) + MoogFf(o, w(1 / 3), 2, 0);
4.timesRepeat {
	o := AllpassC(o, 1, { Rand(1 / 16, 1 / 32) } ! 2, 1)
};
o

(* Suspect jazz ; https://github.com/lukiss/Losers-Union-SC-Research *)
var x = HenonL(1 + LfdNoise1(3).ExpRange(0.01, 1) * 8, 1.4, 0.3, 0, 0).Fold2(1);
var t = (0 .. 8).collect { :n |
	PulseDivider(x, 8, 7 - n)
};
var d = 1.5 + x.ExpRange(0.01, 1);
var e = { :c |
	Perc(t, 0.004, d, c)
};
var c = [0 3 7 -2];
var f = Demand(
	t,
	0,
	[
		Drand(inf, c),
		Dxrand(inf, 48 + (0 .. 2).collect { :o |
			o * 12 + c
		}.concatenation)
	].Sum
).MidiCps;
var o = SinOsc(
	(BrownNoise() * 0.015 + 1) * f,
	(BrownNoise() / 4 * SinOsc([3.25 0.5 2] * f, 0) * e(-28)).Sum
) * e(-6) * (SinOsc(d + 2, 0) * 0.25 + 0.5);
Splay(o, 3 / 4)

(* Phase Modulation Washer ; https://github.com/lukiss/Losers-Union-SC-Research *)
var p = (1, 3 .. 64);
var n = 110;
var f = p / pi * p.degreesToRadians * n;
Splay(
	PmOsc(
		f,
		f * 2,
		SinOsc(f / n, 0) * SinOsc(n / f, 0) * 2,
		SinOsc(3 / p, 0) * pi
	) * SinOsc(1 / p, 0),
	SinOsc(SinOsc(0.1 / n, 0) * 8, 0) / SinOsc(1 / n, 0) / 2
) / 3

(* Something awfully old ; https://github.com/lukiss/Losers-Union-SC-Research ; requires=kr *)
var a = LocalBuf(1, 8 * 2048);
var f = {
	LfdNoise3(0.001 ! 7).Tan.Abs.kr
};
Splay(
	LeakDc(
		CombC(
			Warp1(
				1,
				a,
				f(),
				f() * 0.03,
				4.arithmeticSeries(1 / 7, 1 / 7) * 0.12,
				-1,
				4,
				0,
				4
			),
			0.4,
			0.4,
			2
		).SoftClip,
		0.995
	)
) <! RecordBuf(a, 0, 1, 0, 1, 0, 1, 0, LfdNoise3(500) + (PinkNoise() * 8))

(* Drone for the Evening ; https://github.com/lukiss/Losers-Union-SC-Research *)
var p = (1, 3 .. 64);
var f = (LfdNoise1(8) / 16 + 33).MidiCps;
var c = p ^ (p / p.sum).ArcTan * f * p;
Splay(
	SinOsc(
		c,
		SinOsc(c * 3, 0) * SinOsc((SinOsc(p / c, 0) * 8 + 8) / c, 0) * pi
	) * SinOsc(p / c, 0),
	SinOsc(1 / 3, 0) * SinOsc(1 / 32, 0)
) / 3

(* Tw 14 Nov 2022 Ballad ; https://github.com/lukiss/Losers-Union-SC-Research *)
var d = Dseq(inf, [1, 3 .. 21]);
var f = Ddup(
	LfPar(0.05, 0) * d / [3 7 5 1],
	Dseq(inf, [9 .. 42].degreeToKey([0 1 3 5 7 8 10], 12)).MidiCps
);
var n = 8;
LeakDc(
	GrainFm(
		n,
		TDmdFor(1 / d, 0, 1),
		2 + LfPar(0.3, 0) / d * 2,
		[2 3],
		f,
		f % d / 12,
		LfPar(440, 0) / 2,
		-1, (* Cannot be LocalBuf, Env.perc.discretize *)
		512
	).Splay * 0.23,
	0.995
).Sum

(* 12 May 2020 ; https://github.com/lukiss/Losers-Union-SC-Research *)
var l = { :freq :mul |
	(LfdNoise3(freq) * mul).Abs
};
var i = 1 / 16;
var t = Impulse(l(pi, pi), 0);
CombC(
	{
		SinOsc(
			Demand(
				t,
				0,
				Drand(inf, [9 .. 42].degreeToKey([0 2 4 7 9], pi * pi))
			).MidiCps,
			0
		) * (t.Lag3Ud(0, i) + (t.Lag3Ud(i + l(9, i * 4), l(pi, pi)) / pi))
	} !^ 5,
	i,
	i,
	pi
) / 3

(* 9 Jan 2019 ; https://github.com/lukiss/Losers-Union-SC-Research *)
var a = 40;
var f = { :b |
	LfNoise1(1).ExpRange(10, b)
};
Splay(
	LeakDc(
		Formant(
			[
				({ a := a + 5 } ! 6).MidiCps,
				({ f(29).RoundTo(7) } ! 6).MidiRatio
			].product,
			{ f(1000) } ! 6,
			{ f(500) } ! 6
		),
		0.995
	)
) / 5

(* 3 Nov 2018 ; https://github.com/lukiss/Losers-Union-SC-Research ; requires=kr *)
var a = 9;
var f = { :b |
	LfNoise2(b.Sign).ExpRange(9, b)
};
Splay(
	LeakDc(
		Formant(
			[
				({ a := a + 7 } ! 6).MidiCps,
				({ f(99).RoundTo(12) } ! 6).MidiRatio
			].product,
			{ f(2200) } ! 6,
			{ f(300) } ! 6
		) * SinOscFb(
			f(0.05),
			{ f(0.5) } ! 6
		).kr,
		0.995
	)
) / 13

(* Drone 22 Maj 2021 ; https://github.com/lukiss/Losers-Union-SC-Research *)
var n = 4;
var l = { :mul |
	(LfdNoise3(0.05 ! n) * mul).Abs
};
var d = {
	{
		Demand(
			Impulse(1 / (30 .. 53).atRandom, 0),
			0,
			Dxrand(inf, (7 .. 53).degreeToKey([0 1 4 5 7 9 10], 12).MidiCps)
		)
	} ! n
};
Splay(
	Hpf(
		CombC(
			PmOsc(
				d(),
				d(),
				l(2),
				l(0.5)
			) * (l(0.25) + 1 / 4),
			1 / 4,
			1 / 4 - l(1 / 16),
			7
		),
		110
	) * -12.DbAmp
)

(* 30 Apr. 2020 ; https://github.com/lukiss/Losers-Union-SC-Research *)
var t = {
	TDmdFor(Drand(inf, (1 .. 8) / 16), 0, 1)
};
var r = { :lo :hi |
	TRand(lo, hi, t())
};
Normalizer(
	CompanderD(
		LeakDc(
			LorenzL(
				22050,
				r(19, 11),
				r(1, 18),
				r(0.1, 5),
				r(0.01, 0.06 ! 2),
				0.1,
				0,
				0
			),
			0.995
		),
		0.8,
		r(0.01, 0.5),
		r(0.01, 0.5),
		0.01,
		0.075
	),
	-2.DbAmp,
	0.02
).Fold2(0.8)

(* 22 Nov. 2021 ; https://github.com/lukiss/Losers-Union-SC-Research *)
var k = 1 / 100000;
var n = {
	1 + ((k * (1 .. 7))).scramble
};
Splay(
	VarSaw(
		33 * n(),
		0,
		LfSaw(1 / 3 * n(), 0) * LfSaw(n() / 3, 0)
	) * (LfSaw(5 / 3, 0) * (LfSaw(7 / 3 * n().Neg, 0).Tan)).Abs
).Tanh

(* 12 Juli 2019 ; https://github.com/lukiss/Losers-Union-SC-Research ; rd edit *)
var f = { :freq :mul |
	var z = LfdNoise3(freq) * mul;
	z.Ring1(z % 0.01).Hypot(z)
};
var o = Formant(
	f(f(3, 13), 220),
	f(f(2, 10), f(f(3, 13), 2320)),
	f(f(3, 5), 2500)
);
var a = Excess(
	f(f(2, 12), 1),
	f(f(1, 14), f(4.4, 0.5) + 0.3)
);
(o * a).EqPan2(0).SoftClip

(* 12 Juli 2019 ; https://github.com/lukiss/Losers-Union-SC-Research ; rd edit *)
var k = 8;
var f = { :freq :mul |
	var z = LfdNoise3(freq) * mul;
	z.Ring1(z % 0.01).Hypot(z)
};
var o = Formant(
	f(f(3, 13), 220),
	f(f(2, 10), f(f(3, 13), 2320)),
	f(f(3, 5), 2500)
);
var a = {
	Excess(
		f(f(2 / k, 12), 1),
		f(f(1, 14), f(4.4, 0.5) + 0.3)
	)
} ! k;
(o * a).Splay.SoftClip / k.sqrt

(* 5 Sep. 2020 ; https://github.com/lukiss/Losers-Union-SC-Research ; requires=kr *)
var l = {
	LfdNoise3(1).Abs.kr
};
var r = { :freq :mul |
	(Gendy3(6, 6, l(), l(), freq, l(), l(), 5, l() * 5) * mul).Abs.kr
};
-12.DbAmp * Splay(
	(1 .. 6).collect { :n |
		CombC(
			LeakDc(
				Fm7Matrix(
					{
						[
							Rand(0, 80).MidiCps,
							0,
							Blip(r(l(), 24), r(l(), 228)).Abs.kr
						]
					} ! 6,
					r(0.02, 0.5) ! 6 ! 6
				)[n],
				0.995
			),
			0.5,
			0.05 + r(0.04, l()),
			1
		)
	}
)

(* Ambient Ligeti inspired soundscape ; https://github.com/lukiss/Losers-Union-SC-Research *)
var r = { :n |
	{ LfdNoise3(1 / 86).Abs } ! n
};
Splay(
	LeakDc(
		FreeVerb(
			Warp1(
				1,
				SfAcquireMono("floating_1"),
				r(9),
				[-36 -9 -14 0 -19 -5 3 -2 -24 -7].MidiRatio,
				8 * r(9) + 8 / 86,
				-1,
				12,
				1 / 4 * r(9),
				4
			) * r(9),
			r(9),
			0.5 + r(9),
			r(9)
		),
		0.995
	),
	2 / 4
).Tanh * Line(0, 1, 12)

(* 27 Jan 2023 ; https://github.com/lukiss/Losers-Union-SC-Research *)
var d = Dust(1);
var m = {
	TRand(0.5, 2.5, d).Lag3(1 / 16)
};
var c = (0 .. 9).collect { :n |
	m() ^ n
};
var n = c / c.Sum;
var e = TChoose(
	Dust(1 + m()),
	[
		c.Sum,
		n,
		c.RoundTo(32),
		n.Atan2(m()),
		c.Sin,
		c.Tan,
		n.Hypot(c)
	]
);
var a = ((LfSaw(c.Tan / m(), 0).Abs ^ e.Max(0)).Log10.Sin.Abs ^ 10).Tanh;
Splay(
	SinOsc(c.Log10 * c, 0) * a,
	SinOsc(m() * m(), 0) * 0.5
) / 23

(* 27 Oktober 2022 ; https://github.com/lukiss/Losers-Union-SC-Research *)
var a = [41 73 123 196];
var f = SinOsc(440, 0);
(0 .. 8).do { :i |
	a := a * 5.MidiRatio;
	f := SinOsc(f * a.rotated(i + 3), 0) * SinOsc(f, 0).Tanh
};
Splay(
	LeakDc(f.Tanh, 0.995),
	SinOsc(f.Sum.Abs, 0)
)

(* 23 Mars ; https://github.com/lukiss/Losers-Union-SC-Research *)
var d = { :n |
	{
		Gendy3(1, 1, 1, 1, LfNoise1(2) + 1, 0.5, 0.5, 12, 12).Abs.RoundTo(1 / 16).kr
	} ! n
};
var t = Dust(d(5) / 2).kr;
Splay(
	FreeVerb(
		LeakDc(
			Ifft(
				PvBinScramble(
					Fft(
						{ LocalBuf(1, 4096) } ! 2,
						StandardL(d(2) * 5000, 1, 0.5, 0),
						0.5,
						0,
						1,
						0
					),
					d(1),
					d(1),
					t
				),
				0,
				0
			) * t.Lag3Ud(0.01, d(5)),
			0.995
		),
		d(1) / 3,
		d(1),
		d(1) / 2
	),
	0.5
)

(* 20 jan. 2023 ; https://github.com/lukiss/Losers-Union-SC-Research *)
var l = { :lo :hi |
	TChoose(
		Dust(3 ! 2),
		CuspN(1.1 ^ (0 .. 5), 1, 1.9, 0).Tanh
	).ExpRange(lo, hi)
};
var a = { l(0.001, 1) } ! 6;
var f = l(12, 999);
var e = Env(a, a / a.Sum / f, a.Neg * 9, nil, nil, 0).circle(0, 'lin').asEnvGen(1);
var g = Perc(e > 0.1, 0.005, 0.9 / f, -4);
var p = (1 - g).LinLin(0, 1, -1, 1);
(XFade2(e, (e * f).Sin, p, 1) + g).Tanh.Splay

(* Tennis ; https://github.com/lukiss/Losers-Union-SC-Research *)
var nf = 48000; (* sample rate *)
var b = 1000; (* LocalBuf(1, nf); *)
var n = LeakDc(
	StandardL(22050, LfNoise1(1).Range(0.72, 1.40), 0.5, 0),
	0.995
);
var z = Sweep(0, 1 - n.ExpRange(0.01, 1)) % 1 * nf;
var t = (n.Lag3(0.1) > 0);
var p = PlayBuf(
	1,
	b,
	(1 - n.RoundTo(1 / 16)) / TRand(1, z / 100, t).Lag3(0.001),
	t,
	z,
	1,
	0
);
EqPan2(
	LeakDc(
		MoogFf(
			p,
			p.ExpRange(0.01, 1) * 11025,
			2,
			0
		),
		0.995
	).SoftClip,
	TRand(-1, 1, t)
) <! RecordBuf(b, z, t, 1 - t, t, 1, t, 0, n)

(* Cheap Singing Synth ; simpler (rd) ; https://github.com/lukiss/Losers-Union-SC-Research *)
var tab = [
	600 1040 2250 2450 2750; 60 70 110 120 130; 0 -7 -9 -9 -20;;
	400 1620 2400 2800 3100; 40 80 100 120 120; 0 -12 -9 -12 -18;;
	250 1750 2600 3050 3340; 60 90 100 120 120; 0 -30 -16 -22 -28;;
	400 750 2400 2600 2900; 40 80 100 120 120; 0 -11 -21 -20 -40;;
	350 600 2400 2675 2950; 40 80 100 120 120; 0 -20 -32 -28 -36
];
var x = LorenzL(24, 10, 28, 2.667, 0.05, 0.1, 0, 0).Sin.Range(1, 4) / Diwhite(inf, 2, 10);
var dur = Duty(x, 0, 0, x);
var trg = TDuty(dur, 0, 0, 1, 0);
var mel = (26 .. 42).degreeToKey([0 2 4 5 7 9 11], 12);
var mnn = Demand(
	trg,
	0,
	Dseq(inf, mel.scramble)
);
var env = Adsr(
	Trig(trg, dur * LfNoise2(4).Range(0.5, 1)),
	dur / 48,
	1 / 4,
	1,
	dur / 2,
	-4
);
var voc = SelectXFocus(
	mnn.LinLin(mel.first, mel.last, 0, tab.size),
	tab,
	0.9,
	true
);
var atk = PinkNoise() * (1 - env);
var sus = LfNoise2(voc.second) * ((env - voc.third.DbAmp) / 2) + voc.first;
Splay(
	Bpf(
		SyncSaw(
			LfNoise2(voc.second) * mnn / (voc.first) + mnn.MidiCps,
			atk + sus
		),
		env - (Perc(trg, 0, 1 / (2 ^ (0 .. 5)) / 8, -4) * 0.5) * voc.first + 30,
		voc.second / voc.first
	).SoftClip * voc.third.DbAmp * env
)
