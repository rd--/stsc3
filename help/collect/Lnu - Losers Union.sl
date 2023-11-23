(* Feedback loop ; https://github.com/lukiss/Losers-Union-SC-Research *)
var i = LocalIn(2, 0);
var o = Splay2(
	LeakDc(
		DelayC(
			i + ((1 + i) ^ 0.999),
			1,
			SinOsc(0.01 / (1 .. 8), 0).LinExp(-1, 1, 0.01, 1)
		),
		0.995
	).Tanh
);
(1 / 5 * Lpf(o, 5000)) <! LocalOut(o)

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
	m[3] * 2 / 3
)

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
)

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
)

(* Mistakes were made ; https://github.com/lukiss/Losers-Union-SC-Research *)
var p = 9 / 7 ^ (0 .. 16);
var c = { :freq | LinCongC(freq, 1.1, 0.13, 1, 0) };
var d = (c(c(p.arcTan) ^ 2 * 4) ^ 4 * 8).Abs;
var t = c(c(d / p.arcTan).RoundTo(1 / d) ^ 4 * d * 8).Sin;
var f = Rand(t, 0, 64).Ceiling.MidiCps;
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
).Tanh / 2

(* Grains, Daily ; https://github.com/lukiss/Losers-Union-SC-Research *)
var m = { :lo :hi |
	StandardL(LfdNoise1(1).ExpRange(0.01, 1) * 8 + 0.1, 2, 0.5, 0).ExpRange(lo, hi)
};
var f = {
	StandardL(m(1, 128), m(1, 3), 0.5, 0).ExpRange(40, 5000)
};
LeakDc(
	GrainFm(
		system.scSynth.mainOutputs,
		Impulse(f() / m(0.5, 8), 0),
		m(0.1, 4) / f(),
		f(),
		m(1 / 4, 4) * f(),
		m(1 / 4, 4),
		StandardL(f() / 2, 1, 0.5, 0) / 2,
		-1,
		1024
	),
	0.995
).SoftClip

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
var t = (0 .. 8).collect { :n | PulseDivider(x, 8, 7 - n) };
var d = 1.5 + x.ExpRange(0.01, 1);
var e = { :c | Perc(t, 0.004, d, c) };
var c = [0 3 7 -2];
var f = Demand(
	t,
	0,
	Drand(inf, c) + Dxrand(inf, 48 + (0 .. 2).collect { :o | o * 12 + c }.concatenation)
).MidiCps;
var o = SinOsc(
	(BrownNoise() * 0.015 + 1) * f,
	(BrownNoise() / 4 * SinOsc([3.25 0.5 2] * f, 0) * e(-28)).sum
) * e(-6) * (SinOsc(d + 2, 0) * 0.25 + 0.5);
Splay(o, 1 / 4)

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
)

(* Something awfully old ; https://github.com/lukiss/Losers-Union-SC-Research *)
var a = LocalBuf(1, 8 * 2048);
var f = { LfdNoise3(0.001 ! 7).Tan.Abs };
RecordBuf(LfdNoise3(500) + (PinkNoise() * 8), a, 0, 1, 0, 1, 0, 1, 0);
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
)

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
)
