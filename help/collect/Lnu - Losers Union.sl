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
