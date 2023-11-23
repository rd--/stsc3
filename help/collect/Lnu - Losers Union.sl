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
Splay2(
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
	m[3] / 2,
	1,
	0,
	true
)
