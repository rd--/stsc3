(* deepsea (jrhb) *)
var amp = 0.1;
var variation = 0.9;
var tr = Dust(0.25);
var n = TRand(7, 46, tr);
var dt = 25.0 + TRand(-1.7, 1.7, tr) + LfNoise2(2) * variation * 0.001;
var freq = 901 + TRand(0, 65, tr);
var t = Impulse(dt.Recip, 0) * 100;
var count = PulseCount(t, 0);
var u = Bpf(
	Bpf(count < n * t, freq, 0.1),
	freq * (count % LinLin(LfNoise1(1), -1, 1, 2, 20) + 1),
	0.2
);
var f1 = Bpf(
	EqPan(u, TRand(-1, 1, tr)) * amp * 50,
	700,
	0.1
);
var f2 = CombL(
	Lpf(f1 * LfNoise1(0.1).Max(0), 800),
	0.5,
	0.5,
	1
) + f1.reversed;
var f3 = f2;
5.timesRepeat {
	f3 := AllpassN(f3, 0.18, { 0.06.Rand } ! 2 + 0.06, 8)
};
Lpf(f2 + f3, 400) + (LfNoise1(0.2) * f2)
