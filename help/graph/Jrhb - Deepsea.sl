(* deepsea (jrhb) *)
var amp = 0.1;
var variation = 0.9;
var tr = Dust(0.25);
var n = Rand(tr, 7, 46);
var dt = 25.0 + Rand(tr, -1.7, 1.7) + LfNoise2(2) * variation * 0.001;
var freq = 901 + Rand(tr, 0, 65);
var t = Impulse(dt.Recip, 0) * 100;
var count = PulseCount(t, 0);
var u = Bpf(
	Bpf(count < n * t, freq, 0.1),
	freq * (count % LinLin(LfNoise1(1), -1, 1, 2, 20) + 1),
	0.2
);
var f1 = Bpf(
	Pan2(u, Rand(tr, -1, 1), amp * 10) * 5,
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
