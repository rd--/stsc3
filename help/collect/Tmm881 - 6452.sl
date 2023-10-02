(* https://scsynth.org/t/6452 ; requires=SinGrain *)
var tr = Impulse(100 * Line(0.1, 10, 15, 2), 0);
Pan2(SinGrain(tr, 0.05, TRand(322, 3222, tr)) * 0.1, 0, 1)

(* https://scsynth.org/t/6452 *)
var tr = Impulse(100 * Line(0.1, 10, 15, 2), 0);
Pan2(GrainSin(2, tr, 1, TRand(322, 3222, tr), WhiteNoise() * 0.6, -1, 2048), 0, 1 / 64).transposed.sum

(* https://scsynth.org/t/6452 *)
var tr = Impulse(100 * Line(0.1, 10, 30, 2), 0);
Pan2(GrainFm(2, tr, 10, TRand(322, 3222, tr), TRand(322, 3222, tr), 1, WhiteNoise() * 0.6, -1, 128), 0, 1 / 64)

(* https://scsynth.org/t/6452 *)
var tr = Impulse(100 * Line(0.1, 10, 60, 2), 0);
Pan2(GrainFm(2, tr, 10, TRand(322, 3222, tr), TRand(322, 3222, Impulse(100, 0)), 1, WhiteNoise() * 0.6, -1, 128), 0, 1 / 64)

(* https://scsynth.org/t/6452 *)
var tr = Impulse(1 * Line(0.1, 10, 90, 2), 0);
Pan2(GrainFm(2, tr, 0.1, TRand(322, 3222, tr), TRand(322, 3222, tr), 1, WhiteNoise() * 0.6, -1, 128), 0, 1 / 64)

(* https://scsynth.org/t/6452 *)
var o1 = Saw([2000, 200, 1000, 4000]) * Perc(Impulse(5 * [2, 0.5, 0.25, 0.125], 0), 0.01, 1, -4);
var o2 = SinOsc(128, 0) * Perc(Impulse(5 * 0.125, 0), 0.01, 1, -4);
Pan2(o1.sum + o2, 0, 1 / 5)

(* https://scsynth.org/t/6452 *)
var f = { :p |
	var f1 = p.first, f2 = p.second, l1 = 0.125, l2 = 8;
	var o1 = { SinOsc(Rand(f1, f2) * Line(l1, l2, 60, 2), 0) } !+ 128;
	var o2 = { SinOsc(Rand(f1, f2) * Line(l2, l1, 60, 2), 0) } !+ 128;
	EqPan2(o1 + o2, 0) / 128
};
f([[20, 20000], [20, 200], [200, 2000], [2000, 20000]].atRandom)

(* https://scsynth.org/t/6452 *)
var f = { :p |
	var f1 = p.first, f2 = p.second, l1 = 0.5, l2 = 2;
	var o1 = { SinOsc(TRand(f1, f2, Impulse(10, 0)) * Line(l1, l2, 60, 2), 0) } !+ 128;
	var o2 = { SinOsc(TRand(f1, f2, Impulse(10, 0)) * Line(l1, l2, 60, 2), 0) } !+ 128;
	EqPan2(o1 + o2, 0) / 128
};
f([[20, 20000], [20, 2000], [200, 2000], [20, 200]].atRandom)
