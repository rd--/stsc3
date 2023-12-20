(* https://scsynth.org/t/6452 ; requires=SinGrain *)
var tr = Impulse(100 * Line(0.1, 10, 15), 0);
SinGrain(tr, 0.05, TRand(322, 3222, tr)) * 0.1

(* https://scsynth.org/t/6452 ; rd edit *)
var k = 8;
var tr = Impulse(100 * Line(0.1, 10, 15), 0);
GrainSin(k, tr, Line(1, 0.25, 15), TRand(322, 3222, tr), TRand(-1, 1, tr), -1, 2048).Splay / 99

(* https://scsynth.org/t/6452 ; rd edit *)
var k = 8;
var tr = Impulse(Line(1, 50, 30), 0);
GrainFm(
	8,
	tr,
	TRand(1, 10, tr),
	TRand(322, 3222, tr),
	TRand(322, 3222, tr),
	1,
	TRand(-1, 1, tr),
	-1,
	2048)
.Splay / 99

(* https://scsynth.org/t/6452 ; rd edit *)
var tr = Impulse(1 * Line(0.1, 10, 90), 0);
var k = 8;
var carFreq = TRand(322, 3222, tr);
var modFreq = TRand(322, 3222, tr);
var pan = TRand(-1, 1, tr);
GrainFm(k, tr, 0.1, carFreq, modFreq, 1, pan, -1, 128).Splay / 99

(* https://scsynth.org/t/6452 *)
var o1 = Saw([2000 200 1000 4000]) * Perc(Impulse(5 * [2 0.5 0.25 0.125], 0), 0.01, 1, -4);
var o2 = SinOsc(128, 0) * Perc(Impulse(5 * 0.125, 0), 0.01, 1, -4);
(o1.Sum + o2) / 5

(* https://scsynth.org/t/6452 *)
var f = { :p |
	var f1 = p.first, f2 = p.second, l1 = 0.125, l2 = 8;
	var o1 = { SinOsc(Rand(f1, f2) * Line(l1, l2, 60), 0) } !+ 128;
	var o2 = { SinOsc(Rand(f1, f2) * Line(l2, l1, 60), 0) } !+ 128;
	(o1 + o2) / 128
};
f([20 20000; 20 200; 200 2000; 2000 20000].atRandom)

(* https://scsynth.org/t/6452 *)
var f = { :p |
	var f1 = p.first, f2 = p.second, l1 = 0.5, l2 = 2;
	var tr = Impulse(10, 0);
	var o1 = { SinOsc(TRand(f1, f2, tr) * Line(l1, l2, 60), 0) } !+ 128;
	var o2 = { SinOsc(TRand(f1, f2, tr) * Line(l1, l2, 60), 0) } !+ 128;
	(o1 + o2) / 128
};
f([20 20000; 20 2000; 200 2000; 20 200].atRandom)
