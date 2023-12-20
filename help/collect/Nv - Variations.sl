(* nv 52 https://swiki.hfbk-hamburg.de/MusicTechnology/899 ; requires=crossedMultiply (⊗) *)
var x = [4 4.5; 2 3 5 6];
var z = x.crossedMultiply(x.crossedMultiply(x).allTuples);
var y = (z * 4).concatenation.clump(2) ++ [0];
var f = DmdFor(1 / 5, 0, Dseq(1, y));
GVerb(
	VarSaw(f, 0, 0.9) * LfPulse(5, 0, 0.5).Lag(0.01),
	99, 5, 0.5, 0.5, 15, 1, 0.7, 0.5, 300
).transposed.Mix / 15

(* ---- https://swiki.hfbk-hamburg.de/MusicTechnology/899 (nv) L43 ; keyword parameters ; ? *)
(0 .. 9).collect { :i |
	var x = Impulse(freq: 1, phase: i / 10) + Impulse(freq: 0, phase: 0);
	var o = LfSaw(freq: [102, 101], iphase: 0);
	var d = 1 / Latch(in: 1.015 ^ Sweep(trig: Impulse(freq: 0, phase: 0), rate: 1) * 64 % 1 + 1 * 200, trig: x);
	Pluck(in: o, trig: x, maxdelaytime: 1, delaytime: d, decaytime: 4, coef: 0.2)
}.mean * 0.1
