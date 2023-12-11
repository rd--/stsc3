(* Rand *)
var tr = Impulse(7, 0) * SinOsc(331, 0);
var env = Decay2(tr, { TRand(0.01, 0.05, tr) } ! 2, { TRand(0.05, 0.15, tr) } ! 2);
Rlpf(LfSaw(TRand(321, 333, tr), 0), (LfNoise1(2) * 4 + 100).MidiCps, 1) * env

(* Rand *)
var tr = Dust(5);
var f = TRand(200, 500, tr);
SinOsc(f, 0) * 0.1

(* TRand ; array input *)
var tr = Dust([5, 12]);
var f = TRand([200, 1600], [500, 3000], tr);
SinOsc(f.Lag(0.07), 0) * TRand(0.01, 0.15, tr).Lag2(0.02)
