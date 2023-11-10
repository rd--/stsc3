(* Rand *)
var tr = Impulse(7, 0) * SinOsc(331, 0);
var env = Decay2(tr, { Rand(tr, 0.01, 0.05) } ! 2, { Rand(tr, 0.05, 0.15) } ! 2);
Rlpf(LfSaw(Rand(tr, 321, 333), 0), (LfNoise1(2) * 4 + 100).MidiCps, 1) * env

(* Rand *)
var tr = Dust(5);
var f = Rand(tr, 200, 500);
SinOsc(f, 0) * 0.1

(* Rand ; array input *)
var tr = Dust([5, 12]);
var f = Rand(tr, [200, 1600], [500, 3000]);
SinOsc(f, 0) * 0.1
