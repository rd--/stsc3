;; TRand
var t = Impulse(7, 0) * SinOsc(331, 0);
var e = Decay2(t, { TRand(0.01, 0.05, t) } ! 2, { TRand(0.05, 0.15, t) } ! 2);
RLPF(LFSaw(TRand(321, 333, t), 0), (LFNoise1(2) * 4 + 100).midiCps, 1) * e

;; TRand
var t = Dust(5);
var f = TRand(200, 500, t);
SinOsc(f, 0) * 0.1

;; TRand ; mce
var t = Dust([5, 12]);
var f = TRand([200, 1600], [500, 3000], t);
SinOsc(f, 0) * 0.1
