(* LocalOut ; resonator, must subtract blockSize for correct tuning *)
var p = LocalIn(1, 0);
var i = Impulse(1, 0);
var d = DelayC(i + (p * 0.995), 1, 440.reciprocal - ControlDur());
[p <! LocalOut(d), SinOsc(440, 0) * 0.05]

(* LocalOut ; ping pong *)
var n = Decay(Impulse(0.3, 0), 0.1) * PinkNoise() * 0.2;
var l = LocalIn(2, 0) + [n, 0];
var d = DelayN(l, 0.2, 0.2);
var o = LocalOut(d.reversed * 0.8);
d <! o
