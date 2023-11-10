(* Choose ; select input at trigger *)
var x = MouseX(1, 1000, 1, 0.1);
var t = Dust(x);
var f = IRand(t, 48, 60).MidiCps;
var o = Choose(t, [SinOsc(f, 0), Saw(f * 2), Pulse(f * 0.5, 0.1)]);
o * 0.1

(* Choose ; sequences of different lengths *)
var a = [[1, 2, 3], [1, 2, 3, 4, 5], [1, 2, 3, 4, 5, 6, 7]];
var t1 = Impulse(1 / 5, 0);
var t2 = Impulse(5, 0);
var m = Choose(t1, a.collect { :x | Demand(t2, 0, Dseq(inf, x)) });
var c = SinOsc(1200, 0) * Decay(t1, 1) * 0.1;
SinOsc(m * 110, 0) * 0.1 + c
