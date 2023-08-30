(* impulse sequencer (jmcc) Sc2 *)
var t = Impulse(8, 0);
var seq = { :s | t * Demand(t, 0, Dseq(inf, s)) };
var cSeq = seq([1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0]);
var dSeq = seq([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0]);
var nSeq = seq([1.0, 0.1, 0.1, 1.0, 0.1, 1.0, 0.1, 0.1]);
var bSeq = seq([1, 0, 0.2, 0, 0.2, 0, 0.2, 0]);
var c = Decay2(cSeq, 0.001, 0.3) * SinOsc(1700, 0) * 0.1;
var d = Decay2(dSeq, 0.001, 0.3) * SinOsc(2400, 0) * 0.04;
var n = Decay2(nSeq, 0.001, 0.25) * BrownNoise() * 0.1;
var b = Decay2(bSeq, 0.001, 0.5) * SinOsc(100, 0) * 0.2;
c + d + n + b
