(* Dseq ; shared dseq, different patterns *)
var a = Dseq(inf, [1, 3, 2, 7, 8]);
var t = Impulse(5, 0);
var f = Demand(t, 0, [a, a]) * 30 + 340;
SinOsc(f, 0) * 0.1

(* Dseq ; distinct dseq, equal patterns *)
var a = Dseq(inf, [1, 3, 2, 7, 8]);
var b = Dseq(inf, [1, 3, 2, 7, 8]);
var t = Impulse(5, 0);
var f = Demand(t, 0, [a, b]) * 30 + 340;
SinOsc(f, 0) * 0.1

(* ---- Dseq ; shared dseq, different patterns ; requires=keywords *)
var a = Dseq(repeats: inf, list: [1, 3, 2, 7, 8]);
var t = Impulse(freq: 5, phase: 0);
var f = Demand(trig: t, reset: 0, demandUGens: [a, a]) * 30 + 340;
SinOsc(freq: f, phase: 0) * 0.1

(* Dseq ; distinct dseq, equal patterns ; requires=keywords *)
var a = Dseq(repeats: inf, list: [1, 3, 2, 7, 8]);
var b = Dseq(repeats: inf, list: [1, 3, 2, 7, 8]);
var t = Impulse(freq: 5, phase: 0);
var f = Demand(trig: t, reset: 0, demandUGens: [a, b]) * 30 + 340;
SinOsc(freq: f, phase: 0) * 0.1
