(* https://glicol.org/demo#ontherun ; requires=VbJonVerb *)
var tr = Impulse(12, 0);
var seq = Demand(tr, 0, Dseq(inf, [40, 43, 45, 43, 50, 48, 50, 52]));
var osc = Pulse(seq.MidiCps, 0.5);
var env = Decay2(tr, 0.05, 0.25);
var flt = Rlpf(osc * env, SinOsc(0.2, 0) * 1200 + 1500, 1) * 0.1;
VbJonVerb(flt, 0.1, 0.3, 0.8, 0.15, 0.25) + flt

(* ---- ; alternate reverb ; GVerb(flt, 25, 0.75, 0.5, 0.5, 15, 1, 0.3, 0.3, 300) *)
