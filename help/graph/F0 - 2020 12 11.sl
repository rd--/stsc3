(* f0 ; https://www.listarc.bham.ac.uk/lists/sc-users/msg69041.html ; requires=EnvLinen *)
var freq1 = 174.61, freq2 = 196.00, freq3 = 130.81, pan = 0.0, atk = 0.1, sus = 0.5, rls = 2.0, gate = 1, arpDur = 0.25;
var freq = [freq1, freq2, freq3];
var env = EnvLinen(atk, sus, rls, 1, [1, 1, 1]).asEnvGen(DelayN(gate, arpDur, [0, 0.5, 1] * arpDur).kr);
var sig = GVerb((SinOsc(freq, 0) * env).Sum, 150, 5, 0.5, 0.5, 15, 1, 0.7, 0.5, 300) * 0.1;
Pan2(sig, pan, 0.25).transposed.Sum <! DetectSilence(sig, 0.0001, 0.1, 2)
