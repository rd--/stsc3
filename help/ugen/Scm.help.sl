(* Scm *)
var tick = LfPulse(3.1, 0, 0.5);
var rotate = LfNoise0(0.3) * 3.5 + 3.5;
var pulses = Scm(0, 80, rotate, 100, 40, 100, 3);
var freqs = [60, 67, 72, 75, 79, 81, 84, 86].MidiCps;
var out = Ringz(pulses, freqs, 0.08);
Splay2(out)
