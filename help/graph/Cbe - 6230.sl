(* https://scsynth.org/t/6230 (cbe) *)
var freq = XLine(800, 1200, 100);
var osc = SinOsc(freq, 0);
var step1 = LfPulse(freq / 2, 0, 0.5) * 0.5 + 0.5;
var step2 = Pulse(freq / 2, 0.5) * 0.5 + 0.5;
[osc * step1, osc * step2] * 0.1
