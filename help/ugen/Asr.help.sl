(* Asr *)
var gate = LfPulse(0.5, 0, 0.1) > 0;
SinOsc(440, 0) * Asr(gate, 0.05, 0.7, 0) * 0.1
