;; LPF ; https://scsynth.org/t/5208/7 ; at very low frequencies LPF acts strangely
var impulseFreq = 3;
var lfo = LFPulse(impulseFreq, 0, 0.5);
var freq = LinExp(MouseX(0, 1, 0, 0.2), 0, 1, impulseFreq, 20000);
SinOsc(200 + (LPF(lfo, freq) * 200), 0) * 0.1

;; LPF
LPF(WhiteNoise(), 500) * 0.1

;; LPF ; modulate frequency
LPF(WhiteNoise(), SinOsc([0.1, 0.13], pi) * [2300, 3000] + 3500) * 0.1
