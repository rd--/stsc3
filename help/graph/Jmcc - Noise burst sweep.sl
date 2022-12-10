;; noise burst sweep (jmcc) #6 ; mouse control
var n = { WhiteNoise() } ! 2;
var lfoRate = MouseX(10, 60, 1, 0.2);
var amp = LfSaw(lfoRate, -1).Max(0);
var cfreq = MouseY(400, 8000, 1, 0.2);
var freq = SinOsc(0.2, 0) * cfreq + (1.05 * cfreq);
Resonz(n * amp, freq, 0.1)

