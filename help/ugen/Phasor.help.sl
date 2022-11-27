;; Phasor ; phase modulation ; mouse control
var modfreq = MouseX(1, 1000, 1, 0.2);
var modindex = MouseY(0, 100, 0, 0.2);
var conversion= 2 * pi / SampleRate();
var phase = Phasor(0, 440 * conversion, 0, 2 * pi, 0);
var phaseOffset = modfreq * modindex * conversion * SinOsc(modfreq, 0);
SinOsc(0, phase + phaseOffset) * 0.2
