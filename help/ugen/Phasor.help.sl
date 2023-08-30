(* Phasor ; phase modulation ; mouse control *)
var carFreq = 440;
var modFreq = MouseX(1, 1000, 1, 0.2);
var modIndex = MouseY(0, 100, 0, 0.2);
var modDev = modFreq * modIndex;
var rateConversion = 2 * pi / SampleRate();
var phase = Phasor(0, carFreq * rateConversion, 0, 2 * pi, 0);
var phaseOffset = SinOsc(modFreq, 0) * modDev * rateConversion;
[SinOsc(0, phase + phaseOffset), SinOsc(carFreq, 0)] * 0.1

(* SinOsc ; phase input ; constant frequency *)
var freq = 440;
var rate = 2 * pi * freq / SampleRate();
var phase = Phasor(1, rate, 0, 2 * pi, 0);
SinOsc(0, phase) * 0.1
