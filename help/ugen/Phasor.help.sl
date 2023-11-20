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

(* Phasor ; https://scsynth.org/t/8528/2 *)
var raisedCos = { :phase :index |
	var cosine = (phase * 2.pi).Cos;
	(index.Abs * (cosine - 1)).Exp
};
var rate = 110;
var modRatio = 2.5;
var index = SinOsc(0.3, 0).LinLin(-1, 1, 0, 30);
var modPhase = Phasor(Dc(0), rate * modRatio * SampleDur(), 0, 1, 0);
var mod = (modPhase * 2.pi).Sin;
var raisedCosWindow = raisedCos(modPhase, index);
var carPhase = Phasor(Dc(0), rate * SampleDur(), 0, 1, 0);
var car = (carPhase * 2.pi + (mod * index)).Sin;
var sig = car * raisedCosWindow;
LeakDc(sig, 0.995) * 0.1
