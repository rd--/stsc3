(* ---- sin ; https://scsynth.org/t/6256/7 ; requires=Bilin *)
var freq = 130;
var width = LinLin(LfTri(0.1, 0), -1, 1, 0.01, 0.99);
var phasor = Phasor(0, freq * SampleDur(), width.Neg, 1 - width, 0);
phasor = Bilin(phasor, 0, width.Neg, 1 - width, 0, -1, 1);
phasor = Wrap(phasor.Abs * 1.5, 0, 1);
(phasor * 2 * pi).Sin * 0.1

(* sin ; https://scsynth.org/t/6256/7 ; sinusoid with a variable-slope ramp phase ; requires=Bilin *)
var freq = 130;
var width = LinLin(LfTri(0.1, 0), -1, 1, 0.01, 0.99);
var phasor = Phasor(0, freq * SampleDur(), width.Neg, 1 - width, width.Neg);
phasor = Bilin(phasor, 0, width.Neg, 1 - width, 0, -0.5, 1);
phasor = Select2(phasor > 0, 0.5 + phasor, phasor);
(phasor * 2 * pi).Sin * 0.1
