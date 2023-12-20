(* https://scsynth.org/t/phaseshaping-osc-algorithms/3926/4 ; sp ; event control ; https://core.ac.uk/download/pdf/297014559.pdf *)
Voicer(16) { :e |
	var width = Clip(e.x, 0.05, 0.95);
	var freq = e.y * 800 + 200;
	var saw = LinLin(LfSaw(freq / 2, 1), -1, 1, width.Neg, 1 - width);
	var trig = ToggleFf(Trig(saw.Neg, 2 / SampleRate()) + Trig(saw, 2 / SampleRate()));
	var a = LinLin(saw, width.Neg, 0, 0, pi).Sin;
	var b = LinLin(saw, 0, 1 - width, 0, 2 * pi).Sin;
	Select2(trig, a, b).EqPan2(0) * e.w * e.z
}.Mix * 0.2
