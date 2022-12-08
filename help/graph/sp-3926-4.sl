;; https://scsynth.org/t/phaseshaping-osc-algorithms/3926/4 ; sp ; event control ; https://core.ac.uk/download/pdf/297014559.pdf
var voiceFunc =  { :e |
	var width = Clip(e.x, 0.05, 0.95);
	var freq = e.y * 800 + 200;
	var saw = LinLin(LfSaw(freq / 2, 1), -1, 1, width.negated, 1 - width);
	var trig = ToggleFf(Trig(saw.negated, 2 / SampleRate()) + Trig(saw, 2 / SampleRate()));
	var a = LinLin(saw, width.negated, 0, 0, pi).sin;
	var b = LinLin(saw,  0, 1 - width, 0, 2 * pi).sin;
	Select2(trig, a, b) ! 2 * e.w * e.z
};
Voicer(16, voiceFunc).sum * 0.2
