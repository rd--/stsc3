(* Blip *)
var ln = Line(1, 20, 60);
var gate = LfPulse(ln, 0, 0.23) > 0;
Blip(TRand(100, 1000, gate), TRand(1, 10, gate)) * Asr(gate, 0.01, 1 / ln, -4) * 0.25

(* Blip *)
var ln = Line(1, 20, 60);
var tr = Impulse(ln, 0);
Blip(TRand(100, 1000, tr), TRand(1, 10, tr)) * Perc(tr, 0.01, 1 / ln, -4) * 0.25

(* Blip *)
Blip(
	MouseX([3, 12], 1, 0, 0.2),
	MouseY([100, 1000], 1, 0, 0.2)
) * 0.1

(* Blip ; requires=Voicer (event control) *)
Voicer(16) { :e |
	var o = Blip((e.x * 13 + 48).MidiCps, e.y * 19 + 1);
	EqPan2(o, e.i * 2 - 1) * e.z * e.w * 0.5
}.Mix

(* ---- Blip ; requires=keywords *)
Blip(
	freq: MouseX([3, 12], 1, 0, 0.2),
	numharm: MouseY([100, 1000], 1, 0, 0.2)
) * 0.1
