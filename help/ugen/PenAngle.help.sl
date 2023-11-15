(* PenAngle ; range is (0, 1) ; boundary sine tones give reference points *)
var referenceTones = SinOsc([220, 440], 0) * 0.025;
(1 .. 16).collect { :vc |
	SinOsc(PenAngle(vc) * 220 + 220, 0) * PenDown(vc) * 0.1
}.Splay2 + referenceTones

(* PenAngle ; as pan control *)
(1 .. 16).collect { :vc |
	EqPan2(
		Blip(PenX(vc) * 220 + 220, PenY(vc) * 4 + 1),
		PenAngle(vc) * 2 - 1
	) * PenZ(vc) * PenDown(vc)
}.Mix
