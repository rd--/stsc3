(* Event control ; Kyma Pen name notation *)
(1 .. 16).collect { :vc |
	var n = Lpf(WhiteNoise(), PenRadius(vc) + 1 * 6000) * PenZ(vc) * 2;
	var dlMax = 1 / 110;
	var dl = dlMax * (1 - PenX(vc).RoundTo(0.025) * 0.9);
	var p = Pluck(n, PenDown(vc), dlMax, dl, 10, PenY(vc) / 3);
	EqPan2(p, PenAngle(vc) * 2 - 1)
}.Mix
