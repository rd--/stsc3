(* 20060916 ; rd *)
var f = { :n |
	var t = Dust(1.6);
	var r = { :l :t |
		BufRd(1, l.asLocalBuf, TRand(0, 6, t), 0, 1).Lag(
			LfNoise2(0.3).Range(0.01, 0.1)
		)
	};
	EqPan2(
		SinOsc(r([60, 62, 64, 65, 67, 69], t).MidiCps, 0),
		r([-1, -0.5, 0, 0.25, 0.75, 1], t)
	) * r([0.01, 0.05, 0.1, 0.15, 0.25, 0.35], t)
};
1...4.collect(f).sum * 0.25
