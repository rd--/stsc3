;; 20060916 ; rd
var mkRead = { :l :t |
	BufRd(1, l.asLocalBuf, TRand(0, 6, t), 0, 1)
};
var mkNode = { :n |
	var t = Dust(1.6);
	var f = mkRead([60, 62, 64, 65, 67, 69], t).MidiCps;
	var p = mkRead([-1, -0.5, 0, 0.25, 0.75, 1], t);
	var a = mkRead([0.01, 0.05, 0.1, 0.15, 0.25, 0.35], t);
	EqPan2(SinOsc(f, 0) * a, p)
};
(1 .. 4).collect(mkNode).sum * 0.25
