;; DmdFor ; c.f. equivalent DmdOn graph ; (Duty)
var t = Dust(9);
var n = Lseq(inf, [0, 2, 4, 5, 7, 9, 11, 12]) + (TRand(3, 6, t).RoundTo(1) * 12);
var f = DmdFor(0.1, 0, n.MidiCps);
var o = SinOsc([f, f + 0.7], 0);
o.Cubed.Cubed * 0.1

;; DmdFor
var f = { :tr :trs |
	var n = Lseq(inf, [7, 0, 3, 3, 3, Lrand(1, [5, 10, 12]), 7]) + 48;
	var a = Decay2(tr, 0.01, TRand(0.15, 0.35, tr) * MouseY(0.5, 2, 0, 0.2)) * TRand(0.01, 0.2, tr);
	var f = DmdOn(tr, 0, (n + trs).MidiCps);
	var o = SinOsc([f, f + TRand(tr, 0.7, 1.3)], 0);
	o.Cubed.Cubed * a
};
var d = Lseq(inf, [8, 3, 3, 2, 8, 4, 4]) / MouseX(9, 27, 1, 0.2);
var t1 = TDmdFor(d, 0, 1);
var t2 = PulseDivider(t1, 2, 0);
var t4 = PulseDivider(t1, 4, 0);
var t24 = PulseDivider(t1, 24, 0);
var b = DmdOn(t24, 0, Lseq(inf, [0, -1, -2]));
f(t1, b + 0) + f(t2, b + 12) +  f(t4, b - 5)
