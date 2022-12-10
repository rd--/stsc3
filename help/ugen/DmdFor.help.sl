;; DmdFor ; c.f. equivalent DmdOn graph ; (Duty)
var t = Dust(9);
var n = Seq(inf, [0, 2, 4, 5, 7, 9, 11, 12]) + (TRand(3, 6, t).RoundTo(1) * 12);
var f = DmdFor(0.1, 0, n.MidiCps);
var o = SinOsc([f, f + 0.7], 0);
o.Cubed.Cubed * 0.1
