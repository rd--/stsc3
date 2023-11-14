(* 20061008 ; rd *)
var t = Dust(9);
var u = PulseDivider(t, 9, 0);
var k = DegreeToKey(
	[0 2 3.2 5 7 9 10].asLocalBuf,
	IRand(t, MouseX(15, 0, 0, 0.1), MouseY(15, 27, 0, 0.1)),
	12
);
var m = LfNoise1([3, 3.05]) * 0.04 + Choose(t, [36 48 60 72]) + k;
var o = SinOsc(m.MidiCps, 0) * Decay2(t, 0.005, Rand(t, 0.05, 0.175)) * 0.2;
o * 0.5 + AllpassC(o, 0.15, Rand(u, 0.0075, 0.125), Rand(u, 0.05, 0.15))
