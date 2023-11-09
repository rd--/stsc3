(* 20061008 ; rd ; requires=kr *)
var t = Dust(9).kr;
var u = PulseDivider(t, 9, 0);
var k = DegreeToKey(
	[0 2 3.2 5 7 9 10].asLocalBuf,
	TrIRand(t, MouseX(15, 0, 0, 0.1), MouseY(15, 27, 0, 0.1)),
	12
);
var m = LfNoise1([3, 3.05]) * 0.04 + TrChoose(t, [36, 48, 60, 72]) + k;
var o = SinOsc(m.MidiCps, 0) * Decay2(t, 0.005, TrRand(t, 0.02, 0.15)) * 0.2;
o * 0.5 + AllpassC(o, 0.15, TrRand(u, 0.0075, 0.125), TrRand(u, 0.05, 0.15))
