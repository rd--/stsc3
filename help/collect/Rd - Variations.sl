;; 20061008 ; s-chirp ; rd
var x = MouseX(15, 0, 0, 0.1);
var y = MouseY(15, 27, 0, 0.1);
var t = Dust(9).kr;
var b = TChoose(t, [36, 48, 60, 72]);
var n = LfNoise1([3,  3.05]) * 0.04;
var d = TiRand(x, y, t);
var e = Decay2(t, 0.005, TRand(0.02, 0.15, t));
var k = DegreeToKey([0, 2, 3.2, 5, 7, 9, 10].asLocalBuf, d, 12);
var f = (b + k + n).MidiCps;
var m = e * SinOsc(f, 0) * 0.2;
var u = PulseDivider(t, 9, 0);
var r0 = TRand(0.0075, 0.125, u);
var r1 = TRand(0.05, 0.15, u);
m * 0.5 + AllpassC(m, 0.15, r0, r1)
