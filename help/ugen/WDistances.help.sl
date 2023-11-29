(* WDistances *)
var x = MouseX(-1, 1, 0, 0.2);
var y = MouseY(1, -1, 0, 0.2);
var c = [-45, 45, 135, -135].collect { :each |
	SphericalCoordinate(1, pi / 2, each.degreesToRadians).asCartesianCoordinate.asArray
}.concatenation;
var g = 0.5 / WDistances(1, x, y, 0, c);
var z = Hpf(PinkNoise(), ([1 .. 4] * 12 + 36).MidiCps);
(z * g).Splay

