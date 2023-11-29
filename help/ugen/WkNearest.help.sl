(* WkNearest *)
var x = MouseX(-1, 1, 0, 0.2);
var y = MouseY(-1, 1, 0, 0.2);
var c = [-45, 45, 135, -135].collect { :each |
	SphericalCoordinate(1, pi / 2, each.degreesToRadians).asCartesianCoordinate.asArray
}.concatenation;
var k = WkNearest(2, 1, x, y, 0, c);
var z = [SinOsc(333, 0), PinkNoise(), Saw(111) / 2, Pulse(555, 0.5) / 3] * 0.1;
Select(k, z)
