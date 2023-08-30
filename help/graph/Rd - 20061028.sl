(* 20061028 ; rd *)
var x = MouseX(20, 22000, 0, [0.005, 0.025]);
var y = MouseY(20, 22000, 0, [0.005, 0.075]);
{
	var a = SinOsc(x + LfNoise0([5, 9]), 0);
	var b = SinOsc(y, 0);
	a * b
} !+ 3 * 0.35
