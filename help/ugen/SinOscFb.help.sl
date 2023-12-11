(* SinOscFb *)
SinOscFb([400, 301], MouseX(0, 4, 0, 0.2)) * 0.1

(* SinOscFb *)
var y = MouseY(10, 1000, 1, 0.2);
var x = MouseX(0.5 * pi, pi, 0, 0.2);
SinOscFb(y, x) * 0.1

(* SinOscFb *)
var y = MouseY(1, 1000, 1, 0.2);
var x = MouseX(0.5 * pi, pi, 0, 0.2);
SinOscFb(100 * SinOscFb(y, 0) + 200, x) * 0.1

(* SinOscFb ; OverlapTexture *)
{ :tr |
	var x = MouseX(0.15, 0.85, 0, 0.2);
	var f0 = Choose(tr, [110, 220, 440]);
	{
		var freq = f0 + TRand(0, f0, tr);
		var fb = LinLin(LfNoise2(1), -1, 1, 0, x);
		SinOscFb(freq, fb) * 0.1
	} ! 16
}.OverlapTexture(2, 6, 3).Mix

(* ---- SinOscFb ; overlap (scheduled) *)
{
	var x = MouseX(0.15, 0.85, 0, 0.2);
	var f0 = [110, 220, 440].atRandom;
	{
		var freq = f0 + f0.Rand;
		var fb = LinLin(LfNoise2(1), -1, 1, 0, x);
		EqPan2(SinOscFb(freq, fb), 1.Rand2) * 0.1
	} !+ 16
}.overlap(2, 6, 3)
