(* https://scsynth.org/t/5296 ; tm881 *)
{ Saw({ Rand(33, 2000) } ! 2 * XLine(Rand(0.125, 8), Rand(0.125, 8), 60)) } !+ 99 / 99

(* https://scsynth.org/t/5296 ; tm881 *)
{
	var f = { Rand(33, 2000) } ! 2 * Rand(Impulse(1 / 10, 0), 0.125, 4);
	var t = Impulse(1 / 2 * Rand(Impulse(1 / 5, 0), 0.125, 4), 0);
	var a = Perc(t, 0.01, 1, -4);
	FreeVerb(Saw(f) * a, 0.33, 0.5, 0.5)
} !+ 32 * 0.1

(* https://scsynth.org/t/5296 ; tm881 *)
var a = Blip(
	[
		Rand(1, 30),
		Rand(Impulse([1 / 4, 1 / 7], 0), 0.125, 4),
		XLine(Rand(0.125, 4), Rand(0.125, 4), 15)
	].product,
	200);
var r = GVerb(Saw(322) * a, 10, 0.3, 0.5, 0.5, 15, 1, 0.7, 0.5, 300);
r.transposed.sum * 0.25
