(* https://scsynth.org/t/5296 ; tm881 *)
{ Saw({ Rand(33, 2000) } ! 2 * XLine(Rand(0.125, 8), Rand(0.125, 8), 60)) } !+ 99 / 99

(* https://scsynth.org/t/5296 ; tm881 ; rd edit *)
{
	var f = { Rand(33, 2000) } ! 2 * TRand(0.125, 4, Impulse(1 / 10, 0));
	var t = Impulse(1 / 2 * TRand(0.125, 4, Impulse(1 / 5, 0)), 0);
	var a = Perc(t, 0.01, 1, -4);
	FreeVerb(Saw(f) * a, 0.33, 0.5, 0.5)
} !> 16 * 0.1

(* https://scsynth.org/t/5296 ; tm881 *)
var a = Blip(
	[
		Rand(1, 30),
		TRand(0.125, 4, Impulse([1 / 4, 1 / 7], 0)),
		XLine(Rand(0.125, 4), Rand(0.125, 4), 15)
	].product,
	200);
var r = GVerb(Saw(322) * a, 10, 0.3, 0.5, 0.5, 15, 1, 0.7, 0.5, 300);
r.transposed.Sum * 0.25
