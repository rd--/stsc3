(* tremulate (jmcc) #1 *)
{
	var f = Rand(500, 900);
	var r = Rand(30, 90);
	var o = SinOsc(f * [1.0 1.2 1.5 1.8], 0);
	var a = 0.Max(LfNoise2(r ! 4) * 0.1);
	var l = { 1.Rand2 } ! 4;
	EqPan2(o * a, l).Mix.CombN(0.1, 0.1, 1)
}.xfade(2, 0.5)
