;; tremulate (jmcc) #1
{
	var f = 500 + 400.rand;
	var r = 30 + 60.rand;
	var o = SinOsc(f * [1.0, 1.2, 1.5, 1.8], 0);
	var a = 0.max(LfNoise2(r ! 4) * 0.1);
	var l = { 1.0.rand2 } ! 4;
	Pan2(o * a, l, 1).sum.CombN(0.1, 0.1, 1)
}.xfade(2, 0.5)
