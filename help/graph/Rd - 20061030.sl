(* 20061030 ; rd *)
var tr = Dust([12, 18]);
var f = LfNoise0(TrRand(tr, 1, 64)) * [9000, 12000] + 9500;
var o = Saw([
	f,
	f * TrRand(tr, 0.750, 0.7505),
	f * TrRand(tr, 0.975, 1.025)
]).sum;
(o * TrRand(tr, 0.0, 0.5)).Clip2(0.75) * 0.15
