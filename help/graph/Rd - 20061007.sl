(* 20061007 ; rd *)
var lwr = 48;
var tr = { Dust(0.65) } ! 2;
{
	var n = LinLin(LfNoise2(0.1), -1, 1, lwr, 72);
	var e = Decay2(tr, 0.05, TRand(0.05, 0.75, tr));
	var x = PinkNoise() * e * 0.1;
	var m = LfNoise2(0.1);
	var f = Lag(n.MidiCps, 0.25);
	CombC(x, lwr.MidiCps.Recip, f.Recip, LinLin(m, -1, 1, 1, 8))
} !> 12 * 0.1
