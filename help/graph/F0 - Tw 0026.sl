(* http://www.fredrikolofsson.com/f0blog/?q=node/537 ; graph rewrite (rd) *)
var n = [
	0 0 0 0 0 0 0 3 4 6 4 0 3 2 0 0 0 0 1 6 0 1 0 0 0 0 0 0 4 0 0 3
	1 1 2 0 0 3 3 0 0 3 4 0 1 3 0 0 0 0 1 0 1 7 0 0 5 6 3 0 4 0 9 0
];
var b = [0 2 4 5 7 9 11].asLocalBuf;
var tr = Impulse(4, 0);
var k = DegreeToKey(b, Demand(tr, 0, Dseq(inf, n)), 12);
var e = Decay2(tr, 0.01, TrRand(tr, 0.15, 0.5));
{
	var m = 48 + k + TrRand(tr, 0, 0.05);
	var b = Blip(m.MidiCps, TrRand(tr, 1, 7)) * e * 8;
	CombC(b.Tanh / 8, 1, 1, 8) * TrRand(tr, 0.05, 0.15)
} ! 2
