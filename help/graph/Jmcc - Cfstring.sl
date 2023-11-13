(* cfstring1 (jmcc) ; graph rewrite ; http://www.iamas.ac.jp/~aka/dspss2004/materials/ *)
var nc = 2;
{ :tr |
	var dgr = DegreeToKey(
		[0 2 4 5 7 9 11].asLocalBuf,
		Rand(tr, 0, 12),
		12
	);
	var freq = (60 + dgr).MidiCps;
	var pan = Rand(tr, -0.25, 0.25);
	var amp = Rand(tr, 0.1, 0.2);
	var fc = LinExp(LfNoise1(Rand(tr, 0.25, 0.4)), -1, 1, 500, 2000);
	var osc = {
		LfSaw(
			freq * [Rand(tr, 0.99, 1.01), Rand(tr, 0.99, 1.01)],
			0)
		* amp
	} !+ 8;
	var eg = Decay2(tr, 0.5, 1);
	var out = eg * Rlpf(osc.Distort * 0.2, fc, 0.1);
	PanAz(nc, out, [pan, pan + 0.3], 1, 2, 0.5).sum
}.OverlapTexture(2, 0, 9).Mix
