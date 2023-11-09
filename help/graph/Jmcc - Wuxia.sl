(* wuxia5 (jmcc) ; http://www.iamas.ac.jp/~aka/dspss2004/materials/ ; graph rewrite *)
{ :tr |
	var amp = 0.1;
	var eg = LinSeg(tr, [0, 0.03, 1, 4, 1, 2, 0]);
	var noise = WhiteNoise() * 0.1 * eg;
	var dt = TrXLine(
		tr,
		TrExpRand(tr, 10, 12000),
		TrExpRand(tr, 10, 12000),
		4
	).Recip;
	6.timesRepeat {
		noise := LeakDc(CombC(noise, 0.1, dt, dt * 6) + dt, 0.995)
	};
	{
		var pos = TrRand(tr, -1, 1);
		EqPan2(
			CombC(noise, 0.2, TrRand(tr, 0.1, 0.2), 3) + noise,
			TrLine(tr, pos, pos + TrRand(tr, -2, 2), 4)
		)
	} !+ 5 * eg * amp
}.OverlapTexture(8, 0.01, 2)
