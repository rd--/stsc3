;; dragtokeyboard (jmcc) ; http://www.iamas.ac.jp/~aka/dspss2004/materials/
{
	var in = LfSaw([21000, 21001], 0) * (LfPulse(ExpRand(0.1, 1), 0, 0.3) * 0.2 + 0.02);
	var sr = ExpRand(300, 3000) + [-0.6, 0.6];
	var pw = { :w | LfPulse(ExpRand(0.1, 12), 0, w) };
	Rlpf(
		in * LfPulse(sr, 0, MouseY(0.01, 0.99, 0, 0.2)),
		sr * (pw(0.4) * 0.2 + 0.2 + (pw(0,7) * 0.2)),
		0.1
	)
}.overlap(4, 4, 2)
