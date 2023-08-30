(* dragtokeyboard (jmcc) ; http://www.iamas.ac.jp/~aka/dspss2004/materials/ *)
{
	var pw = { :u :w |
		LfPulse(ExpRand(0.1, u), 0, w)
	};
	var in = LfSaw([21000, 21001], 0) * (pw(1, 0.3) * 0.2 + 0.02);
	var sr = ExpRand(300, 3000) + [-0.6, 0.6];
	Rlpf(
		in * LfPulse(sr, 0, MouseY(0.01, 0.99, 0, 0.2)),
		sr * (pw(12, 0.4) * 0.2 + 0.2 + (pw(12, 0.7) * 0.2)),
		0.1
	)
}.overlap(4, 4, 2)
