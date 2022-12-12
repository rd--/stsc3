;; https://twitter.com/hrhkymd/status/1601331785409134592 ; requires=kr
var b = 1 / 25.6;
var p = Impulse(b, 0).kr;
var f = TChoose(p, [[58, 58.2, 62], [56, 56.2, 67]]) ++ [39, 51, 60, 63, 65];
var t = TChoose(p, [-2, 0, 4]) + f;
{
	var osc = SinOsc(
		t.MidiCps,
		Rlpf(VarSaw(b * 32, 0, 0.5) * 2000, 9, 0.1) * 0.001 + 10
	);
	var env = {
		LfPulse(b * 16, pi, LfNoise0(500)) * LfNoise0(b * 128).Range(0, 0.2)
	} ! 9;
	(osc * env.Lag(10)).sum
} ! 2
