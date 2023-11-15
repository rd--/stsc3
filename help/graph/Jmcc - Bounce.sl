(* jmcc ; http://www.iamas.ac.jp/~aka/dspss2004/materials/ *)
var freq = 80;
var trig = Impulse(4, 0) * 0.05;
var m = Latch((WhiteNoise() * 3 + 3).RoundTo(0.5) + 1, trig);
var o = VarSaw(
	Lag(freq * m, 0.03) * [1 1.004 1.505 1.499],
	0,
	MouseY(0, 1, 0, 0.2)
);
var z = (o * Decay2(trig, 0.005, 1.4)).clump(2).Mix;
CombN(z * 0.5, 0.375, 0.375, 4).SoftClip
