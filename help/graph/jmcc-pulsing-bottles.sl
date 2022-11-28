;; pulsing bottles ; jmcc
var n = 6; ;; number of 'bottles'
{
        Pan2(
		Resonz(
			WhiteNoise(),
			400 + LinRand(0, 7000, 0),
			0.01
		),
		SinOsc(0.1 + 0.4.rand, (2 * pi).rand),
		1
	) * LfPulse(4 + 10.0.rand, 0, 0.7.rand) * 0.8 / n
}.dup(n).sum

;; pulsing bottles (jmcc) #2
var n = 6;
var f = {
	var a = LfPulse(4 + Rand(0, 10),  0,  Rand(0, 0.7)) * 0.8 / n;
	var l = SinOsc(0.1 + Rand(0, 0.4), Rand(0, 2 * pi));
	Pan2(Resonz(WhiteNoise(), 400 + LinRand(0, 7000, 0), 0.01), l, 1) * a
};
f.dup(n).sum
