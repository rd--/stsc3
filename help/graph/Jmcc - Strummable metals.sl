;; strummable metals ; use mouse to strum strings ; jmcc ; requires=kr
var mousex = MouseX(0, 1, 0, 0.2);
var strFunc = { :i |
	var trigger = Hpz1(mousex > (0.25 + (i * 0.07))).Abs;
	var pluck = PinkNoise() * Decay(trigger, 0.05) * 0.04.kr;
	var n = 15;
	EqPan2(
		RingzBank(
			pluck,
			{ 300 * i + LinRand(0, 8000, 0) } ! n,
			nil,
			{ Rand(1, 4) } ! n
		),
		i * 0.2 - 0.5
	);
};
LeakDc(Lpf((1 .. 8).collect(strFunc).sum, 12000), 0.995)
