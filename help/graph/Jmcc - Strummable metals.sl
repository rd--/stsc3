(* strummable metals ; use mouse to strum strings ; jmcc *)
var mousex = MouseX(0, 1, 0, 0.2);
var str = (1 .. 8).collect { :i |
	var trigger = Hpz1(mousex > (0.25 + (i * 0.07))).Abs;
	var pluck = PinkNoise() * Decay(trigger, 0.05) * 0.04;
	var n = 15;
	EqPan2(
		RingzBank(
			pluck,
			{ 300 * i + LinRand(0, 8000, 0) } ! n,
			nil,
			{ Rand(1, 4) } ! n
		),
		i * 0.2 - 0.5
	)
};
LeakDc(Lpf(str.Mix, 12000), 0.995)
