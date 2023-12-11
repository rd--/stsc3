(* DynRingzBank *)
var tr = Dust(25);
var mkRand = { :lhs :rhs |
	Lag(TRand(lhs, rhs, tr), 0.01)
};
var k = DynRingzBank(
	Trig(tr, SampleDur()),
	(1 .. 5) * 2700,
	{ mkRand(0.025, 0.04) } ! 5,
	[0.1]
);
var n = k + (BrownNoise() * 0.075 * Trig(tr, mkRand(0.01, 0.03)));
var f = Lpf(Hpf(n, mkRand(6000, 750)), mkRand(4000, 5500));
EqPan(f, mkRand(-1, 1))
