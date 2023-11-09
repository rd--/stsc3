(* DynRingzBank ; c.f. DynKlank *)
var tr = Dust(25);
var k = DynRingzBank(Trig(tr, SampleDur()), (1 .. 5) * 2700, { TrRand(tr, 0.025, 0.04) } ! 5, [0.1]);
var n = k + (BrownNoise() * 0.075 * Trig(tr, TrRand(tr, 0.01, 0.03)));
var f = Lpf(Hpf(n, Lag(TrRand(tr, 6000, 7500), 0.01)), Lag(TrRand(tr, 4000, 5500), 0.01));
Pan2(f, Lag(TrRand(tr, -0.5, 0.5), 0.01), 1)
