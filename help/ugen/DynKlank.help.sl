(* DynRingzBank ; c.f. DynKlank *)
var tr = Dust(25);
var k = DynRingzBank(Trig(tr, SampleDur()), (1 .. 5) * 2700, { TRand(0.025, 0.04, tr) } ! 5, [0.1]);
var n = k + (BrownNoise() * 0.075 * Trig(tr, TRand(0.01, 0.03, tr)));
var f = Lpf(Hpf(n, Lag(TRand(6000, 7500, tr), 0.01)), Lag(TRand(4000, 5500, tr), 0.01));
Pan2(f, Lag(TRand(-0.5, 0.5, tr), 0.01), 1)
