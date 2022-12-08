;; deepsea (jrhb)
var amp = 0.1;
var variation = 0.9;
var tr = Dust(0.25);
var pan = TRand(-1, 1, tr);
var n = TRand(7, 46, tr);
var dt1 = 25.0 + TRand(-1.7, 1.7, tr);
var dt2 = dt1 + LfNoise2(2) * variation * 0.001;
var freq = 901 + TRand(0, 65, tr);
var t = Impulse(dt2.reciprocal, 0) * 100;
var count = PulseCount(t, tr);
var mul = count < n;
var u1 = Bpf(mul * t, freq, 0.1);
var u2 = Bpf(u1, freq * (count % LinLin(LfNoise1(1), -1, 1, 2, 20) + 1), 0.2);
var o = Pan2(u2, pan, amp * 10);
var f1 = Bpf(o * 5, 700, 0.1);
var f2 = CombL(Lpf(f1 * LfNoise1(0.1).max(0), 800), 0.5, 0.5, 1) + f1.reversed;
var f3 = f2;
5.timesRepeat { f3 := AllpassN(f3, 0.18, { 0.06.Rand } ! 2 + 0.06, 8) };
Lpf(f2 + f3, 400) + (LfNoise1(0.2) * f2)
