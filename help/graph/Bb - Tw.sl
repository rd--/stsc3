;; http://earslap.com/article/sctweeting.html
var a = LocalIn(1, 0);
var x = SinOsc((Decay(Impulse([4, 4.005], 0), 1000 * a.abs) * 50), a).Distort;
x <! LocalOut(x.sum)

;; http://earslap.com/article/sctweeting.html ; wait to start
var f = LocalIn(2, 0).tanh;
var k = Latch(f.first.abs, Impulse(0.5, 0));
f <! LocalOut(f + AllpassN(Pulse([2, 3], k * 0.01 + 0.000001) * 0.9, 1, k * 0.3, 100 * k))

;; http://earslap.com/article/sctweeting.html
var f = LocalIn(2, 0).tanh;
var k = Latch(f.first.abs, Impulse(1 / 4, 0));
f <! LocalOut(f + CombC(Blip([4, 6], 100 * k + 50) * 0.9, 1, k * 0.3, 50 * f))

;; http://earslap.com/article/sctweeting.html
{
	var a = LfNoise1(Rand(0, 0.2));
	DelayC(Bpf(WhiteNoise() * Dust2(a * a * 4 ** 2).Lag(0.008), IRand(0, 10000) + 300, 0.09), 3, a * 1.5 + 1.5) * 45
} !+ 80

;; http://earslap.com/article/sctweeting.html
AllpassC(SinOsc(55, 0).tanh, 0.4, TExpRand(0.0002, 0.4, Impulse(8, 0)).roundTo([0.002, 0.0004]), 2)
