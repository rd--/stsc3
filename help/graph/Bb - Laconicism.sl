;; http://earslap.com/weblog/music-release-laconicism.html
var k = DmdFor(6.4, 0, Seq(inf, [0.05, Choose(1, [0.04, 0.08])]));
Splay(Integrator((LfNoise0(5 ! 3) * k).roundTo(k / 10), 1).sin.sqrt.tanh, 0.3, 1, 0, true)

;; http://earslap.com/weblog/music-release-laconicism.html ; wait
var x = LfNoise1(0.5 * LocalIn(1, 0) + 0.1) * 0.5 + 0.5;
var s = PitchShift(PitchShift(Pulse([90, 90.01], x), 10, x * 4, x, 0), 10, 4 - (x * 4), 1 - x, 0);
s <! LocalOut(x)

;; http://earslap.com/weblog/music-release-laconicism.html ; wait
var q = [0, 3, 5, 7, 10];
var t = Impulse(4, 0) * LfNoise0(500) > 0;
var f = DmdOn(t, 0, Choose(inf, (q + 12 ++ q + 33).MidiCps));
PitchShift(Saw(f) * Decay(t, 3), 7, 2, 0, 0) ! 2

;; http://earslap.com/weblog/music-release-laconicism.html
var snd = Hpf(Pulse([[0.1, 0.11], [0.12, 0.13]], 0.6) * 0.005, 99);
Hpf(MidEq(Limiter(GVerb(snd, [[1, 5 / 4], [1.5, 2]], 99, 0.5, 0.5, 15, 1, 0,7, 0.5) * 300, 1, 0.01) * 0.1, 9000, 0.9, 9), 200).sum.transpose.sum

;; http://earslap.com/weblog/music-release-laconicism.html ; wait
var f = LocalIn(2, 0).tanh;
var k = Latch(f.first.abs, Impulse(0.5, 0));
f <! LocalOut(f + AllpassN(Pulse([2, 3], k * 0.01 + 0.000001) * 0.9, 1, k * 0.3, 100 * k))

;; http://earslap.com/weblog/music-release-laconicism.html
var a = Bpf(Saw([40, 40.001]), LfNoise0(128) + 1 * 4000 + 146, LfNoise1(1) + 1 * 0.05 + 0.01).tanh;
CombC(a, 9, a.abs.Lag(2) * 9, a.abs.Lag(1) * 100)

;; http://earslap.com/weblog/music-release-laconicism.html
var f = LocalIn(2, 0).tanh;
var k = Latch(f.first.abs, Impulse(1 / 4, 0));
f <! LocalOut(f + CombC(Blip([4, 6], 100 * k + 50) * 0.9, 1, k * 0.3, 50 * f))

;; http://earslap.com/weblog/music-release-laconicism.html
var t = Impulse(8, 0) * LfNoise1(2);
CombL((Saw([3, 4]) * Decay(t, 0.1)).tanh, 1, TRand(0, 0.01, t).roundTo(0.00015), TRand(-30, 30, t))

;; http://earslap.com/weblog/music-release-laconicism.html
var n = { :freq | LfNoise0(freq) };
var f = [60, 61];
var l = n(6);
(BBandPass((n(4).max(l).max(SinOsc(f * (l * 9).ceiling.Lag(0.1), 0) * 0.7)), f, n(1).abs / 2) * 700 * l.Lag(1)).tanh

;; http://earslap.com/weblog/music-release-laconicism.html
var t = [0, 0, 0, 1, 5, 7, 10, 12, 12, 12] + 30;
var a = DmdFor(1/8, 0, Choose(inf, t + 24 ++ t ++ t));
(BHiPass(LfNoise1(8) ** 6, [a, a + 7].MidiCps, a / 3000) * (67 - a)).tanh

;; http://earslap.com/weblog/music-release-laconicism.html
AllpassL(SinOsc(55,0).tanh, 0.4, TExpRand(0.0002, 0.4, Impulse(8, 0)).roundTo([0.002, 0.004]), 2)

;; http://earslap.com/weblog/music-release-laconicism.html
var i = { :freq | Impulse(freq, 0) };
var ph = Integrator(Integrator(i(64).Lag(LfNoise1(2 ! 2)  * 2 + 2) * 99, 0.9), 0.99).Fold2(pi);
SinOsc(LagUd(i(2), 0, 0.4) * 360, ph)

;; http://earslap.com/weblog/music-release-laconicism.html
var t = [0, 3, 5, 7, 10, 12] + 40;
var p = DmdFor(1/4, 0, Choose(inf, (t + 12 ++ t).MidiCps));
var b = TRand(1500, 2000, Impulse(16, 0)).Lag(0.1);
Blip([b, b + p], 1).mean ! 2 ** 2

;; http://earslap.com/weblog/music-release-laconicism.html
var i = Impulse(8, 0).Lag(0.3) ! 2;
10.timesRepeat {
	i := LeakDc(AllpassC(i, 1, LfNoise0(8).range(0.00001, 0.2), -0.15) * LfNoise0(8).range(1, 3), 0.995).tanh
};
i

;; http://earslap.com/weblog/music-release-laconicism.html
var n = { :freq | LfNoise0(freq) };
var v = Blip([20000, 20000 - 9], 1) * (n(16) * 0.5 + 0.5 ** 9);
42.timesRepeat {
	v := LeakDc(AllpassC(v, 1, n(5) * 0.05 + (0.05 + 0.001), 100), 0.995)
};
(v * 99).tanh

;; http://earslap.com/weblog/music-release-laconicism.html
(Hpf(LfNoise1(2), [10, 10.1]) * 100).tanh

;; http://earslap.com/weblog/music-release-laconicism.html ; requires=kr
var x = DmdFor(1/8, 0, Choose(inf, [0, Choose(1, [0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1])]));
LeakDc(Brf(Saw(8) * Decay2(x, 0.01, 0.3).kr ** 1.5, x * 20 + [45.1, 45], 0.1), 0.995).tanh

;; http://earslap.com/weblog/music-release-laconicism.html ; wait
var n = { :freq :mul :add | LfNoise0(freq) * mul + add };
var v = Blip([60, 61], 5) * (n(4, 1, 0) ** 8);
12.timesRepeat {
	v := LeakDc(CombC(v, 1, n(1, 0.05, 0.06).Lag(5000), 9), 0.995)
};
Limiter(v, 0.9, 1)
