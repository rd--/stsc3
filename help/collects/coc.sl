;; wandering sines ; https://github.com/cianoc/supercollider_fragments
var o = SinOsc((LfNoise1(0.5) * 600 + (LfSaw(1.5, 0) * 50 + 500)).abs, 0) * 0.1;
var d = CombN(o, 3.0, [1.35, 0.7], 6);
Pan2(o, 0, 1) + d

;; random sine waves ; https://github.com/cianoc/supercollider_fragments
var f = { Pan2(SinOsc(SinOsc(1 / 10, 0.rrand(6)) * 200 + 600, 0), 0.rrand(1), 0.05) };
f.dup(15).Splay2

;; random sine waves ; https://github.com/cianoc/supercollider_fragments ; Rand
var f = { Pan2(SinOsc(SinOsc(1 / 10, Rand(0, 6)) * 200 + 600, 0), Rand(0, 1), 0.05) };
f.dup(15).Splay2

;; nice use of blip ; https://github.com/cianoc/supercollider_fragments
var t = Impulse(6, 0);
var o = Blip(TRand(48, 72, t).MidiCps, TRand(1, 12, t)) * TRand(-0.5, 0.4, t).max(0);
var p = Pan2(o, TRand(-1.0, 1.0, t), Decay2(t, 0.1, 3) * 0.5);
p + CombL(p, 2.0, 4/6, 6)

;; interesting rising sounds ; https://github.com/cianoc/supercollider_fragments
var f = { arg c; Pan2(SinOsc(LfSaw((c * 0.2 + 1)/3, 0) * 500 + 700, 0), LfNoise0(1), 0.05) };
1.to(5).collect(f).sum

;; use of dust with rising sounds ; https://github.com/cianoc/supercollider_fragments
var f = {
	var s = RingzBank(Dust(1 / 3) * 0.1, { 1000.expRand(10000) }.dup(3), [1], { 1.0.rrand(4.0) }.dup(15));
	Pan2(s, LfTri(3.0.rrand(10.0), 0), 0.1)
};
f.dup(20).sum

;; use of dust with rising sounds ; https://github.com/cianoc/supercollider_fragments ; Rand
var f = {
	var s = RingzBank(Dust(1 / 3) * 0.1, { ExpRand(1000, 10000) }.dup(3), [1], { Rand(1, 4) }.dup(15));
	Pan2(s, LfTri(Rand(3, 10), 0), 0.1)
};
f.dup(20).sum

;; pretty ; nice, but inessential ; https://github.com/cianoc/supercollider_fragments
var f = {
	arg i;
	var freq = MouseX(0.1.rrand(5.0), 3.0.rrand(20.0), 0, 0.2);
	var amp = LfNoise0(MouseX(1.0.rrand(6.0), 6.0.rrand(1.0), 0, 0.2)).max(0);
	var osc = SinOsc(SinOsc(freq, 0) * MouseY(10, 50, 0, 0.2) + 200.0.rrand(5000.0), 0) * amp;
	Pan2(osc, 1.0.rand2, 0.03) };
1.to(12).collect(f).sum

;; pretty ; nice, but inessential ; https://github.com/cianoc/supercollider_fragments ; Rand
var f = {
	arg i;
	var freq = MouseX(Rand(0.1, 5), Rand(3, 20), 0, 0.2);
	var amp = LfNoise0(MouseX(Rand(1, 6), Rand(1, 6), 0, 0.2)).max(0);
	var osc = SinOsc(SinOsc(freq, 0) * MouseY(10, 50, 0, 0.2) + Rand(200, 5000), 0) * amp;
	Pan2(osc, Rand(-1, 1), 0.03) };
1.to(12).collect(f).sum

;; random impulses ; https://github.com/cianoc/supercollider_fragments
var f = {
	var freq = 100.expRand(3000);
	var amp = Decay2(Dust(1 / 5), 0.1, (freq ** -0.7) * 100);
	Pan2(SinOsc(freq * (LfNoise1(1 / 6) * 0.4 + 1), 0) * amp, LfNoise1(1 / 8), 1) };
f.dup(15).sum * 0.1

;; random impulses ; https://github.com/cianoc/supercollider_fragments ; Decay2
var f = {
	var freq = ExpRand(100, 3000);
	var t = Dust(1 / 5);
	var amp = Decay2(t, 0.01, freq ** -0.7 * 100);
	Pan2(SinOsc(freq * (LfNoise1(1 / 6) * 0.4 + 1), 0) * amp, LfNoise1(1 / 8), 1) };
f.dup(15).sum * 0.1

;; angry birds ; https://github.com/cianoc/supercollider_fragments
var lfo = LfNoise1([28, 27]) * 400 + 2000;
SinOsc(lfo, 0) * 0.1

;; lasers ; https://github.com/cianoc/supercollider_fragments ; requires=kr
SinOsc(Adsr(Dust(1), 0.1, 0.4, 0.8, 0.5, [-4]).kr * 10000, 0) * 0.1

;; saw as an lfo ; https://github.com/cianoc/supercollider_fragments
var lfo = LfSaw(2, 0) * -100 + 600;
SinOsc(lfo, 0) * 0.1

;; synched impulses ; https://github.com/cianoc/supercollider_fragments
var sync = 5;
[
	SinOsc(100, 0) * Decay2(Impulse(3 / sync, 0), 0.01, 1),
	SinOsc(300, 0) * Decay2(Impulse(7 / sync, 0), 0.01, 1),
	SinOsc(500, 0) * Decay2(Impulse(5 / sync, 0), 0.01, 1),
	SinOsc(700, 0) * Decay2(Impulse(2 / sync, 0), 0.01, 1),
	SinOsc(900, 0) * Decay2(Impulse(9 / sync, 0), 0.01, 1),
	SinOsc(1100, 0) * Decay2(Impulse(6 / sync, 0), 0.01, 1),
	SinOsc(1300, 0) * Decay2(Impulse(1 / sync, 0), 0.01, 1)
].sum * 0.1

;; synched impulses ; https://github.com/cianoc/supercollider_fragments
var sync = 5;
var f = { arg frq, num; SinOsc(frq, 0) * Decay2(Impulse(num / sync, 0), 0.01, 1) };
[f.value(100, 3), f.value(300, 7), f.value(500, 5), f.value(700, 2), f.value(900, 9), f.value(1100, 6), f.value(1300, 1)].Splay2 * 0.2

;; synchronised impulses ; https://github.com/cianoc/supercollider_fragments
var sync = 5;
var freq = [1, 3, 5, 7, 9, 11, 13];
var numer = [3, 7, 5, 2, 9, 6, 1];
var f = { arg i; SinOsc(freq[i] * 100, 0) * Decay2(Impulse(numer[i] / sync, 0), 0.01, 1) };
(1 .. freq.size).collect(f).Splay2 * 0.5

;; nice buzzing effect
var speed = 14;
var f = SinOsc(1000, 0) * 150 + 300;
var t = Impulse(1 / 3, 0);
var v = { arg n; SinOsc(f * n, 0) * (LfNoise1(Rand(speed, speed * 2)) * 0.5 + 0.5) / n };
1.to(12).collect(v).sum * 0.1

;; additive saw
var f = 100;
var t = Impulse(1 / 3, 0);
var dt = [1.4, 1.1, 2, 1, 1.8, 2.9, 4, 0.3, 1, 3.6, 2.3, 1.1];
var v = { arg n; SinOsc(f * n, 0) * Decay2(t, 0.01, dt.at(n)) / n };
1.to(12).collect(v).sum * 0.1

;; lovely bells ; https://github.com/cianoc/supercollider_fragments
var t = Impulse(1 / 3, 0);
var m = Rand(1, 3).dup(2);
var env = Decay2(t, 0.01 * m, 1 * m) / 1.to(6);
SinOsc([60, 64, 67, 71, 74, 78].MidiCps, 0).sum * env * 0.1

;; interesting drone ; https://github.com/cianoc/supercollider_fragments
var freq = [40, 42, 43, 45, 47, 48, 41, 42].MidiCps;
var amp = LfNoise1({ Rand(0.1, 0.5) }.dup(8)) * 0.5 + 0.5;
Pan2(SinOsc(freq, 0), { Rand(-1, 1) }.dup(8), amp).sum * 0.1

;; great inharmonic spectrum ; https://github.com/cianoc/supercollider_fragments
var freq = [72, 135, 173, 239, 267, 306, 355, 473, 512, 572, 626];
var amp = [0.25, 0.11, 0.12, 0.04, 0.1, 0.15, 0.05, 0.01, 0.03, 0.02, 0.12];
(SinOsc(freq, 0) * amp).Splay2 * 0.2

;; random bells, let it run for a while ; https://github.com/cianoc/supercollider_fragments
var k = 12;
var tr = Dust(3 / 7);
var f = {
	SinOsc(Rand(50, 4000), 0) * Decay2(tr, 0.01, Rand(0.2, 3)) * Rand(0, 1)
};
f.dup(k).Splay2 / k

;; shimmering harmonics ; https://github.com/cianoc/supercollider_fragments
var harmonics = 16;
var f = {
	var amp = SinOsc(1 / Rand(3, 6), 0) * Rand(0.1, 0.9);
	Pan2(SinOsc(ExpRand(100, 2000), 0), Rand(-1, 1), amp)
};
f.dup(harmonics).sum / (2 * harmonics)

;; worth experimenting with ; https://github.com/cianoc/supercollider_fragments
var tr = Dust(3 / 7);
var f0 = Rand(100, 400);
var f = {
	arg partial;
	var env = Asr(tr, 0, 5, [0]) / partial;
	var amp = LfNoise1(Rand(5, 12)).max(0);
	Pan2(SinOsc(f0 * partial, 0), Rand(-1, 1), env * amp)
};
1.to(16).collect(f).sum * 0.5

;; multiple sines ; https://github.com/cianoc/supercollider_fragments
var speeds = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11] / 20;
var f0 = (MouseX(0, 36, 0, 0.2).roundTo(7) + 24).MidiCps;
var f = {
	arg partial;
	Pan2(SinOsc(f0 * partial, 0), Rand(-1, 1), SinOsc(speeds.atRandom, 0).max(0))
};
var harmonics = 16;
1.to(harmonics).collect(f).sum / harmonics * 0.5

;; more bells ; https://github.com/cianoc/supercollider_fragments
var env = Decay2(Dust(1 / 3), 0.01, 2) * 0.1;
var osc = SinOsc({ Rand(300, 1200) }.dup(12), 0);
Pan2(osc, { Rand(-1, 1) }.dup(12), env).sum

;; pink noise, frequencies emerge ; https://github.com/cianoc/supercollider_fragments
RLPF(PinkNoise() * 0.3, { LfNoise0(12) }.dup(2) * 500 + 500, 0.2)

;; random drones ; https://github.com/cianoc/supercollider_fragments
{ RingzBank(PinkNoise() * 0.0005, { ExpRand(60, 10000) }.dup(15), [1], [1]) }.dup

;; this one floats in and out ; https://github.com/cianoc/supercollider_fragments
var totalPartials = 3;
var f = {
	var base = ExpRand(50, 100);
	var freq = { Rand(1, 24) * base * Rand(1, 1.1) }.dup(totalPartials);
	var amp = { Rand(0.1, 0.5) }.dup(10);
	var res = RingzBank(GrayNoise() * Rand(0.03, 0.1), freq, amp, [1]);
	Pan2(res, LfNoise1(1), (SinOsc(10 / Rand(1, 5), 0) * 0.005).max(0));
};
f.dup(8).sum * LfNoise1(1 / 10).abs

;; phase modulation, all three ; https://github.com/cianoc/supercollider_fragments
{
	var car = LfNoise0(9) * 300 + 700;
	var mod = LfNoise0(9) * 500 + 700;
	var ix = LfNoise0(9) * 6 + 12;
	PmOsc(car, mod, ix, 0) * 0.15
}.dup

;; ping pong ; https://github.com/cianoc/supercollider_fragments
var rate = 5;
var trig = Impulse(5, 0);
var freq = TRand([36, 60], [72, 86], trig).MidiCps;
var ratio = 2;
var env = Decay2(trig, 0, 1.25 / rate);
PmOsc(freq, freq * ratio, 3 + env * 4, 0) * env * 0.25

;; sample and hold ; https://github.com/cianoc/supercollider_fragments
var i = Latch(LfSaw(MouseX(1.1, 30, 0, 0.2), 0) * 5 + 5, Impulse(10, 0));
PmOsc(300, 356, i, 0) * 0.1

;; envelope used also for the index ; https://github.com/cianoc/supercollider_fragments
var f = Latch((SinOsc([100, 200, 300, 550], 0) * 100 + 110).sum, Impulse(7, 0));
var e = Decay2(Impulse(7, 0), 0.02, 0.2);
PmOsc(f, f * [1.25, MouseX(1, 3, 0, 0.2)], e * [5, MouseY(3, 9, 0, 0.2)], 0) * e * 0.1

;; generating melodic runs ; https://github.com/cianoc/supercollider_fragments
SinOsc(Latch(LfSaw(MouseX(0.1, 20, 0, 0.2), 0) * 500 + 600, Impulse(10, 0)), 0) * 0.1

;; generating melodic runs, line ; https://github.com/cianoc/supercollider_fragments
SinOsc(Latch(LfSaw(Line(0.1, 20, 60, 0), 0) * 500 + 600, Impulse(10, 0)), 0) * 0.1

;; risefallpad ; https://github.com/cianoc/supercollider_fragments ; requires=voicer
var vc = {
	arg e;
	var freq = e.p.unitCps;
	var gate = e.w;
	var osc1 = Saw(freq + LfTri(0.3, 0));
	var osc2 = Saw((freq.CpsMidi - 0.03).MidiCps);
	var filterEnv = Adsr(gate, 1, 1.4, 0.05, 0.2, 0);
	var ampEnv = Adsr(gate, 0.1, 0.01, 0.6, 1.5, 0) * e.z;
	Pan2(RLPF(osc1 + osc2, 3000 * filterEnv + freq, 1.5), e.o * 2 - 1, ampEnv)
};
Voicer(16, vc).sum
