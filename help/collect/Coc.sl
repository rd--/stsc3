;; Wandering sines ; https://github.com/cianoc/supercollider_fragments
var f = (LfNoise1(0.5) * 600 + (LfSaw(1.5, 0) * 50 + 500)).Abs;
var o = SinOsc(f, 0) * 0.1;
var d = CombN(o, 3.0, [1.35, 0.7], 6);
Pan2(o, 0, 1) + d

;; Random sine waves ; https://github.com/cianoc/supercollider_fragments
{
	Pan2(SinOsc(SinOsc(1 / 10, 6.Rand) * 200 + 600, 0), 1.Rand, 0.05)
} !+ 15

;; Nice use of Blip ; https://github.com/cianoc/supercollider_fragments
var t = Impulse(6, 0);
var o = Blip(TRand(48, 72, t).MidiCps, TRand(1, 12, t)) * TRand(-0.5, 0.4, t).Max(0);
var p = Pan2(o, TRand(-1.0, 1.0, t), Decay2(t, 0.1, 3) * 0.5);
p + CombL(p, 2.0, 4/6, 6)

;; Interesting rising sounds ; https://github.com/cianoc/supercollider_fragments
(1 .. 5).collect { :c |
	Pan2(SinOsc(LfSaw((c * 0.2 + 1) / 3, 0) * 500 + 700, 0), LfNoise0(1), 0.05)
}.sum

;; Use of Dust with rising sounds ; https://github.com/cianoc/supercollider_fragments ; Rand
{
	var s = RingzBank(Dust(1 / 3) * 0.1, { ExpRand(1000, 10000) } ! 3, [1], { Rand(1, 4) } ! 15);
	Pan2(s, LfTri(Rand(3, 10), 0), 0.1)
} !+ 20

;; Pretty nice but inessential ; https://github.com/cianoc/supercollider_fragments ; Rand
(1 .. 12).collect { :i |
	var freq = MouseX(Rand(0.1, 5), Rand(3, 20), 0, 0.2);
	var amp = LfNoise0(MouseX(Rand(1, 6), Rand(1, 6), 0, 0.2)).Max(0);
	var osc = SinOsc(SinOsc(freq, 0) * MouseY(10, 50, 0, 0.2) + Rand(200, 5000), 0) * amp;
	Pan2(osc, 1.Rand2, 0.03)
}.sum

;; Random impulses ; https://github.com/cianoc/supercollider_fragments
{
	var freq = 100.ExpRand(3000);
	var amp = Decay2(Dust(1 / 5), 0.1, (freq ** -0.7) * 100);
	Pan2(SinOsc(freq * (LfNoise1(1 / 6) * 0.4 + 1), 0) * amp, LfNoise1(1 / 8), 1)
} !+ 15 * 0.1

;; Random impulses ; https://github.com/cianoc/supercollider_fragments ; Decay2
{
	var freq = ExpRand(100, 3000);
	var t = Dust(1 / 5);
	var amp = Decay2(t, 0.01, freq ** -0.7 * 100);
	Pan2(SinOsc(freq * (LfNoise1(1 / 6) * 0.4 + 1), 0) * amp, LfNoise1(1 / 8), 1)
} !+ 15 * 0.1

;; Angry birds ; https://github.com/cianoc/supercollider_fragments
var lfo = LfNoise1([28, 27]) * 400 + 2000;
SinOsc(lfo, 0) * 0.1

;; Lasers ; https://github.com/cianoc/supercollider_fragments ; requires=kr
SinOsc(Adsr(Dust(1), 0.1, 0.4, 0.8, 0.5, [-4]).kr * 10000, 0) * 0.1

;; Saw as an Lfo ; https://github.com/cianoc/supercollider_fragments
var lfo = LfSaw(2, 0) * -100 + 600;
SinOsc(lfo, 0) * 0.1

;; Synched impulses ; https://github.com/cianoc/supercollider_fragments
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

;; Synched impulses, abstracted ; https://github.com/cianoc/supercollider_fragments
var sync = 5;
var f = { :frq :num |
	SinOsc(frq, 0) * Decay2(Impulse(num / sync, 0), 0.01, 1)
};
[f(100, 3), f(300, 7), f(500, 5), f(700, 2), f(900, 9), f(1100, 6), f(1300, 1)].Splay2 * 0.2

;; Synchronised impulses, structured ; https://github.com/cianoc/supercollider_fragments
var sync = 5;
var freq = [1, 3, 5, 7, 9, 11, 13];
var numer = [3, 7, 5, 2, 9, 6, 1];
(1 .. freq.size).collect { :i |
	SinOsc(freq[i] * 100, 0) * Decay2(Impulse(numer[i] / sync, 0), 0.01, 1)
}.Splay2 * 0.2

;; Nice buzzing effect
var speed = 14;
var f = SinOsc(1000, 0) * 150 + 300;
var t = Impulse(1 / 3, 0);
(1 .. 12).collect { :n |
	SinOsc(f * n, 0) * (LfNoise1(Rand(speed, speed * 2)) * 0.5 + 0.5) / n
}.sum * 0.1

;; Additive sawtooth
var f = 100;
var t = Impulse(1 / 3, 0);
var dt = [1.4, 1.1, 2, 1, 1.8, 2.9, 4, 0.3, 1, 3.6, 2.3, 1.1];
(1 ..  12).collect { :n |
	SinOsc(f * n, 0) * Decay2(t, 0.01, dt[n]) / n
}.sum * 0.1

;; Lovely bells ; https://github.com/cianoc/supercollider_fragments
var t = Impulse(1 / 3, 0);
var m = Rand(1, 3) ! 2;
var env = Decay2(t, 0.01 * m, 1 * m) / (1 .. 6);
SinOsc([60, 64, 67, 71, 74, 78].MidiCps, 0).sum * env * 0.1

;; Interesting drone ; https://github.com/cianoc/supercollider_fragments
var freq = [40, 42, 43, 45, 47, 48, 41, 42].MidiCps;
var amp = LfNoise1({ Rand(0.1, 0.5) } ! 8) * 0.5 + 0.5;
Pan2(SinOsc(freq, 0), { 1.Rand2 } ! 8, amp).sum * 0.1

;; Great inharmonic spectrum ; https://github.com/cianoc/supercollider_fragments
var freq = [72, 135, 173, 239, 267, 306, 355, 473, 512, 572, 626];
var amp = [0.25, 0.11, 0.12, 0.04, 0.1, 0.15, 0.05, 0.01, 0.03, 0.02, 0.12] * 0.2;
Splay2(SinOsc(freq, 0) * amp)

;; Random bells, let it run for a while ; https://github.com/cianoc/supercollider_fragments
var k = 12;
var tr = Dust(3 / 7);
Splay2({
	SinOsc(Rand(50, 4000), 0) * Decay2(tr, 0.01, Rand(0.2, 3)) * Rand(0.1, 1)
} ! k) / k

;; Shimmering harmonics ; https://github.com/cianoc/supercollider_fragments
var harmonics = 16;
{
	var amp = SinOsc(1 / Rand(3, 6), 0) * Rand(0.1, 0.9);
	Pan2(SinOsc(ExpRand(100, 2000), 0), 1.Rand2, amp)
} !+ harmonics / (2 * harmonics)

;; Worth experimenting with ; https://github.com/cianoc/supercollider_fragments
var tr = Dust(3 / 7);
var f0 = Rand(100, 400);
(1 .. 16).collect { :partial |
	var env = Asr(tr, 0, 5, [0]) / partial;
	var amp = LfNoise1(Rand(5, 12)).Max(0);
	Pan2(SinOsc(f0 * partial, 0), 1.Rand2, env * amp)
}.sum * 0.5

;; Multiple sines ; https://github.com/cianoc/supercollider_fragments
var speeds = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11] / 20;
var f0 = (MouseX(0, 36, 0, 0.2).RoundTo(7) + 24).MidiCps;
var harmonics = 16;
(1 .. harmonics).collect { :partial |
	Pan2(SinOsc(f0 * partial, 0), 1.Rand2, SinOsc(speeds.atRandom, 0).Max(0))
}.sum / harmonics * 0.5

;; More bells ; https://github.com/cianoc/supercollider_fragments
var env = Decay2(Dust(1 / 3), 0.01, 2) * 0.1;
var osc = SinOsc({ Rand(300, 1200) } ! 12, 0);
Pan2(osc, { 1.Rand2 } ! 12, env).sum

;; Pink noise, frequencies emerge ; https://github.com/cianoc/supercollider_fragments
Rlpf(PinkNoise() * 0.3, { LfNoise0(12) } ! 2 * 500 + 500, 0.2)

;; Random drones ; https://github.com/cianoc/supercollider_fragments
{
	RingzBank(
		PinkNoise() * 0.0005,
		{ ExpRand(60, 10000) } ! 15,
		[1],
		[1]
	)
} ! 2

;; This one floats in and out ; https://github.com/cianoc/supercollider_fragments
var totalPartials = 3;
{
	var base = ExpRand(50, 100);
	var freq = { Rand(1, 24) * base * Rand(1, 1.1) } ! totalPartials;
	var amp = { Rand(0.1, 0.5) } ! 10;
	var res = RingzBank(GrayNoise() * Rand(0.03, 0.1), freq, amp, [1]);
	Pan2(res, LfNoise1(1), (SinOsc(10 / Rand(1, 5), 0) * 0.005).Max(0));
} !+ 8 * LfNoise1(1 / 10).Abs

;; Phase modulation, all three ; https://github.com/cianoc/supercollider_fragments
{
	var car = LfNoise0(9) * 300 + 700;
	var mod = LfNoise0(9) * 500 + 700;
	var ix = LfNoise0(9) * 6 + 12;
	PmOsc(car, mod, ix, 0) * 0.15
} ! 2

;; Ping pong ; https://github.com/cianoc/supercollider_fragments
var rate = 5;
var trig = Impulse(5, 0);
var freq = TRand([36, 60], [72, 86], trig).MidiCps;
var ratio = 2;
var env = Decay2(trig, 0, 1.25 / rate);
PmOsc(freq, freq * ratio, 3 + env * 4, 0) * env * 0.25

;; Sample and hold ; https://github.com/cianoc/supercollider_fragments
var i = Latch(LfSaw(MouseX(1.1, 30, 0, 0.2), 0) * 5 + 5, Impulse(10, 0));
PmOsc(300, 356, i, 0) * 0.1

;; Envelope used also for the index ; https://github.com/cianoc/supercollider_fragments
var f = Latch((SinOsc([100, 200, 300, 550], 0) * 100 + 110).sum, Impulse(7, 0));
var e = Decay2(Impulse(7, 0), 0.02, 0.2);
PmOsc(f, f * [1.25, MouseX(1, 3, 0, 0.2)], e * [5, MouseY(3, 9, 0, 0.2)], 0) * e * 0.1

;; Generating melodic runs ; https://github.com/cianoc/supercollider_fragments
SinOsc(Latch(LfSaw(MouseX(0.1, 20, 0, 0.2), 0) * 500 + 600, Impulse(10, 0)), 0) * 0.1

;; Generating melodic runs, line in place of mouse ; https://github.com/cianoc/supercollider_fragments
SinOsc(Latch(LfSaw(Line(0.1, 20, 60, 0), 0) * 500 + 600, Impulse(10, 0)), 0) * 0.1

;; Rise fall pad ; https://github.com/cianoc/supercollider_fragments ; requires=voicer
var vc = { :e |
	var mnn = e.x * 24 + 48;
	var freq = mnn.MidiCps;
	var gate = e.w;
	var osc1 = Saw(freq + LfTri(0.3, 0));
	var osc2 = Saw((mnn - 0.03).MidiCps);
	var filterEnv = Adsr(gate, 1, 1.4, 0.05, 0.2, 0);
	var ampEnv = Adsr(gate, 0.1, 0.01, 0.6, 1.5, 0) * e.z;
	Pan2(Rlpf(osc1 + osc2, 3000 * filterEnv + freq, 1.5), e.o * 2 - 1, ampEnv)
};
Voicer(16, vc).sum
