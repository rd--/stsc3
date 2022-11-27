;; PMOsc ; mouse control
PMOsc(440, MouseY(1, 550, 0, 0.2), MouseX(1, 15, 0, 0.2), 0) * 0.1

;; PMOsc ; carrier and modulator not linked
var tr = Impulse(10, 0);
var c = TRand(100, 5000, tr);
var m = TRand(100, 5000, tr);
PMOsc(c, m, 12, 0) * 0.1

;; PMOsc ; modulator expressed as ratio
var carrier = LFNoise0(MouseY(3, 11, 0, 0.2)) * 500 + 700;
var modRatio = MouseX(1, 2, 0, 0.2);
PMOsc(carrier, carrier * modRatio, 12, 0) * 0.1

;; PMOsc ; random parameters, linear modulation
var dur = 6;
OverlapTexture({ :tr |
	var cf = TRand(0, 2000, tr);
	var mf = TRand(0, 800, tr);
	var pme = TRand(0, 12, tr);
	var l1 = TRand(-1, 1, tr);
	var l2 = TRand(-1, 1, tr);
	var l = TLine(l1, l2, dur, tr);
	var pm = TLine(0, pme, dur, tr);
	Pan2 (PMOsc(cf, mf, pm, 0), l, 0.05)
}, 2, 2, 4)

;; PMOsc ; event control
var s = Voicer(16, { :e |
	var cps = (e.x * 24 + 42).midiCps;
	var cpsv = cps + (cps * SinOsc(e.y * 4 + 4, 0) * 0.02);
	var mfreq = LinLin(LFPulse(1 / 8, 0, 0.5), 0, 1, 1.01, 2.01) * cps;
	var ix = TXLine(3, 0.001, 0.2, e.w);
	PMOsc(cpsv, mfreq, ix, 0) * e.z * e.w
}).sum;
XFade2(s, GVerb(BPF(s, 90.midiCps, 1), 50, 5, 0.5, 0.5, 15, 0, 0.7, 0.5, 300), 0.2, 1)
