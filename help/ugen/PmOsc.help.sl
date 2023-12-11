(* PmOsc ; mouse control *)
PmOsc(440, MouseY(1, 550, 0, 0.2), MouseX(1, 15, 0, 0.2), 0) * 0.1

(* PmOsc ; carrier and modulator not linked *)
var tr = Impulse(10, 0);
var c = TRand(100, 5000, tr);
var m = TRand(100, 5000, tr);
PmOsc(c, m, 12, 0) * 0.1

(* PmOsc ; modulator expressed as ratio *)
var carrier = LfNoise0(MouseY(3, 11, 0, 0.2)) * 500 + 700;
var modRatio = MouseX(1, 2, 0, 0.2);
PmOsc(carrier, carrier * modRatio, 12, 0) * 0.1

(* PmOsc ; random parameters, linear modulation *)
var dur = 6;
{ :tr |
	var cf = TRand(0, 2000, tr);
	var mf = TRand(0, 800, tr);
	var pme = TRand(0, 12, tr);
	var l1 = TRand(-1, 1, tr);
	var l2 = TRand(-1, 1, tr);
	var l = TLine(l1, l2, dur, tr);
	var pm = TLine(0, pme, dur, tr);
	EqPan2(PmOsc(cf, mf, pm, 0), l)
}.OverlapTexture(2, 2, 4).Mix * 0.05

(* PmOsc ; event control *)
var s = Voicer(16) { :e |
	var cps = (e.x * 24 + 42).MidiCps;
	var cpsv = cps + (cps * SinOsc(e.y * 4 + 4, 0) * 0.02);
	var mfreq = LinLin(LfPulse(1 / 8, 0, 0.5), 0, 1, 1.01, 2.01) * cps;
	var ix = TxLine(3, 0.001, 0.2, e.w);
	PmOsc(cpsv, mfreq, ix, 0) * e.z * e.w
}.Sum;
XFade2(s, GVerb(Bpf(s, 90.MidiCps, 1), 50, 5, 0.5, 0.5, 15, 0, 0.7, 0.5, 300), 0.2, 1)
