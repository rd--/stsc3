;; Seq ; shared seq, different patterns
var a = Lseq(inf, [1, 3, 2, 7, 8]);
var t = Impulse(5, 0);
var f = Demand(t, 0, [a, a]) * 30 + 340;
SinOsc(f,  0) * 0.1

;; Seq ; distinct seq, equal patterns
var a = Lseq(inf, [1, 3, 2, 7, 8]);
var b = Lseq(inf, [1, 3, 2, 7, 8]);
var t = Impulse(5, 0);
var f = Demand(t, 0, [a, b]) * 30 + 340;
SinOsc(f,  0) * 0.1

;; Seq ; at audio rate ; mouse control
var n = Lseq(inf, [1, 3, 2, 7, 8, 32, 16, 18, 12, 24]);
var x = MouseX(1, 10000, 1, 0.1);
var t = Impulse(x, 0);
var f = DmdOn(t, 0, n) * 30 + 340;
SinOsc(f, 0) * 0.1

;; Seq ; at control rate
var n = Lseq(3, [1, 3, 2, 7, 8]);
var x = MouseX(1, 40, 1, 0.1);
var t = Impulse(x, 0);
var f = DmdOn(t, 0, n) * 30 + 340;
SinOsc(f.kr, 0) * 0.1

;; Seq ; the SC2 Sequencer UGen is somewhat like the DmdOn & Seq idiom below
var tr = Impulse(6, 0);
var n0 = DmdOn(tr, 0, Lseq(inf, [60, 62, 63, 58, 48, 55]));
var n1 = DmdOn(tr, 0, Lseq(inf, [63, 60, 48, 62, 55, 58]));
LfSaw([n0, n1].MidiCps, 0) * 0.05

;; Seq ; rather than Mce expansion at tr, it can be clearer to view tr as a functor
var m = { :f |
	var tr = Impulse(f, 0);
	var sq = Lseq(inf, [60, 63, 67, 69]);
	DmdOn(tr, 0, sq).MidiCps
};
SinOsc([2, 3, 5].collect(m), 0).Splay2 * 0.1

;; Seq
var t = Impulse(2, 0);
var m = DmdOn(t, 0, Lseq(inf, [55, 60, 63, 62, 60, 67, 63, 58]));
SinOsc(m.MidiCps, 0) * 0.1
