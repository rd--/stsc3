(* Seq ; shared seq, different patterns *)
var a = Dseq(inf, [1 3 2 7 8]);
var t = Impulse(5, 0);
var f = Demand(t, 0, [a a]) * 30 + 340;
SinOsc(f, 0) * 0.1

(* Seq ; distinct seq, equal patterns *)
var a = Dseq(inf, [1 3 2 7 8]);
var b = Dseq(inf, [1 3 2 7 8]);
var t = Impulse(5, 0);
var f = Demand(t, 0, [a b]) * 30 + 340;
SinOsc(f, 0) * 0.1

(* Seq ; at audio rate ; mouse control *)
var n = Dseq(inf, [1 3 2 7 8 32 16 18 12 24]);
var x = MouseX(1, 10000, 1, 0.1);
var t = Impulse(x, 0);
var f = Demand(t, 0, n) * 30 + 340;
SinOsc(f, 0) * 0.1

(* Seq ; at control rate *)
var n = Dseq(3, [1 3 2 7 8]);
var x = MouseX(1, 40, 1, 0.1);
var t = Impulse(x, 0);
var f = Demand(t, 0, n) * 30 + 340;
SinOsc(f.kr, 0) * 0.1

(* Seq ; the Sc2 Sequencer UGen is somewhat like the Demand & Seq idiom below *)
var tr = Impulse(6, 0);
var n0 = Demand(tr, 0, Dseq(inf, [60 62 63 58 48 55]));
var n1 = Demand(tr, 0, Dseq(inf, [63 60 48 62 55 58]));
LfSaw([n0 n1].MidiCps, 0) * 0.05

(* Seq ; rather than Mce expansion at tr, it can be clearer to view tr as a functor *)
var freq = [2 3 5].collect { :trFreq |
	var tr = Impulse(trFreq, 0);
	var sq = Dseq(inf, [60 63 67 69]);
	Demand(tr, 0, sq).MidiCps
};
SinOsc(freq, 0).Splay * 0.1

(* Seq *)
var t = Impulse(2, 0);
var m = Demand(t, 0, Dseq(inf, [55 60 63 62 60 67 63 58]));
SinOsc(m.MidiCps, 0) * 0.1
