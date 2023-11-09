(* TrigAllocator ; no voice stealing *)
var t = Impulse(1, 0);
var d = TrIRand(t, 2, 9);
var w = TrigAllocator(8, 0, t, d);
var e = Asr(w, d / 3, d / 3, -4) * TrRand(w, 0.1, 0.2);
var f = TrIRand(w, 48, 72).MidiCps + TrRand(w, -9, 9);
var o = SinOsc(f, 0) * e;
o.Splay2

(* TrigAllocator ; voice stealing ; algorithm input selects rule *)
var t = Impulse(4, 0);
var d = TrRand(t, 0.2, 2);
var w = TrigAllocator(5, 1, t, d);
var e = Asr(w, 0.01, d / 2, -4) * TrRand(w, 0.1, 0.2);
var f = TrIRand(w, 48, 72).MidiCps + TrRand(w, -9, 9);
var o = SinOsc(f, 0) * e;
o.Splay2
