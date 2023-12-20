(* LinExp *)
var mod = SinOsc(Line(1, 10, 10), 0);
[SinOsc(mod * 400 + 500, 0), SinOsc(LinExp(mod, -1, 1, 100, 900), 0)] * 0.1

(* LinExp *)
var s = LfSaw(0.25, 1);
SinOsc(LinExp(s + 1, 0, 2, 220, 440), 0) * 0.1

(* LinExp ; mce *)
var s = [LfSaw(1 / 4, 1), SinOsc(1 / 3, pi / 2)];
SinOsc(LinExp(s + 1, 0, 2, 220, 440), 0) * 0.1

(* LinExp *)
var tr = Dust(2 / [3, 5]);
var note = TRand(48, 72, tr);
var freq = Lag(note.MidiCps, 0.05);
var env = Decay2(tr, 0.005, TRand(0.1, 0.9, tr)) * 0.2;
var lfo = LinExp(SinOsc(0.12, 0), -1, 1, 300, 8000);
var snd = LfSaw(freq + [0, 1], 0);
2.timesRepeat {
	snd := Rlpf(snd, lfo, 0.2).SoftClip
};
snd * env
