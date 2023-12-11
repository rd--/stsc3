(* VarSaw ; phase value = (0, 2 * pi), offset to lowest and midpoint ascending *)
VarSaw(110, 1 * [0, 0.25], 0.5) * 0.1

(* VarSaw ; per-note width modulation *)
var d = LinLin(LfNoise2(0.1), -1, 1, 0.05, 0.5);
var tr = Impulse(1 / d, 0);
var w0 = TRand(0, 0.35, tr);
var w1 = TRand(0.65, 1, tr);
var w = Phasor(tr, (w1 - w0) / SampleRate(), w0, w1, 0);
var o = VarSaw(TRand(36, 72, tr).MidiCps, 0, w) * Decay2(tr, 0.1, d);
EqPan(o, TRand(-1, 1, tr)) / 7

(* VarSaw (Jmcc) *)
var f0 = 80;
var tr = Impulse(4, 0) * 0.05;
var n = WhiteNoise().MulAdd(3, 3).RoundTo(0.5) + 1;
var f = Lag(f0 * Latch(n, tr), 0.03);
var osc = VarSaw(
	f * [1, 1.004, 1.505, 1.499],
	0,
	MouseY(0, 1, 0, 0.2)
) * Decay2(tr, 0.005, 1.4);
CombN(osc.clump(2).Mix * 0.5, 0.375, 0.375, 4).SoftClip
