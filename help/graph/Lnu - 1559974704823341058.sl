(* https://twitter.com/lukiss163/status/1559974704823341058 ; requires=StandardN *)
var f = {
	StandardN((LfdNoise1(1) * 8 + 8).RoundTo(4), 1, 0.5, 0).Abs
};
var p = LfPulse(LinExp(f(), -1, 1, 4, 80).RoundTo(1).MidiCps.Lag(0.05), 0, 0.5);
var w = LinExp(LfSaw({ -8 } ! 2 * f(), 0), -1, 1, 100, f() * 8000);
(LeakDc(MoogFf(p, w, (LfdNoise1(1) * 2 + 2).Abs, 0), 0.995)).Tanh
