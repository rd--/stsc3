(* DwgPluckedStiff ; re-sounding *)
var tr = TDmdFor(Dseq(inf, [1 1 2 1 1 1 2 3 1 1 1 1 2 3 4] * 0.175), 0, 1);
var freq = [
	Sequencer([60 62 63 58 48 55], tr),
	Sequencer([63 60 48 62 55 58], tr)
].MidiCps;
var amp = TrRand(tr, 0.05, 0.65); (* pulse amplitude (0 - 1, def = 0.5) *)
var gate = 1; (* synth release *)
var pos = TrRand(tr, 0.05, 0.25); (* pluck position (0 - 1, def = 0.14) *)
var c1 = 1 / TrRand(tr, 0.25, 1.75); (* reciprocal of decay time (def = 1.0) *)
var c3 = TrRand(tr, 10, 1400); (* high frequency loss factor (def = 30) *)
var inp = amp * LfClipNoise(2000) * Decay2(tr, 0.001, TrRand(tr, 0.05, 0.150)) * LfClipNoise(2000); (* pluck signal *)
var release = TrRand(tr, 0.05, 0.15); (* release time (seconds, def = 0.1) *)
var fB = TrRand(tr, 1, 4); (* inharmonicity factor (def = 2.0) *)
var ps = DwgPluckedStiff(freq, amp, gate, pos, c1, c3, inp, release, fB);
Pan2(ps, TrRand(tr, -1, 1), 0.1).sum
