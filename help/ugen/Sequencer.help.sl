(* Sequencer ; audio rate *)
var freq = Sequencer([60 62 65 69], Impulse(2, 0)).MidiCps;
SinOsc(freq, 0) * 0.1

(* Sequencer ; control rate *)
var freq = Sequencer([60 62 65 69], Impulse(2, 0).kr).MidiCps;
SinOsc(freq, 0) * 0.1

(* Sequencer ; jmcc *)
var clock = Impulse(8, 0);
var trig1 = ImpulseSequencer([0.4 0.1 0.2 0.1], clock);
var root = Sequencer([24 26 24 22], PulseDivider(clock, 64, 0));
var x = Rlpf(
	{ GrayNoise() } ! 2 * Decay2(trig1, 0.005, 0.7) * 0.4,
	MouseX(200, 8000, 1, 0.2),
	0.2
).Distort;
var trig2 = ImpulseSequencer([0.4 0 0.1 0.1 0.4 0.1 0.1 0.1], clock);
var freq = (Sequencer([33 33 35 36 45 47 38 40 33 33 35 36 47 48 50 52], clock) + root).MidiCps;
var z = VarSaw(freq * [1, 1.505], 0, MouseY(0, 1, 0, 0.2)) * Decay2(trig2, 0.005, 1.4);
CombN(x + z * 0.5, 0.26, 0.26, 4).SoftClip

(* Sequencer ; jmcc *)
var p = Drand(inf, [
	Dseq(1, [0 0 1 2 7 8 9 8 10 10 1 2 1 2 3 4 0 0 1 2 7 8 9 8 11 11 1 2 1 2 3 4]),
	Dseq(2, [0 0 1 2 7 8 3 4 0 0 1 2 1 2 3 4 0 0 1 2 7 8 3 4 0 0 1 2 1 2 3 4])
]);
var clock = Impulse(12, 0);
var root = Sequencer([57, 59, 62, 55], PulseDivider(clock, 128, 0));
var trig1 = ImpulseSequencer(
	[1 0 0 0 1 0 0 1 1 0 0 0 1 0 0 1 1 0 0 0 1 0 0 1 1 0 1 0 1 0 1 1],
	PulseDivider(clock, 2, 0)
);
var r = Lpf(
	Rlpf(
		{ BrownNoise() } ! 2 * Decay2(trig1, 0.005, 0.7) * 3,
		MouseX(200, 300, 1, 0.2),
		0.4
	).Distort,
	800
);
var trig2 = ImpulseSequencer([0.4 0.1 0.2 0.1], clock);
var x = Rlpf(
	{ GrayNoise() } ! 2 * Decay2(trig2, 0.005, 0.3) * 0.4,
	MouseX(200, 8000, 1, 0.2),
	0.2
).Distort;
var scale = [0 2 3 5 7 8 10].asLocalBuf;
var trig3 = ImpulseSequencer([0.4 0 0.1 0.1 0.4 0.1 0.1 0.1] * 1.5, clock);
var freq = (DegreeToKey(scale, Demand(clock, 0, p), 12) - 12 + root).MidiCps;
var z = Rlpf(
	VarSaw(freq * [1, 1.505], 0, MouseY(0, 1, 0, 0.2)) * Decay2(trig3, 0.005, 0.8),
	MouseX(200, 8000, 1, 0.2),
	0.2
).Distort;
var trig4 = ImpulseSequencer(
	[1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1] * 0.2,
	clock
);
var y = SinOsc((root + 24).MidiCps, 0) * Decay2(trig4, 0.005, 0.2);
var trig5 = ImpulseSequencer(
	[1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1] * 0.3,
	clock);
var w = SinOsc((root + 24 + 7).MidiCps, 0) * Decay2(trig5, 0.005, 0.2);
var snd = (r * 0.1 + x + z + [y, w]) * 0.4;
(CombN(snd, 0.51, 0.51, 4) * 0.4 + snd.reversed).SoftClip

(* Sequencer ; jmcc *)
var z = 0, a = 0.1, b = 0.2, c = 0.4;
var p = Dseq(1, [
	Dseq(2, [c z z z z z z z]),
	Dseq(2, [c z z b z z b z c z z z b z b a]),
	Drand(inf, [
		Dseq(2, [c z z b z z b z c z b z c z b z]),
		Dseq(2, [c z a b z a b z c z b z c a b a]),
		Dseq(2, [c a a b z a b a c z b z c a b a]),
		Dseq(2, [c a a b z a b a c z b z z b a a]),
		Dseq(2, [c z z z z z z z]),
		Dseq(2, [c z z b z z b z c z z z b z b a])
	])
]);
var q = Drand(inf, [
	Dseq(2, [c z b z a a a a b z b z z z b z]),
	Dseq(2, [c z z z z z z z b z b z z z c z]),
	Dseq(2, [c z z z a z z z b z b z a z c z])
]);
var clock = Impulse(9, 0);
(* bass drum *)
var trig1 = PulseDivider(clock, 16, 0);
var r = SinOsc(80, 0) * Decay2(trig1, 0.005, 0.5) * 0.3;
(* drum *)
var trig2 = ImpulseSequencer([p], clock);
var exc2 = WhiteNoise() * Decay2(trig2, 0.005, 0.05);
var x = RingzBank(exc2, { Rand(80, 800) } ! 4, nil, { Rand(0.05, 0.2) } ! 4).Distort * 0.2;
(* metallic *)
var trig3 = ImpulseSequencer([q], clock);
var exc3 = WhiteNoise() * Decay2(trig3, 0.005, 0.05);
var s = RingzBank(exc3, { Rand(3500, 4000) } ! 4, nil, { Rand(0.05, 0.2) } ! 4).Distort * 0.1;
(* whine *)
var exc4 = { GrayNoise() } ! 2 * 0.0007;
var y = { :tr |
	var env = SinOsc(TRand(1, 6, tr), { TRand(0, 2.pi, tr) } ! 2) * 0.5 + 0.5;
	DynRingzBank(
		exc4,
		{ TiRand(200, 2500, tr) } ! 4,
		[1],
		{ TRand(0.2, 0.8, tr) } ! 4
	) * env
}.OverlapTexture(4, 2, 2).Mix.ScaleNeg(MouseX(-1, 1, 0, 0.2));
var snd = x + (y * 0.5) + s + r;
6.timesRepeat {
	snd := AllpassN(Lpz2(snd), 0.03, { Rand(0, 0.03) } ! 2, 5)
};
snd

(* Sequencer ; jmcc *)
var clock = Impulse(XLine(24, 1, 12), 0);
var x = SinOsc(Sequencer(Dwhite(inf, 2000, 12000), clock), 0) * Decay2(clock, 0.002, 0.04);
{
	CombN(x, 0.1, { ExpRand(0.02, 0.05) } ! 2, 2) * Line(0.2, 0, 12)
} !+ 2
