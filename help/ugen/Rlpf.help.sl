(* Rlpf *)
Rlpf(Dust([12, 15]), LfNoise1(1 / [3, 4]) * 1500 + 1600, 0.02)

(* Rlpf *)
var f = SinOsc(0.5, 0) * 40 + 220;
Rlpf(WhiteNoise(), f, 0.1)

(* Rlpf *)
var f = SinOsc(XLine(0.7, 300, 20), 0) * 3600 + 4000;
Rlpf(LfSaw(200, 0) * 0.1, f, 0.2)

(* Rlpf *)
var ctl = Rlpf(LfSaw(5, 0) * 0.1, 25, 0.03);
SinOsc(ctl * 200 + 400, 0) * 0.1

(* Rlpf *)
var freq = Demand(
	Impulse(0.25, 0),
	0,
	Dseq(inf, [27, 24, 22, 24] + 2)
).MidiCps + [0, 0.3];
Rlpf(LfPulse(freq, 0, 0.2) * 0.4 - 0.2, 300, 0.5)

(* Rlpf *)
Rlpf(
	Dust(LfNoise1([0.2, 1.5]).Range([200, 2000], [300, 2700])),
	LfNoise1([1.5, 0.2]).Range(40, 300),
	0.02
)
