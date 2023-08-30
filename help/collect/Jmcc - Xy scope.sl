(* mouse x controls phase relation ; / shape = in-phase ; \ shape = out-of-phase ; O shape = 90 degree phase relation ; jmcc *)
SinOsc(80, [0, MouseX(0, 2 * pi, 0, 0.2)]) / 3

(* harmonic relations ; jmcc *)
SinOsc([MouseX(60, 300, 0, 30) + 0.04, MouseY(60, 300, 1 , 30)], 0) * 0.1

(* harmonic relations ; jmcc *)
Rlpf(
	LfPulse(
		[MouseX(60, 300, 0, 30) + 0.04, MouseY(60, 300, 0, 30)],
		0,
		0.5
	) * 0.1 - 0.05,
	800,
	1
)

(* panning a mono signal ; x = left channel, y = right channel ; jmcc *)
Pan2(BrownNoise(), MouseX(-1, 1, 0, 0.2), 0.1)

(* uncorrelated channels ; jmcc *)
{ BrownNoise() } ! 2 * 0.1

(* uncorrelated channels ; jmcc *)
{ PinkNoise() } ! 2 * 0.1

(* waveform ; jmcc *)
[
	LfTri(80, 0) * 0.2,
	CombN(
		OnePole(BrownNoise() * 0.1, MouseX(0, 1, 0, 0.2)),
		1 / 80,
		1 / 80,
		0.3
	) * SinOsc(80.04, 0.5 * pi).Abs
]

(* waveform ; jmcc *)
[
	LfTri(80, 0) * 0.2,
	CombN(
		Resonz(BrownNoise() * 0.4, MouseX(20, 12000, 1, 0.2), 0.2),
		1 / 80,
		1 / 80,
		0.3
	) * SinOsc(80, 1).Abs
]

(* filtered decorrelated noise ; jmcc *)
CombN(
	Resonz({ BrownNoise() } ! 2 * 0.4, MouseX(20, 12000, 1, 0.2), 0.2),
	1 / 80,
	1 / 80,
	0.3
)

(* quad ; eye ; jmcc *)
var m = MouseX(0.5, 1, 0, 0.2);
var a = LfPulse(200, 0, 0.3) * 0.05;
var b = OnePole(a, m);
var c = OnePole(b, m);
var d = OnePole(c, m);
var e = OnePole(d, m);
[b, c, d, e]
