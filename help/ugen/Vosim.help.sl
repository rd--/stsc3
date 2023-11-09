(* Vosim ; mouse control of frequency *)
Splay2(
	Vosim(
		Impulse(110 + [-1 0 1], 0),
		MouseX([110 220 440], 880, 1, 0.2),
		[3 5 7],
		[0.77 0.88 0.99]
	)
) * 0.1

(* Vosim ; noise modulation of frequency *)
Splay2(
	Vosim(
		Impulse(110 + [-1 0 1], 0),
		LinExp(LfNoise2([0.35 0.25 0.15]), -1, 1, [110 220 440], 880),
		[3 5 7],
		[0.77 0.88 0.99]
	)
) * 0.1

(* Vosim *)
{ :tr |
	{
		Vosim(
			Impulse(110 + TrRand(tr, -1, 1), 0),
			TrExpRand(tr, 110, 880),
			TrChoose(tr, [3, 5, 7]),
			TrChoose(tr, [0.77, 0.88, 0.99])
		)
	} ! 2
}.OverlapTexture(3, 5, 3) * 0.1

(* Vosim *)
var p = TrRand(Impulse([3, 7], 0), 0, 1);
var t = Impulse([9, 27] * (1 + (p > 0.95)), 0);
var f = TrRand(t, [40, 120, 220], [440, 990, 880]);
var n = TrRand(t, 2, [4, 8, 12]);
var d = TrRand(t, [0.2, 0.4, 0.6], [0.6, 0.8, 1]);
var a = TrRand(t, 0, [0.2, 0.6, 1]);
var l = TrRand(t, -1, 1);
var x = MouseX(0.25, 2, 0, 0.2);
var y = MouseY(0.25, 0.85, 0, 0.2);
var z = 9;
var freq = f * x * LinLin(LfNoise2(z), -1, 1, 0.25, 2);
var decay = d * y * LinLin(LfNoise2(z), -1, 1, 0.25, 2);
EqPan2(
	(Vosim(t, freq, n, decay) * a).sum,
	l
).sum * 0.25

(* Vosim ; requires=voicer *)
Voicer(16) { :e |
	var tr = Impulse(e.p.UnitCps, 0);
	var freq = LinExp(e.y, 0, 1, 440, 880);
	EqPan2(Vosim(tr, freq, e.k + 1 * e.y * 12, e.j), e.i * 2 - 1) * e.w * e.z
}.sum
