(* Resonz ; resonator *)
{ :tr |
	Resonz(
		Pulse(TRand(2, 7, tr), 0.5),
		{ TExpRand(120, 2500, tr) } ! 4,
		0.005
	).Splay2
}.OverlapTexture(2, 4, 6).Mix

(* Resonz *)
Resonz(WhiteNoise() * 0.5, 2000, 0.1)

(* Resonz ; modulate frequency ; SinOsc *)
var f = SinOsc(0.5, 0) * 40 + 220;
Resonz(WhiteNoise(), f, 0.1)

(* Resonz ; modulate frequency ; XLine *)
var f = XLine(1000, 8000, 10);
Resonz(WhiteNoise() * 0.1, f, 0.05)

(* Resonz ; modulate bandwidth *)
var bw = XLine(1, 0.001, 8);
Resonz(WhiteNoise() * 0.1, 2000, bw)

(* Resonz ; modulate bandwidth opposite direction *)
var bw = XLine(0.001, 1, 8);
Resonz(WhiteNoise() * 0.1, 2000, bw)

(* Resonz ; PinkNoise ; event control *)
Voicer(16) { :e |
	EqPan2(
		Resonz(
			PinkNoise(),
			e.p.UnitCps,
			e.y * 0.25) * 24,
		e.i * 2 - 1
	) * e.z * e.w
}.Mix

(* Resonz ; PinkNoise ; event control *)
Voicer(16) { :e |
	var env = Perc(e.w, 0.01, 1 + e.j, -4);
	var freq = e.p.UnitCps;
	var rq = LinLin(e.y, 0, 1, 0.05, 0.25) / freq;
	var scl = 900;
	EqPan2(Resonz(PinkNoise(), freq, rq) * scl * e.z, e.i * 2 - 1) * env
}.Mix
