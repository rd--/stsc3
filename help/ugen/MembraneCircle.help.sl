(* MembraneCircle ; event control *)
Voicer(16) { :e |
	var loss = LinExp(e.y, 0, 1, 0.99999, 0.99950);
	var wobble = SinOsc(2, 0);
	var tension = LinExp(e.x, 0, 1, 0.01, 0.1) + (wobble * 0.0001);
	var env = Perc(e.w, 0.0001, 1 - e.z, -4) * (e.z + (e.y / 4));
	EqPan2(MembraneCircle(PinkNoise() * env, tension, loss), e.i * 2 - 1)
}.Mix (* Cpu Load: Mac Studio M1 Max 2022: 16 voices = 33% *)
