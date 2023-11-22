(* 20061027 ; rd *)
var h0 = {
	var f = 40;
	var h = HenonN(
		[f, f * 0.5],
		LfNoise0(1) * 0.2 + 1.2,
		LfNoise0(1) * 0.15 + 0.15,
		0,
		0
	);
	Saw(h * 3200 + 1600) * 0.35
};
var h1 = {
	var n = LfNoise0(2);
	var p = n.Range(2400, 3200);
	var o = Blip(
		HenonN(
			40,
			MouseX(1.2, 1.4, 0, 0.1),
			MouseY(0.2, 0.3, 0, 0.1), 0, 0).Range(p, p * 2),
		LfNoise0(32).Range(1, 32)
	);
	EqPan(
		o,
		n.Range(-0.75, 0.75)
	) * n.Range(0.55, 0.85) * 0.35
};
h0() + h1()
