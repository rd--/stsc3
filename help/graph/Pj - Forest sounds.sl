(* forest sounds (pj) *)
{
	var n1 = BrownNoise();
	var n2 = LfNoise2(50);
	var o = SinOsc(n2 * 50 + 50, 0) * 100 + 2000;
	Bpf(n1, o, 0.001) * 10
} ! 2
