(* Convolution *)
var tr = Dust(100);
{
	var freq = TRand(2700, 2750, tr);
	var amp = TRand(0.1, 0.2, tr);
	Hpf(amp * Convolution(Saw(freq), tr, 512), 10000)
} !^ 4
