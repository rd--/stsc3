(* Convolution *)
var tr = Dust(100);
var vc = {
	var freq = TrRand(tr, 2700, 2750);
	var amp = TrRand(tr, 0.1, 0.2);
	Hpf(amp * Convolution(Saw(freq), tr, 512), 10000)
};
Splay2(vc ! 4)
