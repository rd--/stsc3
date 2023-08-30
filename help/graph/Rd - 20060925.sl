(* 20060925 ; rd *)
var t = Impulse((LfNoise0([3, 3.25]) * 16) + 18, 0);
var o = Bpf(
	WhiteNoise() * Decay2(t, 0.01, TRand(0.005, MouseY(0.01, 0.15, 0, 0.1), t)),
	TRand(10, MouseX(100, 12000, 0, 0.1), t),
	TRand(0, 1, t)
);
var p = PvRandComb(
	Fft(BufAlloc(1, 2048), o, 0.5, 0, 1, 0),
	TExpRand(0.15, 1, t),
	t
);
(o * 0.1).Lpz2 + Ifft(p, 0, 0)
