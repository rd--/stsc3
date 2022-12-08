;; https://sccode.org/1-4Qy ; f0 ; 0002
{
	var o = Bpf(Impulse(LinRand(0.5, 18.5, 0), 0), LinRand(0, 9999, 0), LinRand(0, 0.3, 0)) * 5;
	Release(Pan2(o, 1.Rand2, 1), 0, 3, 9)
}.playEvery { 2 + 1.randomFloat }
