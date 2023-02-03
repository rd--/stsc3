;; https://sccode.org/1-4Qy ; f0 ; 0112
(1 .. 37).collectTexture { :i |
	{
		var lvl = Lseq(1, [12 .. 0] * (i % 63 + 99));
		var saw = VarSaw(DmdFor(1 / 12, 0, lvl) * [1, 1.01], 0, i / 9 % 9 / 9) / 9;
		Release(saw, 0.02, 1, 0.02);
	} !+ 2
} { 1 / 3 }
