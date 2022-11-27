;; https://sccode.org/1-4Qy ; f0 ; 0001
(0 .. 23).collectTexture { :i |
	var p = SinOsc(i % 9 // 3 * 100 + (i % 9) + 500, 0);
	Release(Pan2(SinOsc(i + 1, p) * 0.03, Rand(-1, 1), 1), 0, 2, 25)
} { 2.seconds }
