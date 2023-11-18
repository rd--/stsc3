(* https://sccode.org/1-4Qy ; f0 ; 0001 *)
(0 .. 23).collectTexture { :each |
	var p = SinOsc(each % 9 // 3 * 100 + (each % 9) + 500, 0);
	Release(EqPan(SinOsc(each + 1, p) * 0.03, 1.Rand2), 0, 2, 25)
} { 2 }
