(* https://sccode.org/1-4Qy ; f0 ; 0252 *)
(0 .. 23).collectTexture { :i |
	var e = Line(0, pi, 10).Sin;
	var o = SyncSaw([7, 8], ([5, 10, 12, 20, 24] * 10).atWrap(i * 2));
	var f = MoogFf(o, 4000 * e, 2, 0) * e / 3;
	Release(f, 0.02, 10, 0.02)
} { 1 }
