(* https://twitter.com/HernaniVillase/status/517517924236795905 ; rewrite (rd) *)
{ :tr |
	var f = Rand(tr, 97, 101);
	var o = LfTri(
		{ Rand(tr, 0, 200) + 216 } ! 8 + f,
		{ Rand(tr, 0, pi * 2) } ! 8
	);
	var e = LfTri(
		{ Rand(tr, 0, 0.01) + 0.1 } ! 8,
		0
	) * 0.1 + 0.01;
	(o * e).Splay2
}.OverlapTexture(3, 5, 3).Mix
