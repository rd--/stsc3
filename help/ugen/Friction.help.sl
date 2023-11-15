(* sc-140 ; 15 ; Jason Dixon *)
var x = {
	MulAdd(Clip(LfNoise2(3), 0, 1), 0.02990, 0.00001).RoundTo(0)
} ! 5;
Friction(LfTri(50, 0), x, 0.414, 0.313, x * 30000, 1).Splay
