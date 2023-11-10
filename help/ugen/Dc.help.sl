(* Dc *)
Dc(0)

(* Dc ; Silence at left, SinOsc at right *)
[Dc(0), SinOsc(440, 0)] * 0.1

(* Dc ; Silence at first eight channels, random sine tones at next eight *)
{ :tr |
	var p = { Dc(0) } ! 8;
	var q = { SinOsc(Rand(tr, 220, 880), 0) } ! 8;
	(p ++ q) * 0.1
}.OverlapTexture(2, 5, 2)
