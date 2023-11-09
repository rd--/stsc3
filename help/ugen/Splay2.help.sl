(* Splay2 ; texture (Play) *)
{ :tr |
	var k = 7;
	var o = SinOsc({ TrIRand(tr, 40, 90).MidiCps } ! k, 0);
	var a = { TrRand(tr, 0.05, 0.1) } ! k;
	Splay2(o * a)
}.OverlapTexture(6, 3, 3)

(* ---- Splay2 ; texture (Eval) *)
{
	var k = 7;
	var o = SinOsc({ IRand(40, 90).MidiCps } ! k, 0);
	var a = { Rand(0.05, 0.1) } ! k;
	Splay2(o * a)
}.overlap(6, 3, 3)
