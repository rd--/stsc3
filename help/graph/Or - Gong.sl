(* https://recarteblog.wordpress.com/2021/05/05/gongfm_sc/ ; or ; texture (rd) *)
var dur = 7;
{ :tr |
	var freq = [67, 70, 74].MidiCps;
	var ratio3 = Rand(tr, 0.34, 1.35);
	var op3 = [
		SinOsc(freq * ratio3, 0),
		freq,
		ratio3,
		Rand(tr, 3.29, 5.06),
		LinSeg(tr, [0, 0.4, 1, 0.3, 1, dur, 0])
	].product;
	var ratio2 = Rand(tr, 0.17, 0.55);
	var op2 = [
		SinOsc(freq * ratio2 + op3, 0),
		freq,
		ratio2,
		Rand(tr, 1.33, 2.00),
		LinSeg(tr, [0, 0, 1, 0.3, 1, dur, 0])
	].product;
	var ratio1 = Rand(tr, 0.49, 1.11);
	var op1 = [
		SinOsc(freq * ratio1 + op2, 0),
		LinSeg(tr, [0, 0.003, 1, 0.3, 1, dur - 0.5, 0])
	].product;
	var sig = op1.Splay2;
	EqBalance2(
		sig.first,
		sig.second,
		Rand(tr, -0.75, 0.75)
	) * 0.7
}.OverlapTexture(dur, 0, 2)
