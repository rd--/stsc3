(* https://recarteblog.wordpress.com/2021/05/05/gongfm_sc/ ; or ; texture (rd) *)
var dur = 7;
{ :tr |
	var freq = [67, 70, 74].MidiCps;
	var ratio3 = TRand(0.34, 1.35, tr);
	var op3 = [
		SinOsc(freq * ratio3, 0),
		freq,
		ratio3,
		TRand(3.29, 5.06, tr),
		LinSeg(tr, [0, 0.4, 1, 0.3, 1, dur, 0])
	].product;
	var ratio2 = TRand(0.17, 0.55, tr);
	var op2 = [
		SinOsc(freq * ratio2 + op3, 0),
		freq,
		ratio2,
		TRand(1.33, 2.00, tr),
		LinSeg(tr, [0, 0, 1, 0.3, 1, dur, 0])
	].product;
	var ratio1 = TRand(0.49, 1.11, tr);
	var op1 = [
		SinOsc(freq * ratio1 + op2, 0),
		LinSeg(tr, [0, 0.003, 1, 0.3, 1, dur - 0.5, 0])
	].product;
	var sig = op1.Splay2;
	EqBalance2(
		sig.first,
		sig.second,
		TRand(-0.75, 0.75, tr)
	) * 0.7
}.OverlapTexture(dur, 0, 2).Mix
