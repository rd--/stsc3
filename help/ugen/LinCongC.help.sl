(* LinCongC ; texture *)
{ :tr |
	var freq = SampleRate() / 2;
	var m = IRand(tr, 0, 1000000);
	var a = IRand(tr, 1, 2000);
	var c = IRand(tr, 1, 30000);
	LinCongC(freq, a, c, m, { IRand(tr, 0, m) } ! 2) * 0.1
}.OverlapTexture(1, 2, 4).Mix

(* LinCongC ; rd edit ; https://github.com/lukiss/Losers-Union-SC-Research *)
var p = 9 / 7 ^ (0 .. 16);
var c = { :freq | LinCongC(freq, 1.1, 0.13, 1, 0) };
var d = (c(c(p.arcTan) ^ 2 * 4) ^ 4 * 8).Abs;
var t = c(c(d / p.arcTan).RoundTo(1 / d) ^ 4 * d * 8).Sin;
var f = Rand(t, 0, 64).Ceiling.MidiCps;
SinOsc(f, 0).Splay * 0.1
