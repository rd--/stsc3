;; LinRand
var minmax = 1;
{
	var freq = LinRand(200, 10000, minmax);
	var dur =  (1 / freq) * 7500;
	FSinOsc(freq, 0) * Ln(0.2, 0, dur)
}.dup(15).Splay2
