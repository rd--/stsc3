;; sc-140 ; 03 ; Tim Walters ; sc140_sourcecode.txt
var f = { :k |
	(0 .. 7).collect({ :i |
		var  e = Decay(Dust(1 / 4 ** i), SinOsc(0.1, 0) + 1 * k + i) * k * 999;
		var ph = SinOsc(i * k ** i / [4, 5], 0) * e;
		SinOsc(i * k * k, ph)
	}).product
};
(0 .. 15).collect(f).sum