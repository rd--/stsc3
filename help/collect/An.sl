(* https://twitter.com/alln4tural/status/99846300173991936 ; graph rewrite *)
{ :tr |
	var h = Choose(tr, [33, 38, 40]).MidiCps * (2 ^ Choose(tr, [0 .. 5]));
	{
		SinOsc(ExpRand(tr, h - (h / 256), h + (h / 256)), 0) * 0.025
	} !^ 64
}.OverlapTexture(1, 9, 5)

(* https://twitter.com/alln4tural/status/99846300173991936 ; graph rewrite *)
{ :tr |
	var h = Choose(tr, [33, 38, 40]).MidiCps * (2 ^ Choose(tr, [0 .. 4]));
	{
		SinOsc(ExpRand(tr, h - (h / 64), h + (h / 64)), 0) * 0.025
	} !^ 8
}.OverlapTexture(1, 9, 40)

(* https://twitter.com/alln4tural/status/1529413845231587328 (an) ; edit (rd) *)
0...23.collect { :k |
	0...7.collect { :i |
		var ph = [
			SinOsc({ i * k ^ i / (2 ^ -2...4).atRandom } ! 2, 0),
			Decay(Dust(1 / 4 ^ i), SinOsc(0.1, 0) + k + i),
			k * 999
		].product;
		SinOsc(i * k * k, ph)
	}.product
}.sum
