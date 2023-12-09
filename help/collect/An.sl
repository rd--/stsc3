(* https://twitter.com/alln4tural/status/99846300173991936 ; graph rewrite *)
{ :tr |
	var h = TChoose(tr, [33 38 40]).MidiCps * (2 ^ TChoose(tr, [0 .. 5]));
	{
		SinOsc(TExpRand(h - (h / 256), h + (h / 256), tr), 0) * 0.025
	} !^ 64
}.OverlapTexture(1, 9, 5).Mix

(* https://twitter.com/alln4tural/status/99846300173991936 ; graph rewrite *)
{ :tr |
	var h = TChoose(tr, [33 38 40]).MidiCps * (2 ^ TChoose(tr, [0 .. 4]));
	{
		SinOsc(TExpRand(h - (h / 64), h + (h / 64), tr), 0) * 0.025
	} !^ 8
}.OverlapTexture(1, 9, 40).Mix

(* https://twitter.com/alln4tural/status/1529413845231587328 (an) ; edit (rd) *)
(0 .. 23).collect { :k |
	(0 .. 7).collect { :i |
		var ph = [
			SinOsc({ i * k ^ i / (2 ^ (-2 .. 4)).atRandom } ! 2, 0),
			Decay(Dust(1 / 4 ^ i), SinOsc(0.1, 0) + k + i),
			k * 999
		].product;
		SinOsc(i * k * k, ph)
	}.product
}.Mix
