(* https://twitter.com/alln4tural/status/1529413845231587328 (an) ; edit (rd) *)
(1 .. 3).collect { :k |
	(1 .. 9).collect { :i |
		var ph = [
			SinOsc({ i * k ^ i / (2 ^ (-2 .. 4)).atRandom } ! 2, 0),
			Decay(Dust(1 / 4 ^ i), SinOsc(0.1, 0) + k + i),
			k * 999
		].product;
		SinOsc(i * k * k, ph)
	}.product
}.Mix
