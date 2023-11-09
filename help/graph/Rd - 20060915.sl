(* 20060915 ; rd *)
{
	var np = 12;
	var tr = Dust(0.075);
	var prt = { :d :a |
		{ :cf |
			var rln = { :r :a :b |
				TrLine(tr, a + TrRand(tr, 0, r), b, d)
			};
			var f = [
				cf,
				TrRand(tr, cf, cf + 2)
			] + (SinOsc(rln(1, 5, 0.01), 0) * rln(10, 20, 0));
			SinOsc(f, 0) * Decay2(tr, TrRand(tr, 0.1, 0.2), d) * a
		}
	};
	var fp = { TrRand(tr, 220, 660) } ! np;
	fp.collect(prt(TrRand(tr, 4, 7), TrRand(tr, 0.01, 0.05))).sum
} !+ 5
