(* 20060915 ; rd *)
{
	var np = 12;
	var tr = Dust(0.075);
	var prt = { :d :a |
		{ :cf |
			var rln = { :r :a :b |
				TLine(a + TRand(0, r, tr), b, d, tr)
			};
			var f = [
				cf,
				TRand(cf, cf + 2, tr)
			] + (SinOsc(rln(1, 5, 0.01), 0) * rln(10, 20, 0));
			SinOsc(f, 0) * Decay2(tr, TRand(0.1, 0.2, tr), d) * a
		}
	};
	var fp = { TRand(220, 660, tr) } ! np;
	fp.collect(prt(TRand(4, 7, tr), TRand(0.01, 0.05, tr))).Sum
} !> 5
