(* 20060915 ; rd *)
{
	var np = 12;
	var tr = Dust(0.075);
	var prt = { :d :a |
		{ :cf |
			var rln = { :r :a :b |
				Line(tr, a + Rand(tr, 0, r), b, d)
			};
			var f = [
				cf,
				Rand(tr, cf, cf + 2)
			] + (SinOsc(rln(1, 5, 0.01), 0) * rln(10, 20, 0));
			SinOsc(f, 0) * Decay2(tr, Rand(tr, 0.1, 0.2), d) * a
		}
	};
	var fp = { Rand(tr, 220, 660) } ! np;
	fp.collect(prt(Rand(tr, 4, 7), Rand(tr, 0.01, 0.05))).sum
} !+ 5
