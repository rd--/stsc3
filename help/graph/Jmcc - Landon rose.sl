(* modified from a patch by Landon Rose ; jmcc #8 ; graph rewrite *)
var m = [32 43 54 89; 10 34 80 120; 67 88 90 100; 14 23 34 45];
var f = { :i |
	var s = { PinkNoise() * 0.001 } ! 2;
	var e = LinSeg(Impulse(0.125, [0 0.25 0.5 0.75][i]), [0 2 1 2 0]);
	RingzBank(s, m[i].MidiCps, [1], [3]) * e
};
(1 .. 4).collect(f).Mix
