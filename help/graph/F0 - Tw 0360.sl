(* https://sccode.org/1-4Qy ; f0 ; 0360 *)
var t = SinOsc([7.992, 8], 0);
var e = Perc(t, 0.005, 0.2, -4);
var f = [1, 2].collect { :i |
	var q = [
		Dseq(240, [64, 66, 71, 73, 74, 66, 64, 73, 71, 66, 74, 73]),
		Dseq(120, [64, 76, 69, 71, 74, 76, 69, 71]),
		Dseq(60, [69, 71, 74, 76])
	];
	Demand(t[i], 0, Dseq(1, q)).MidiCps
};
var x = SinOsc(f, SinOsc(1 / [12, 9], 0) / 4 + 1 * SinOsc(f, e * pi)) * e / 2;
FreeVerb2(x[1], x[2], 0.1, 1, 0)

(* ---- ; calculations
'@BGIJB@IGBJI'.ascii == [64, 66, 71, 73, 74, 66, 64, 73, 71, 66, 74, 73]
'@LEGJLEG'.ascii == [64, 76, 69, 71, 74, 76, 69, 71]
'EGJL'.ascii == [69, 71, 74, 76]
*)
