(* nv 52 https://swiki.hfbk-hamburg.de/MusicTechnology/899 ; requires=*.x (⊗) *)
var x = [4 4.5; 2 3 5 6];
var z = x *.x (x *.x x).allTuples;
var y = (z * 4).concatenation.clump(2) ++ [0];
var f = DmdFor(1 / 5, 0, Dseq(1, y));
GVerb(
	VarSaw(f, 0, 0.9) * LfPulse(5, 0, 0.5).Lag(0.01),
	99, 5, 0.5, 0.5, 15, 1, 0.7, 0.5, 300
).transposed.Mix / 15
