(* sweep verb ; jmcc #5 ; requires=AudioIn *)
var s = AudioIn([1, 2]) * 0.01;
var z = DelayC(s.Sum, 0.048, 0.048);
var y = CombC(z, 0.1, LfNoise1({ Rand(0, 0.1) } ! 6) * 0.04 + 0.05, 15).Sum;
4.timesRepeat {
	y := AllpassC(y, 0.050, { Rand(0, 0.05) } ! 2, 1)
};
LeakDc(y, 0.995)

(* ---- notes.md

_z_ is the reverb predelay time.
_y_ is 6 modulated comb delays in parallel followed by 4 allpass delays on each of two channels (8 total).

*)
