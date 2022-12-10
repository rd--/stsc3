;; https://sccode.org/1-4Qy ; f0 ; 0240 ; requires=BLowPass4
var l = [
	32, 114,
	32, 101, 114,
	32, 100, 101, 114,
	32, 105, 100, 101, 114,
	32, 108, 105, 100, 101, 114,
	32, 108, 108, 105, 100, 101, 114,
	32, 111, 108, 108, 105, 100, 101, 114,
	32, 67, 111, 108, 108, 105, 100, 101, 114,
	32, 114, 67, 111, 108, 108, 105, 100, 101, 114,
	32, 101, 114, 67, 111, 108, 108, 105, 100, 101, 114,
	32, 112, 101, 114, 67, 111, 108, 108, 105, 100, 101, 114,
	32, 117, 112, 101, 114, 67, 111, 108, 108, 105, 100, 101, 114,
	32, 83, 117, 112, 101, 114, 67, 111, 108, 108, 105, 100, 101, 114,
	32, 35, 83, 117, 112, 101, 114, 67, 111, 108, 108, 105, 100, 101, 114, 32,
	32, 35, 83, 117, 112, 101, 114, 67, 111, 108, 108, 105, 100, 101, 114,
	32, 35, 83, 117, 112, 101, 114, 67, 111, 108, 108, 105, 100, 101, 114,
	32, 83, 117, 112, 101, 114, 67, 111, 108, 108, 105, 100, 101, 114,
	32, 117, 112, 101, 114, 67, 111, 108, 108, 105, 100, 101, 114,
	32, 112, 101, 114, 67, 111, 108, 108, 105, 100, 101, 114,
	32, 101, 114, 67, 111, 108, 108, 105, 100, 101, 114,
	32, 114, 67, 111, 108, 108, 105, 100, 101, 114,
	32, 67, 111, 108, 108, 105, 100, 101, 114,
	32, 111, 108, 108, 105, 100, 101, 114,
	32, 108, 108, 105, 100, 101, 114,
	32, 108, 105, 100, 101, 114,
	32, 105, 100, 101, 114,
	32, 100, 101, 114,
	32, 101, 114,
	32, 114,
	32,
	32
];
var d = Pulse(DmdFor(1 / [10, 4], 0, Seq(99, l.MidiCps)).mean, 0.5);
Pan2(CombN(BLowPass(BLowPass(d, 1200, 1), 1200, 1), 1, 1, 8) / 2, d, 1) / 2

(*
' #SuperCollider '.ascii.pyramid(6) = [32, 114, 32, 101, 114, 32, 100, 101, 114, 32, 105, 100, 101, 114, 32, 108, 105, 100, 101, 114, 32, 108, 108, 105, 100, 101, 114, 32, 111, 108, 108, 105, 100, 101, 114, 32, 67, 111, 108, 108, 105, 100, 101, 114, 32, 114, 67, 111, 108, 108, 105, 100, 101, 114, 32, 101, 114, 67, 111, 108, 108, 105, 100, 101, 114, 32, 112, 101, 114, 67, 111, 108, 108, 105, 100, 101, 114, 32, 117, 112, 101, 114, 67, 111, 108, 108, 105, 100, 101, 114, 32, 83, 117, 112, 101, 114, 67, 111, 108, 108, 105, 100, 101, 114, 32, 35, 83, 117, 112, 101, 114, 67, 111, 108, 108, 105, 100, 101, 114, 32, 32, 35, 83, 117, 112, 101, 114, 67, 111, 108, 108, 105, 100, 101, 114, 32, 35, 83, 117, 112, 101, 114, 67, 111, 108, 108, 105, 100, 101, 114, 32, 83, 117, 112, 101, 114, 67, 111, 108, 108, 105, 100, 101, 114, 32, 117, 112, 101, 114, 67, 111, 108, 108, 105, 100, 101, 114, 32, 112, 101, 114, 67, 111, 108, 108, 105, 100, 101, 114, 32, 101, 114, 67, 111, 108, 108, 105, 100, 101, 114, 32, 114, 67, 111, 108, 108, 105, 100, 101, 114, 32, 67, 111, 108, 108, 105, 100, 101, 114, 32, 111, 108, 108, 105, 100, 101, 114, 32, 108, 108, 105, 100, 101, 114, 32, 108, 105, 100, 101, 114, 32, 105, 100, 101, 114, 32, 100, 101, 114, 32, 101, 114, 32, 114, 32, 32]
*)
