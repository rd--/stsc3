;; https://sccode.org/1-4Qy ; f0 ; 0274 ; requires=pyramid
var b = 1 / [1, 8, 2];
var q = ' #SuperCollider '.ascii.asArray.pyramid(1);
var f = DmdFor(b, 0, Dseq(inf, q * 9 ** 1.0595 / 2));
Splay2(SinOsc(f, SinOsc(b, 0) * Lag(SinOsc(0 - b, 0) < 0.9, 0.1) * 5000)) / 3

(* ---- ; calculations

' #SuperCollider '.ascii.asArray.pyramid(1) = [
	32,
	32, 35,
	32, 35, 83,
	32, 35, 83, 117,
	32, 35, 83, 117, 112,
	32, 35, 83, 117, 112, 101,
	32, 35, 83, 117, 112, 101, 114,
	32, 35, 83, 117, 112, 101, 114, 67,
	32, 35, 83, 117, 112, 101, 114, 67, 111,
	32, 35, 83, 117, 112, 101, 114, 67, 111, 108,
	32, 35, 83, 117, 112, 101, 114, 67, 111, 108, 108,
	32, 35, 83, 117, 112, 101, 114, 67, 111, 108, 108, 105,
	32, 35, 83, 117, 112, 101, 114, 67, 111, 108, 108, 105, 100,
	32, 35, 83, 117, 112, 101, 114, 67, 111, 108, 108, 105, 100, 101,
	32, 35, 83, 117, 112, 101, 114, 67, 111, 108, 108, 105, 100, 101, 114,
	32, 35, 83, 117, 112, 101, 114, 67, 111, 108, 108, 105, 100, 101, 114, 32
]

*)
