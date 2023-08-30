(* ring modulate input ; jmcc #5 ; requires=AudioIn *)
var input = AudioIn([1, 2]);
var freq = MouseX(10, 4000, 1, 0.2);
var modulator = SinOsc(freq, [0, 0.5 * pi]);
input * modulator

(* ---- notes.md

_MouseX_ controls ring modulator frequency.
Offset phase of one oscillator by 90 degrees (_pi_ / 2).

*)
