(* tw 0120 (f0) ; requires=kr *)
var z = LfTri(1 / [7, 8], 0) * LfTri(1 / 9, 0) * 99;
var l = (60 .. 79).MidiCps;
var f = Select(z, l);
var w = LfTri(1 / [3, 4], 0) % 1;
var o = VarSaw(f.kr, 0, w.kr);
CombN(o, 1, 1 / [5, 6], 8) / 8
