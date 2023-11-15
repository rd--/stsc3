(* https://sccode.org/1-4Qy ; f0 ; 0279 *)
var b = [1 3 6 8 10];
var m = DegreeToKey(b.asLocalBuf, LfSaw(LfSaw(1 / b / 99, 0) / pi, 0) * 12 + 18 + b, 12);
var f = CombN(m.MidiCps, 0.2, 0.2, 1);
LfSaw(f, 0).Splay * 0.6
