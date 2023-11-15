(* https://twitter.com/redFrik/status/1125557217086857216 *)
var scl = [0 2.94 4.98 7.02 9.96];
var b = (-7 .. 6) + 0.7 * 2 / 666;
var m = DegreeToKey(
	scl.asLocalBuf,
	LfTri(b, b) * LfTri(b, 0) * 9 + 9 % 32,
	12
) + 24;
var e = AmpComp(m, 440, 1 / 3) * LfTri(b, b) * b * 9;
var o = VarSaw(m.MidiCps, 0, LfTri(b, 0) + 1 / 2) * e;
var s = Rlpf(o, Lag2(m, 1 / b % 1) * 3, 1);
AllpassN(s, 0.3, 0.2 - b, 3).Splay.Tanh
