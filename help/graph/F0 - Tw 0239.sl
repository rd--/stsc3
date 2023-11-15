(* https://sccode.org/1-4Qy ; f0 ; 0239 *)
var b = 0.11 / (1 .. 6);
var q = [32 35 83 117 112 101 114 67 111 108 108 105 100 101 114 32];
var o = LfTri(DmdFor(b, 0, Dseq(inf, q.MidiCps)), 0);
Splay(CombC(o, 4, LfTri(b / 9, 0) % LfTri(b, 0) * 4 % 4, 5) / 6).Tanh

(* ---- ; calculations
' #SuperCollider '.ascii = [32 35 83 117 112 101 114 67 111 108 108 105 100 101 114 32]
*)
