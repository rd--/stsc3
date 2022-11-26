;; https://sccode.org/1-4Qy ; f0 ; 0239
var b = 0.11 / [1 .. 6];
var q = " #SuperCollider ".ascii;
var o = LFTri(DmdFor(b, 0, Dseq(inf, q.midiCps)), 0);
Splay2(CombC(o, 4, LFTri(b / 9, 0) % LFTri(b, 0) * 4 % 4, 5) / 6).tanh
