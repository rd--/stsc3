;; https://sccode.org/1-4Qy ; f0 ; 0233 ; requires=kr
var b = 1 / [1, 4, 6, 8, 11];
var c = LfTri(b / 98, 0);
var q = Lseq(inf, Select(LfTri(b / 99, 0) + c * 5, 1 / b + 59).kr).MidiCps;
Splay2(LfTri(DmdFor(b, c, q) + c, 0) / 2)
