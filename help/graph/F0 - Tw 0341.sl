(* https://sccode.org/1-4Qy ; f0 ; 0341 *)
var e = LfTri(2 ^ LfTri(1 / 5, 0), 0).RoundTo(LfTri(1 / 8, 0) / 3);
var o = SinOsc(e ^ [99, 150], Bpf(e % 1, 500, 1)) / 6;
GVerb(Hpf(o, 9), 99, 5, 0.1, 0.5, 15, 1, 0.7, 0.5, 300).Mix
