;; https://sccode.org/1-4Qy ; f0 ; 0341
var e = LFTri(2 ** LFTri(1 / 5, 0), 0).roundTo(LFTri(1 / 8, 0) / 3);
var o = SinOsc(e ** [99, 150], BPF(e % 1, 500, 1)) / 6;
GVerb(HPF(o, 9), 99, 5, 0.1, 0.5, 15, 1, 0.7, 0.5, 300).sum
