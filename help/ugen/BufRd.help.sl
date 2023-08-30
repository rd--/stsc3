(* BufRd ; requires=kr *)
var numFrames = 2 * 48000;
var buf = BufAlloc(1, numFrames);
var osc = SinOsc(LfNoise1(2).kr * 300 + 400, 0) * 0.1;
var wr = BufWrite(buf, Phasor(0, 1, 0, numFrames, 0), 1, osc);
BufRd(1, buf, Phasor(0, 1, 0, numFrames, 0), 1, 2) <! wr
