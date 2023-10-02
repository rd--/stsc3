(* bit-reduction (adc) *)
var f = LfNoise2(8) * 200 + 300;
var nh = LfNoise2(3) * 10 + 20;
var src = Blip(f, nh);
var sr = MouseX(1000, SampleRate() * 0.1, 1, 0.2);
var bitSize = MouseY(1, 24, 1, 0.2);
var downSample = Latch(src, Impulse(sr, 0));
var bitRedux = downSample.RoundTo(0.5 ^ bitSize);
[downSample, bitRedux]
