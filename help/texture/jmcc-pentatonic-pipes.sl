;; pentatonic pipes (jmcc) #9 ; mousex on right half of screen causes pulsation ; requires=kr
var n = 5;
var mode = [0, 3, 5, 7, 10].asLocalBuf;
var root = 36 + (0 .. 12).atRandom;
{
	var f = (DegreeToKey(mode, IRand(0, 20), 12) + root).MidiCps;
	var s = Pan2((Resonz(PinkNoise() * 20, f, 0.002) * 4).Distort * 0.2, Rand(-1, 1), 1);
	var z = s * LinXFade2(1, SinOsc(5, 0).max(0), MouseX(0, 1, 0, 0.2) > 0.5).kr; (* pulsing *)
	(1 / 20).coin.ifTrue { root := 36 + (0 .. 12).atRandom }; (* maybe shift *)
	CombN(z, 0.3, 0.3, 8) + z.reversed
}.overlap(10, 0.01, n)
