;; pentatonic pipes (jmcc) ; mousex on right half of screen causes pulsation ; requires=kr
var n = 5;
var mode = [0, 3, 5, 7, 10].asLocalBuf;
var root = 36 + 12.randomInteger;
var z = OverlapTexture({ :tr |
	var f = (DegreeToKey(mode, TRand(0, 20, tr), 12) + root).MidiCps;
	Pan2((Resonz(PinkNoise() * 20, f, 0.002) * 4).Distort * 0.2, TRand(-1, 1, tr), 1);
}, 10, 0.1, n);
z := z * LinXFade2(1, SinOsc(5, 0).max(0), MouseX(0, 1, 0, 0.2) > 0.5).kr;
CombN(z, 0.3, 0.3, 8) + z.reversed
