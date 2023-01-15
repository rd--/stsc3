;; LfSaw ; phase value = (0, 2), offset to lowest and midpoint ascending
LfSaw(110, 2 * [0.5, 0]) * 0.1

;; LfSaw ; as phasor
(LfSaw(220, 0) * pi + pi).Sin * 0.1

;; LfSaw ; as phase input to sin ; scale using LinLin
LinLin(LfSaw(440, 0), -1, 1, 0, 2 * pi).Sin * 0.1

;; LfSaw ; as phasor
var freq = LfNoise2(3) * 110 + 220;
[(LfSaw(freq, 0) * pi + pi).Sin, SinOsc(freq, 0)] * 0.1

;; LfSaw
LfSaw(500, 1) * 0.05

;; LfSaw ; used as both oscillator and lfo
LfSaw(LfSaw(4, 0) * 400 + 400, 0) * 0.05

;; LfSaw ; output range is bi-polar
var f = [LinLin(LfSaw(0.5, 1), -1, 1, 200, 1600), 200, 1600];
(SinOsc(f, 0) * [0.1, 0.05, 0.05]).sum

;; LfSaw ; mixed with sin, then with distortions
var f = XLn(220, 440, 10);
var o1 = SinOsc(f + [0, 0.7], 0);
var o2 = LfSaw (f + [0, 0.7], 0) * 0.3;
var o3 = o1 + o2;
o3.Distort.Distort.Cubed * 0.5

;; LfSaw ; https://scsynth.org/t/6320/2 (nh) ; requires=voicer
var voiceFunc = { :e |
	var freq = (e.p * 127).MidiCps;
	var auto = SinOsc(e.z * 2, 0).Range(1, 1 + e.y);
	var formatTable = [[400, 1600, 2700], [830, 1200, 4000]].asLocalBuf;
	var formants = BufRd(3, formatTable, e.y * 3, 1, 2).kr  * [1 / auto, auto, auto ** 0.5];
	var phase = LfSaw(freq, 0).Range(0, 1);
	var snd = (phase * formants / freq * 2 * pi).Sin;
	(snd[1] + (snd[2].Sign * 0.25)) * e.z * 2 * e.w
};
Voicer(16, voiceFunc).Splay2
