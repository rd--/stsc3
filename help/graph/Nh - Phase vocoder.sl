(* phase vocoder (nh) ; https://scsynth.org/t/old-school-vocoders/5198/6 ; warning: AudioIn ; requires=kr *)
var src = AudioIn([1]);
var notes = { Choose(Dust(0.5).kr, 60 + [-9, -7, -5, -3, -2, 0, 2, 3, 5, 7, 9, 10]) } ! 3;
var numBands = 32;
var bandFreqs = LinExp([0 .. numBands - 1], 0, numBands - 1, 100, 8000);
var voicedCarrier = Saw(notes.MidiCps);
var isVoiced = Lag(Lag(ZeroCrossing(src).CpsMidi, 0.05) > 5000.CpsMidi, 0.05);
var carrier = SelectX(isVoiced, [voicedCarrier, PinkNoise()]);
var filterQ = TRand(10, 100, Dust(0.5));
var srcAmp = Amplitude(Bpf(src, bandFreqs, 1 / filterQ), 0.01, 0.05);
var snd = Bpf(carrier, bandFreqs, 0.05) * srcAmp;
EqPan2(snd.Sum, 0) * numBands / 4
