(* http://earslap.com/article/combination-tones-and-the-nonlinearities-of-the-human-ear.html *)
var times = Dseq(1, (1 ! 12 ++ [1.5 0.5 2]) !++ 2 / 2);
var pitchBase = [55 55 56 58 58 56 55 53 51 51 53 55];
var pitches = Dseq(1, (pitchBase ++ [55 53 53] ++ pitchBase ++ [53 51 51]).MidiCps);
var freqs = DmdFor(times, 0, pitches);
var baseRandFreq = TRand(1000, 2000, Impulse(32, 0)).Lag(0.01);
SinOsc([baseRandFreq, baseRandFreq + freqs], 0).mean
