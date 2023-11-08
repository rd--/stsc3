(* ---- PlayBuf ; requires=SfAcquire *)
var sf = SfAcquireStereo('piano-c5');
PlayBuf(1, sf, SfRateScale(sf), 1, 0, 1, 0) * 0.5

(* PlayBuf ; normal playback at same speed of recording *)
var sf = SfAcquireMono('floating_1');
PlayBuf(1, sf, 1, 0, 0, 1, 0) * 0.25

(* PlayBuf ; accelerating pitch *)
var sf = SfAcquireMono('floating_1');
var rate = XLn(0.1, 100, 60);
PlayBuf(1, sf, rate, 0, 0, 1, 0) * 0.25

(* PlayBuf ; sine wave control of playback rate ; negative rate plays backwards *)
var sf = SfAcquireMono('floating_1');
var rate = SinOsc(XLn(0.2, 8, 30), 0) * 2 + 0.1;
PlayBuf(1, sf, rate, 0, 0, 1, 0) * 0.25

(* PlayBuf ; modulate the frequency of a SinOsc with a sample (adc) *)
var soundFile = SfAcquireMono('floating_1');
var sampleLoop = PlayBuf(1, soundFile, 0.5, 0, 0, 1, 0);
SinOsc([100, 150] + (sampleLoop * MouseX(0, 1400, 0, 0.1)), 0) * MouseY(0.2, 0.0002, 0, 0.2)

(* PlayBuf ; modulate frequency and amplitude of a SinOsc with a sample (adc) *)
var soundFile = SfAcquireMono('floating_1');
var sampleLoop = PlayBuf(1, soundFile, 0.5, 0, 0, 1, 0);
var ampMod = (sampleLoop * MouseY(0, 1, 0, 0.2)) + MouseY(1, 0, 0, 0.2);
SinOsc([100, 150] + (sampleLoop * MouseX(0, 1400, 0, 0.1)), 0) * ampMod * 0.2

(* PlayBuf ; modulate phase and amplitude of a SinOsc with a sample (adc) *)
var soundFile = SfAcquireMono('floating_1');
var sampleLoop = PlayBuf(1, soundFile, 0.5, 0, 0, 1, 0);
var ampMod = (sampleLoop * MouseY(0, 1, 0, 0.2)) + MouseY(1, 0, 0, 0.2);
SinOsc([100, 149.8], sampleLoop * MouseX(0, 7, 0, 0.1)) * ampMod * 0.2
