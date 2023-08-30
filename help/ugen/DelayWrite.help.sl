(* DelayWrite ; karplus-strong (adc) *)
var freq = 100;
var repeatFreq = 0.3;
var feedbackCoef = 0.997;
var buffer = BufAlloc(1, 48000 * 0.3).BufClear;
var exciter = Decay(Impulse(0.2, 0), 0.02) * PinkNoise();
var filterFreq = MouseY(20000, 2000, 1, 0.2);
var delayedSignal = DelayTap(buffer, 1 / freq);
var filteredSignal = Lpf(delayedSignal, filterFreq);
var mixedSignal = (filteredSignal * feedbackCoef) + exciter;
mixedSignal <! DelayWrite(buffer, mixedSignal)
