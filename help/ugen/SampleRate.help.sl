(* SampleRate ; compare sine tone derived from sample rate with a 440hz tone (48000 * 0.01 = 480, 44100 * 0.01 = 441, &etc) *)
var freq = [SampleRate() * 0.01, 440];
SinOsc(freq, 0) * 0.1
