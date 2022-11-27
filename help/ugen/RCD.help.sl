;; RCD ; div16
var trig = LFPulse(8, 0, 0.001);
var freqs = (0 .. 7).collect { :i | (i + 1) * 100 };
var rotate = 0;
var div = 1;
var pulses = RCD(trig, rotate, 0, div, 0, 0, 0, 0, 0);
var out = Ringz(pulses, freqs, [2, 1, 0.5, 0.3, 0.2, 0.3, 0.5, 1]) * 0.05;
Splay2(out)

;; RCD ; rotation
var freqs = (0 .. 7).collect { :i | (i + 1) * 100 };
var decays = (0 .. 7).collect { :i | 8 / (i + 1) };
var trig = LFPulse(5, 0, 0.005);
var rotate = LFNoise0(0.3) * 8 + 8;
var reset = 0;
var spread = 1;
var metronome = Ringz(trig, 6000, 0.01) * 0.03;
var pulses = RCD(trig, rotate, reset, 3, spread, 0, 0, 0, 0);
var out = Ringz(pulses, freqs, decays) * 0.05;
Splay2(out * 0.7).tanh + metronome

;; RCD  ; using 'reset'
var clock = LFPulse(8, 0, 0.001);
var freqs = (0 .. 7).collect { :i | (i * 4 + 50).midiCps };
var rotate = 4;
var reset = CoinGate(0.05, clock);
var pulses = RCD(clock, rotate, reset, 0, 0, 0, 0, 0, 0);
var out = Ringz(pulses, freqs, [1, 1, 0.5, 0.2, 0.2, 0.3, 0.5, 1]) * 0.05;
Splay2(out)

;; RCD ;  auto-reset on ...
var freqs = (0 .. 7).collect { :i | (i + 1) * 100 };
var decays = (0 .. 7).collect { :i | 1 / (i + 1) };
var clock = LFPulse(8, 0, 0.001);
var rotate = 7;
var spread = 1;
var len = 23;
var pulses = RCD(clock, rotate, 0, 0, spread, 1, len, 0, 0);
var out = Ringz(pulses, freqs, decays) * 0.05;
Splay2(out)

;; RCD ; ... and off
var freqs = (0 .. 7).collect { :i | (i + 1) * 100 };
var decays = (0 .. 7).collect { :i | 1 / (i + 1) };
var clock = LFPulse(8, 0, 0.001);
var rotate = 7;
var spread = 1;
var pulses = RCD(clock, rotate, 0, 0, spread, 0, 0, 0, 0);
var out = Ringz(pulses, freqs, decays) * 0.05;
Splay2(out)

;; RCD ; gates
var freqs = (0 .. 7).collect { :i | (i * 5 + 50).midiCps };
var amps = [1, 0.5, 0.3, 0.3, 0.3, 0.2, 0.2, 0.2];
var trig = LFPulse(7, 0, 0.01);
var rotate = -2;
var spread = TIRand(0, 1, Impulse(0.13, 0));
var div = TIRand(0, 3, Impulse(0.1, 0));
var pulses = RCD(trig, rotate, 0, div, spread, 0, 0, 0, 1);
var oscs = SinOsc(freqs, 0) * pulses * amps;
var out = Splay2(oscs.rotateLeft(3) * 0.5);
out  + (CombN(out, 2, [2, 1], 3) * 0.3)
