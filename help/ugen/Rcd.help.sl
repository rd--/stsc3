(* Rcd ; div16 *)
var trig = LfPulse(8, 0, 0.001);
var freqs = (1 .. 8) * 100;
var rotate = 0;
var div = 1;
var pulses = Rcd(trig, rotate, 0, div, 0, 0, 0, 0, 0);
var out = Ringz(pulses, freqs, [2, 1, 0.5, 0.3, 0.2, 0.3, 0.5, 1]) * 0.05;
Splay2(out)

(* Rcd ; rotation *)
var freqs = (1 .. 8) * 100;
var decays = 8 / (1 .. 8);
var trig = LfPulse(5, 0, 0.005);
var rotate = LfNoise0(0.3) * 8 + 8;
var reset = 0;
var spread = 1;
var metronome = Ringz(trig, 6000, 0.01) * 0.03;
var pulses = Rcd(trig, rotate, reset, 3, spread, 0, 0, 0, 0);
var out = Ringz(pulses, freqs, decays) * 0.05;
Splay2(out * 0.7).Tanh + metronome

(* Rcd ; using 'reset' *)
var clock = LfPulse(8, 0, 0.001);
var freqs = ((0 .. 7) * 4 + 50).MidiCps;
var rotate = 4;
var reset = CoinGate(0.05, clock);
var pulses = Rcd(clock, rotate, reset, 0, 0, 0, 0, 0, 0);
var out = Ringz(pulses, freqs, [1, 1, 0.5, 0.2, 0.2, 0.3, 0.5, 1]) * 0.05;
Splay2(out)

(* Rcd ; auto-reset on *)
var freqs = (1 .. 8) * 100;
var decays = 1 / (1 .. 8);
var clock = LfPulse(8, 0, 0.001);
var rotate = 7;
var spread = 1;
var len = 23;
var pulses = Rcd(clock, rotate, 0, 0, spread, 1, len, 0, 0);
var out = Ringz(pulses, freqs, decays) * 0.05;
Splay2(out)

(* Rcd ; auto-reset off *)
var freqs = (1 .. 8) * 100;
var decays = 1 / (1 .. 8);
var clock = LfPulse(8, 0, 0.001);
var rotate = 7;
var spread = 1;
var pulses = Rcd(clock, rotate, 0, 0, spread, 0, 0, 0, 0);
var out = Ringz(pulses, freqs, decays) * 0.05;
Splay2(out)

(* Rcd ; gates *)
var freqs = ((0 .. 7) * 5 + 50).MidiCps;
var amps = [10 5 3 3 3 2 2 2] / 10;
var trig = LfPulse(7, 0, 0.01);
var rotate = -2;
var spread = TiRand(0, 1, Impulse(0.13, 0));
var div = TiRand(0, 3, Impulse(0.1, 0));
var pulses = Rcd(trig, rotate, 0, div, spread, 0, 0, 0, 1);
var oscs = SinOsc(freqs, 0) * pulses * amps;
var out = Splay2(oscs.rotatedLeft(3) * 0.5);
out + (CombN(out, 2, [2, 1], 3) * 0.3)
