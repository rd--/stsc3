;; analog bubbles ; jmcc
var f = MulAdd(LfSaw(0.4, 0), 24, MulAdd(LfSaw([8, 7.23], 0), 3, 80)).MidiCps; (* glissando function *)
CombN(SinOsc(f, 0) * 0.04, 0.2, 0.2, 4) (* echoing sine wave *)

;; analog bubbles (jmcc) #1
var o = LfSaw([8, 7.23], 0) * 3 + 80;
var m = LfSaw(0.4, 0) * 24 + o;
CombN(SinOsc(m.MidiCps, 0) * 0.04, 0.2, 0.2, 4)

;; analog bubbles (jmcc) #1 ; keywords
var o = LfSaw(freq: [8, 7.23], iphase: 0) * 3 + 80;
var m = LfSaw(freq: 0.4, iphase: 0) * 24 + o; (* glissando function *)
var s = SinOsc(freq: m.MidiCps, phase: 0) * 0.04;
CombN(in: s, maxdelaytime: 0.2, delaytime: 0.2, decaytime: 4) * 0.1 (* echoing sine wave *)

;; analog bubbles (jmcc) ; applicative order
CombN(MulAdd(SinOsc(MulAdd(LfSaw(0.4, 0), 24, MulAdd(LfSaw([8, 7.23], 0), 3, 80)).MidiCps, 0), 0.04, 0), 0.2, 0.2, 4)

;; analog bubbles (jmcc) #1 ; left-to-right
0.4.LfSaw(0).Mul(24).Add([8, 7.23].LfSaw(0).MulAdd(3, 80)).MidiCps.SinOsc(0).Mul(0.04).CombN(0.2, 0.2, 4)

;; analog bubbles (jmcc) #1 ; left-to-right
var o = LfSaw([8, 7.23], 0).MulAdd(3, 80);
var m = LfSaw(0.4, 0).MulAdd(24, o);
SinOsc(m.MidiCps, 0).Mul(0.04).CombN(0.2, 0.2, 4)

//---- analog bubbles (jmcc) ; the .stc initializer definition form does not require var syntax qualifiers
o = LfSaw([8, 7.23], 0) * 3 + 80;
m = LfSaw(0.4, 0) * 24 + o;
s = SinOsc(m.MidiCps, 0) * 0.04;
CombN(s, 0.2, 0.2, 4)
