"modal space (jmcc) #8"
| b x k c n |
b := #(0 2 3.2 5 7 9 10) asLocalBuf.
x := MouseX minval: 0.0 maxval: 15.0 warp: 0.0 lag: 0.2.
k := DegreeToKey bufnum: b in: x octave: 12.
n := LFNoise1 freq: #(3 3).
c := [ :r |
 | o t f d m |
 o := SinOsc freq: (k + r + (n * 0.04)) midiCps kr phase: 0 mul: 0.1.
 t := LFPulse freq: {48 midiCps. 55 midiCps} iphase: 0 width: 0.15.
 f := (SinOsc freq: 0.1 phase: 0 mul: 10 add: r) midiCps.
 d := RLPF in: t freq: f rq: 0.1 mul: 0.1.
 m := o + d.
 (CombN in: m maxdelaytime: 0.31 delaytime: 0.31 decaytime: 2) + m ].
(c value: 48) + (c value: 72) * 0.25
