"analogue daze (jmcc) #3"
| pattern f a x g |
pattern := #(55 63 60 63 57 65 62 65).
f := [ :octave :clockRate :pwmrate :fltrate |
  | sequ tr patternCps sq pwm cf |
  tr := Impulse freq: clockRate phase: 0.
  sequ := [ :s | DmdOn trig: tr kr reset: 0 demandUGens: (Seq repeats: Float infinity list: s) ].
  patternCps := (pattern + (12 * octave)) midiCps.
  sq := sequ value: patternCps.
  pwm := SinOsc freq: pwmrate phase: (2 * Float pi) rand mul: 0.4 add: 0.5.
  cf := SinOsc freq: fltrate phase: (2 * Float pi) rand mul: 1400 add: 2000.
  RLPF in: (LFPulse freq: (Lag in: sq lagTime: 0.05) kr iphase: 0 width: pwm kr mul: 0.1) freq: cf kr rq: 1 / 15 ].
a := [ LFNoise0 freq: (LFNoise1 freq: 0.3 mul: 6000 add: 8000) kr ] dup * 0.07.
x := Decay in: (Impulse freq: 2 phase: 0) decayTime: 0.15 mul: a.
g := x + {f valueWithArguments: #(1 8 0.31 0.2). f valueWithArguments: #(0 2 0.13 0.11)}.
(CombN in: g maxdelaytime: 0.375 delaytime: 0.375 decaytime: 5) + g reverse * 0.4.
