"analogue daze (jmcc) #3 ; semiautotranslate"
|pattern f a x g z e|
pattern := #(55 63 60 63 57 65 62 65) .
f := [ :param |
  |sequ tr patternCps sq pwm cf octave clockRate pwmrate fltrate|
  octave := param at: 1 .
  clockRate := param at: 2 .
  pwmrate := param at: 3 .
  fltrate := param at: 4 .
  sequ := [ :s :tr | (Demand trig: tr reset: 0 demandUGens: (Dseq repeats: #dinf list: s)) kr ] .
  tr := Impulse kr: {clockRate. 0} .
  patternCps := (pattern + (12 * octave)) mce midicps .
  sq := sequ value: patternCps value: tr .
  pwm := (SinOsc kr: {pwmrate . (2 * (Float pi)) rand}) * 0.4 + 0.5 .
  cf := (SinOsc kr: {fltrate . (2 * (Float pi)) rand}) * 1400 + 2000 .
  RLPF ar: {(LFPulse ar: {Lag kr: {sq . 0.05} . 0 . pwm}) * 0.1 . cf . 1 / 15} ] .
a := (2 mceFill: [:i | LFNoise0 ar: {(LFNoise1 kr: #(0.3)) * 6000 + 8000}]) * 0.07 .
x := (Decay ar: {Impulse ar: #(2 0) . 0.15}) * a .
g := x + {f value: #(1 8 0.31 0.2) . f value: #(0 2 0.13 0.11)} .
z := ((CombN ar: {g . 0.375 . 0.375 . 5}) + g "mceReverse") * 0.4 .
e := EnvLinen attackTime: 2 sustainTime: 56 releaseTime: 2 level: 1 curves: #(#lin #lin #lin) .
z * (e envGen: 1 doneAction: 2)
