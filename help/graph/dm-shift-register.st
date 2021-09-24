"https://sccode.org/1-590 (dm)"
| shift_register amp pulse rate trans trig octave note chord cf mf sig cmp rvb |
shift_register := [ :length :tr :in |
  | buf count |
  buf := LocalBuf numChannels: 1 numFrames: length.
  count := PulseCount trig: tr reset: 0.
  Mrg
    lhs: (Demand trig: tr reset: 0 demandUGens: (Dbufrd bufnum: buf phase: length iota + count loop: 1) reverse)
    rhs: (Demand trig: tr reset: 0 demandUGens: (Dbufwr bufnum: buf phase: count input: in loop: 1)) ].
amp := 0.1.
pulse := Impulse freq: 1 / 16 phase: 0.
rate := TChoose trig: pulse array: #(3 5 10).
trans := TChoose trig: pulse array: #(0 2 -2 7 -5).
trig := Trig1 in: (CuspL freq: rate kr * 3 a: 1 b: 1.9 xi: 0) dur: 0.001.
octave := Demand trig: (PulseDivider trig: trig div: 4 start: 0) reset: 0 demandUGens: (Drand repeats: Float infinity list: #(12 -12)).
note := Demand trig: trig reset: 0 demandUGens: (Dseq repeats: Float infinity list: #(42 46 51 54 59 63 66) shuffled + trans kr + octave).
chord := shift_register value: 5 value: trig value: note midicps.
cf := Vibrato freq: chord rate: 6 depth: 0.02 delay: 0 onset: 0 rateVariation: 0.04 depthVariation: 0.1 iphase: 0 trig: 0.
mf := chord * ((LFPulse freq: 1 / 8 iphase: 0 width: 0.5) range: 1.01 -> 2.01).
sig := PMOsc carfreq: cf modfreq: mf pmindex: ((Env levels: #(3 3 0) times: #(0 0.2) curves: #(-4 -4)) gen: trig) modphase: 0.
cmp := (sig * (AmpCompA freq: chord root: 0 minAmp: 0.32 rootAmp: 1) * amp) mix.
rvb := GVerb in: (BPF in: cmp freq: 90 midicps rq: 1)
             roomsize: 50 revtime: 8 damping: 0.5 inputbw: 0.5 spread: 15 drylevel: 0 earlyreflevel: 0.7 taillevel: 0.5 maxroomsize: 300.
XFade2 inA: (Mce with: cmp with: cmp) inB: rvb pan: 0.2 level: 1
