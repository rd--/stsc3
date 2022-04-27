!Sc3HelpGraph methodsFor: 'Sc Help Graphs'!
adcBitReduction
  "bit-reduction (adc)"
  "
  Sc3HelpGraph new adcBitReduction play.
  Sc3 reset.
  "
  | f nh src sr bitSz downSample bitRedux |
  f := LFNoise2 freq: 8 mul: 200 add: 300.
  nh := LFNoise2 freq: 3 mul: 10 add: 20.
  src := Blip freq: f kr numharm: nh kr.
  sr := MouseX minval: 1000 maxval: SampleRate new * 0.1 warp: 1 lag: 0.2.
  bitSz := MouseY minval: 1 maxval: 24 warp: 1 lag: 0.2.
  downSample := Latch in: src trig: (Impulse freq: sr phase: 0).
  bitRedux := downSample roundTo: 0.5 ** bitSz.
  ^Mce with: downSample with: bitRedux
!
adcDownSample
  "down-sample (adc)"
  "
  Sc3HelpGraph new adcDownSample play.
  Sc3 reset.
  "
  |f nh src sr|
  f := LFNoise2 freq: 8 mul: 200 add: 300.
  nh := LFNoise2 freq: 3 mul: 10 add: 20.
  src := Blip freq: f kr numharm: nh kr.
  sr := MouseX minval: 1000 maxval: SampleRate new * 0.1 warp: 1 lag: 0.2.
  ^Latch in: src trig: (Impulse freq: sr phase: 0)
!
dmShiftRegister
  "https://sccode.org/1-590 (dm)"
  "
  Sc3HelpGraph new dmShiftRegister play.
  Sc3 reset.
  "
  | shift_register amp pulse rate trans trig octave note chord cf mf sig cmp rvb |
  shift_register := [ :length :tr :in |
    | buf count |
    buf := BufAlloc numChannels: 1 numFrames: length.
    count := PulseCount trig: tr reset: 0.
    Mrg
      lhs: (DmdOn trig: tr reset: 0 demandUGens: (Dbufrd bufnum: buf phase: length iota + count loop: 1) reverse)
      rhs: (DmdOn trig: tr reset: 0 demandUGens: (Dbufwr bufnum: buf phase: count input: in loop: 1)) ].
  amp := 0.1.
  pulse := Impulse freq: 1 / 16 phase: 0.
  rate := TChoose trig: pulse array: #(3 5 10).
  trans := TChoose trig: pulse array: #(0 2 -2 7 -5).
  trig := Trig1 in: (CuspL freq: rate kr * 3 a: 1 b: 1.9 xi: 0) dur: 0.001.
  octave := DmdOn trig: (PulseDivider trig: trig div: 4 start: 0) reset: 0 demandUGens: (Choose repeats: Float infinity list: #(12 -12)).
  note := DmdOn trig: trig reset: 0 demandUGens: (Seq repeats: Float infinity list: #(42 46 51 54 59 63 66) shuffled + trans kr + octave).
  chord := shift_register value: 5 value: trig value: note midiCps.
  cf := Vibrato freq: chord rate: 6 depth: 0.02 delay: 0 onset: 0 rateVariation: 0.04 depthVariation: 0.1 iphase: 0 trig: 0.
  mf := chord * ((LFPulse freq: 1 / 8 iphase: 0 width: 0.5) range: 1.01 -> 2.01).
  sig := PMOsc carfreq: cf modfreq: mf pmindex: ((EnvSpec levels: #(3 3 0) times: #(0 0.2) curves: #(-4 -4)) asEnvGen: trig) modphase: 0.
  cmp := (sig * (AmpCompA freq: chord root: 0 minAmp: 0.32 rootAmp: 1) * amp) mix.
  rvb := GVerb in: (BPF in: cmp freq: 90 midiCps rq: 1)
               roomsize: 50 revtime: 8 damping: 0.5 inputbw: 0.5 spread: 15 drylevel: 0 earlyreflevel: 0.7 taillevel: 0.5 maxroomsize: 300.
  ^XFade2 inA: (Mce with: cmp with: cmp) inB: rvb pan: 0.2 level: 1
!
esTw435684664200540161
  "https://twitter.com/sluyterrific_sc/status/435684664200540161 (es)"
  "
  Sc3HelpGraph new esTw435684664200540161 play.
  Sc3 reset.
  "
  | f |
  f := [ :i |
    | m |
    m := i % (LFNoise2 freq: 0.01 mul: 50 add: 50) + (LFNoise2 freq: 0.1 mul: 10 add: 40).
    SinOsc freq: m midiCps phase: 0 mul: (LFNoise2 freq: 1 mul: 0.01 add: 0.01) ].
  ^((1 to: 99) collect: f) splay2
!
esTw570012853274615808
  "https://twitter.com/sluyterrific_sc/status/570012853274615808 (es)"
  "
  Sc3HelpGraph new esTw570012853274615808 play.
  Sc3 reset.
  "
  | t o a s |
  t := Dust2 density: #(9 9).
  o := 0.1.
  a := LFNoise2 freq: o mul: 9 add: 9.
  s := VarSaw freq: (TExpRand lo: 9 hi: 5000 trig: t) iphase: 0 width: 0.5 mul: (Decay in: t decayTime: 1).
  ^GreyholeRaw in1: (s at: 1) in2: (s at: 2) damping: 0 delaytime: a diffusion: (LFNoise2 freq: 1) feedback: (LFNoise2 freq: 1 mul: 1 add: 1) moddepth: 2 modfreq: a size: o mul: 0.2
!
f0Pkt00
  "pkt 00 (f0) ; http://www.fredrikolofsson.com/f0blog/?q=node/490"
  "
  Sc3HelpGraph new f0Pkt00 play.
  Sc3 reset.
  "
  | f0 ph0 a0 i w s |
  f0 := SinOsc freq: #(0.11 0.22 0.33) phase: 0 mul: 1 add: #(100 200 300).
  ph0 := SinOsc freq: #(0.1 0.2 0.3) phase: 0 mul: 2 * Float pi.
  a0 := SinOsc freq: #(0.01 0.02 0.03) phase: 0 mul: 0.05 add: 0.1.
  i := SinOsc freq: f0 phase: ph0 mul: a0.
  w := SinOsc freq: (SinOsc freq: (SinOsc freq: 0.13 phase: 0 mul: 5 add: 6) phase: 0 mul: 8 add: 50) phase: 0 mul: 1 add: 1.
  s := Splay input: i spread: w level: 0.7 center: (SinOsc freq: 1.2 phase: 0 mul: 0.6) levelComp: true.
  ^GVerb in: s roomsize: 20 revtime: 5 damping: 1 inputbw: 0.5 spread: 25 drylevel: 0 earlyreflevel: 1 taillevel: 1 maxroomsize: 300
!
f0Tw0011
  "tw 0011 (f0) ; https://twitter.com/redFrik/status/23182604046"
  "
  Sc3HelpGraph new f0Tw0011 play.
  Sc3 reset.
  "
  | s f |
  s := [ :o :i |
    SinOsc freq: (i + #(0 0.0001) ** 2) * (f value: o value: i - 1)
           phase: (f value: o value: (i - 1)) * 0.0001
           mul: (f value: o value: i - 1)].
  f := [ :o :i | i > 0 ifTrue: [ s value: o value: i ] ifFalse: [ o ] ].
  ^(f value: 60 value: 6) / 60
!
f0Tw0041
  "http://www.fredrikolofsson.com/f0blog/?q=node/537 (f0)"
  "
  Sc3HelpGraph new f0Tw0041 play.
  Sc3 reset.
  "
  | s i o |
  s := Sweep trig: (LocalIn numChan: 6 default: 0) rate: 1.
  i := Impulse freq: #(1 0.749 6 12 3 4) phase: 0.
  o := SinOsc freq: (RunningMax in: s trig: i) reciprocal phase: 0.
  ^Mrg lhs: o splay2 tanh * 0.05 rhs: (LocalOut channelsArray: o)
!
f0Tw0045
  "http://www.fredrikolofsson.com/f0blog/?q=node/537 (f0)"
  "
  Sc3HelpGraph new f0Tw0045 play.
  Sc3 reset.
  "
  | f pf p |
  f := SinOsc freq: (SinOsc freq: 0.11 phase: 0) phase: 0.
  pf := SinOsc freq: ((SinOsc freq: 0.01 phase: 0) + 1) * 95 phase: 0 mul: (SinOsc freq: 0.005 phase: 0 mul: 50) add: 100.
  p := SinOsc freq: pf phase: (SinOsc freq: #(98 97) phase: 0) mul: (SinOsc freq: 0.0005 phase: 0) + Float pi.
  ^(SinOsc freq: f phase: p) tanh * 0.1
!
f0Tw0077
  "http://sccode.org/1-4Qy (f0)"
  "
  Sc3HelpGraph new f0Tw0077 play.
  Sc3 reset.
  "
  | f p m b |
  f := (SinOsc freq: {1/5. 1/6} phase: 0) + #(798 912).
  p := (SinOsc freq: (1/16) phase: 0 mul: 19 add: 99) * (SinOsc freq: #(9 8) phase: 0).
  m := SinOsc freq: (SinOsc freq: 6 phase: 0 mul: (SinOsc freq: 0.009 phase: 0)) phase: 0.
  b := SinOsc freq: f phase: p * m.
  ^(SinOsc freq: #(201 301) phase: b) tanh * 0.1
!
f0Tw0084
  "http://sccode.org/1-4Qy (f0)"
  "
  Sc3HelpGraph new f0Tw0084 play.
  Sc3 reset.
  "
  | f g w |
  f := Saw freq: ((5 to: 15 by: 2) * 19) mul: 99 add: 199.
  g := Saw freq: ((1 to: 13 by: 3) * 29) mul: 199 add: 299.
  w := Saw freq: (Saw freq: 3 mul: 2 add: 3) * (5 to: 15 by: 2) * 19 mul: 299 add: 399.
  ^((Formant fundfreq: f formfreq: g bwfreq: w) splay2) / 9
!
f0Tw0121
  "http://sccode.org/1-4Qy (f0)"
  "
  Sc3HelpGraph new f0Tw0121 play.
  Sc3 reset.
  "
  | z l m o |
  z := SinOsc freq: {1/8. 1/7} phase: 0 mul: (SinOsc freq: (1/30) phase: 0 mul: 9).
  l := (56 to: 98 by: 6) collect: [:x | x midiCps].
  m := SinOsc freq: {1/4. 1/3} phase: 0.
  o := SinOsc freq: (Select which: z array: l) kr phase: 0 mul: m.
  ^(CombN in: o maxdelaytime: 1 delaytime: {1/6. 1/5} decaytime: 9) tanh * 0.1
!
f0Tw0125
  "http://sccode.org/1-4Qy (f0)"
  "
  Sc3HelpGraph new f0Tw0125 play.
  Sc3 reset.
  "
  | f u d i p |
  f := SinOsc freq: {1/8. 1/9} phase: 0 mul: 4 add: #(400 202).
  u := (SinOsc freq: (1/9) phase: 0) + 1 / 88.
  d := (SinOsc freq: (1/8) phase: 0) + 1 / 99.
  i := InFb numChan: 1 bus: #(1 0).
  p := CombC in: (LagUD in: i lagTimeU: u lagTimeD: d) maxdelaytime: 1 delaytime: 0.08 decaytime: 9.
  ^SinOsc freq: f phase: p mul: 0.15
!
f0Tw0456384156159574016
  "https://twitter.com/redFrik/status/456384156159574016 (f0)"
  "
  Sc3HelpGraph new f0Tw0456384156159574016 play.
  Sc3 reset.
  "
  | a s f |
  a := 1 / #(3 12 4 1 6 2).
  s := (Lag3 in: (SinOsc freq: a phase: 0) lagTime: (SinOsc freq: (2.67 ** a) phase: 0) abs) * 99.
  f := (SinOsc freq: (1 / a / 9) phase: a) > 0 * 20 + 99 / a.
  ^(SinOsc freq: (HPF in: (Ringz in: s freq: f decaytime: 1) freq: 440) phase: 0 mul: 0.25) splay2
!
f0Tw0839296011982024704
  "f0 ; https://twitter.com/redFrik/status/839296011982024704"
  "
  Sc3HelpGraph new f0Tw0839296011982024704 play.
  Sc3 reset.
  "
  | t f m i g |
  t := LFSaw freq: #(0.5 0.6) iphase: 0.
  f := (LFSaw freq: 5 iphase: 0 mul: (LFSaw freq: 0.015 iphase: 0) add: 1) * 98.
  m := (2 ** ((LFSaw freq: 4 iphase: 0) roundTo: 0.5)) * 99.
  i := (2 ** (LFSaw freq: 1 / #(8 9) iphase: 0)) * 8.
  g := GrainFM numChan: 1 trigger: t dur: 16 carfreq: f modfreq: m index: i pan: 0 envbufnum: -1 maxGrains: 512.
  ^(g / 2) tanh * 0.25
!
f0Tw1105496695637454848
  "f0 ; https://twitter.com/redFrik/status/1105496695637454848"
  "
  Sc3HelpGraph new f0Tw1105496695637454848 play.
  Sc3 reset.
  "
  | b w s x y z m o f rq |
  b := (1 to: 15) / 151.
  w := (VarSaw freq: b iphase: b width: 1 / 5.15 mul: 1 add: 1.5) ceil.
  s := Select which: ((VarSaw freq: 1 / 15 iphase: 1 / 5 width: b mul: 5) ceil + 5 / 5) array: {51 * 1.5. 51. 151}.
  x := Lag in: s lagTime: b.
  y := VarSaw freq: b + 5 iphase: 0 width: 0.5.
  z := VarSaw freq: b iphase: b width: b mul: b / 5 add: #(1.5 5 1).
  m := VarSaw freq: 5 - b iphase: b width: b mul: 5 add: 5.
  o := VarSaw freq: (w * x + y * z) kr iphase: b width: (VarSaw freq: b iphase: 0 width: 0.5) kr + 5 / 15 mul: (m > 1.515) kr.
  f := 1515 ** ((VarSaw freq: 1 - b / 5 iphase: 0 width: 0.5) / 15 + 1 + b).
  rq := (1.5 ** (VarSaw freq: b iphase: 0 width: 0.5)) / 5.
  ^(BLowPass in: o freq: f kr rq: rq kr mul: 1 / 5) splay2
!
f0Tw1125557217086857216
  "https://twitter.com/redFrik/status/1125557217086857216 (f0)"
  "
  Sc3HelpGraph new f0Tw1125557217086857216 play.
  Sc3 reset.
  "
  | scl b i m v o s |
  scl := #(0 2.94 4.98 7.02 9.96). "pythagorean"
  b := (-7 to: 6) + 0.7 * 2 / 666.
  i := (LFTri freq: b iphase: b mul: (LFTri freq: b iphase: 0 mul: 9)) + 9 % 32.
  m := (DegreeToKey bufnum: (scl asLocalBuf) in: i octave: 12) + 24.
  v := VarSaw freq: m midiCps kr iphase: 0 width: (LFTri freq: b iphase: 0) kr + 1 / 2.
  o := v * (AmpComp freq: m root: 440 exp: 1/3) kr * (LFTri freq: b iphase: b) kr * b * 9.
  s := RLPF in: o freq: (Lag2 in: m lagTime: (1 / b % 1)) * 3 rq: 1.
  ^(AllpassN in: s maxdelaytime: 0.3 delaytime: (0.2 - b) decaytime: 3) splay2 tanh
!
f0Tw1210118661022867458
  "f0 ; https://twitter.com/redFrik/status/1210118661022867458"
  "
  Sc3HelpGraph new f0Tw1210118661022867458 play.
  Sc3 reset.
  "
  | b d g l f o c p |
  b := 1 / (2 to: 7).
  d := 48 reciprocal.
  g := 0 cos cos.
  l := #(0 -16 0 -16 0 0 -16 0 -16 0 -16 0 0 0 -16 -16 47 0 -16 0 -16 0 -16 0 0 -16 0 0 -16 0 0 0 0 0 -5 -16 -16 -15).
  f := Duty dur: b reset: 0 doneAction: 0 level: (Seq repeats: Float infinity list: l + 48).
  o := SinOscFB freq: f feedback: ((SinOsc freq: d phase: 0) max: 0) mul: 0 cos sin.
  c := SinOsc freq: 0 phase: Float pi ** o.
  p := PitchShift in: c windowSize: g pitchRatio: 1 / b pitchDispersion: d timeDispersion: d mul: b.
  ^LeakDC in: (c + p) splay2 coef: 0.995 mul: g
!
f0Tw1254441448327479299
  "f0 <https://twitter.com/redFrik/status/1254441448327479299>"
  "
  Sc3HelpGraph new f0Tw1254441448327479299 play.
  Sc3 reset.
  "
  | b e c d m l j f y o |
  b := #(1 3 5 8 10).
  e := {3. 2/3. 4. 3/2. 2}.
  c := 0.021.
  d := (LFTri freq: b / 999 iphase: 0) % 1.
  m := LFTri freq: b * c iphase: 0.
  l := m * 7 + 20 + (Seq repeats: Float infinity list: (b % m * 5 + 6)).
  j := Duty dur: (e / (12 ** m)) reset: 0 doneAction: 0 level: l.
  f := (DegreeToKey bufnum: (b asLocalBuf) in: j octave: 12) midiCps.
  y := Decay2 in: (Impulse freq: {2/3. 1.5. 3. 1.5. 3} phase: 0) attackTime: c decayTime: d mul: d.
  o := SinOscFB freq: f feedback: (((LFTri freq: (c / b) iphase: 0) + 1) / 3) mul: y.
  ^FreeVerb in: o splay2 mix: 0.1 room: 1 damp: 0.5 mul: 0.2
!
f0Tw1319946903711338496
  "https://twitter.com/redFrik/status/1319946903711338496 ; graph rewrite (rd)"
  "
  Sc3HelpGraph new f0Tw1319946903711338496 play.
  Sc3 reset.
  "
  | scl gen |
  scl := #(0 0.804 1.944 3.084 3.888 5.028 5.832 6.972 7.776 8.916 10.056 10.86).
  gen := [ :tr |
    | i z |
    i := PulseCount trig: tr reset: 0.
    z := [
      | f y m |
      m := ((12 * (TRand lo: 3 hi: 6 trig: tr)) + (Select which: (TChoose trig: tr array: #(0 2 3 5 7 8 10)) array: scl)) midiCps.
      f := LFTri freq: 1 / 99 iphase: 0 mul: 9 add: m.
      y := SinOscFB freq: f feedback: ((LFTri freq: (1 + i) / 50 iphase: 0) + 1) / 2.
      Pan2 in: y pos: (LFTri freq: i / 70 iphase: 0) level: ((TRand lo: 0 hi: 1 trig: tr) > 0.5 / 2) ] dup: 9.
    (CombN in: z sum maxdelaytime: 0.2 delaytime: 0.2 decaytime: 1) tanh * 0.1 ].
  ^OverlapTexture graphFunc: gen sustainTime: 3 transitionTime: 9 overlap: 2
!
f0Tw1338987643057688579
  "f0 <https://twitter.com/redFrik/status/1338987643057688579>"
  "
  Sc3HelpGraph new f0Tw1338987643057688579 play.
  Sc3 reset.
  "
  | b x t d c z p |
  b := (8 to: 18) asArray.
  x := VarSaw freq: #(18.1 81.8 1.81) +~ b iphase: 1 width: (VarSaw freq: (b / 81) iphase: 0 width: 0.5).
  t := VarSaw freq: (8 / 1.818) iphase: {1. 8/18} width: 0.5.
  d := ((VarSaw freq: (b / 181.8) iphase: 0 width: 0.5) exprange: (1.81 / 818) -> (1.8 / 181.8)) roundTo: (1.81 / 818).
  c := (VarSaw freq: (1 / 8) iphase: (1 / b) width: 0.5) + 8.18 * 1.8.
  z := (VarSaw freq: (1 / 81.8) iphase: 0 width: 0.5) range: (1.8 ** -1) -> (8.1 / 8.18).
  p := (Pluck in: x trig: t maxdelaytime: (1.8 / 181.8) delaytime: d decaytime: c coef: z) * (VarSaw freq: (1.8 / b) iphase: 0 width: 0.5) / 1.8.
  ^HPF in: p splay2 freq: (18 / 1.81)
!
f0Tw1343283697840226307
  "f0 <https://twitter.com/redFrik/status/1343283697840226307>"
  "
  Sc3HelpGraph new f0Tw1343283697840226307 play.
  Sc3 reset.
  "
  | b f z t d c p |
  b := (2 to: 12) / 12.
  f := #(136.1 163.3 181.4 204.1 244.9 272.1 326.5 362.8 408.2 489.8 544.2).
  z := ([ WhiteNoise new ] dup: 11) * ((LFTri freq: b iphase: 0) + 1 / 2) + BrownNoise new.
  t := LFTri freq: (121 ** (LFTri freq: (2 / 121 * b) iphase: 0)) iphase: 0.
  d := (Lag2 in: ((LFTri freq: (1 / 212.1) iphase: 0) <= (1 / 212)) lagTime: (2 / b)) / 12 * 1.2 + 1.2 / f.
  c := (LFTri freq: (b / 12) iphase: 0) + 1 / 2.
  p := Pluck in: z trig: t maxdelaytime: (2 / 121) delaytime: d decaytime: (1 / 2 * 12 / 1.2) coef: c.
  ^Splay input: (HPF in: p freq: 12) level: 1 / 2.1 / 2
!
f0Tw1395040511795372038
  "f0 ; https://twitter.com/redFrik/status/1395040511795372038"
  "
  Sc3HelpGraph new f0Tw1395040511795372038 play.
  Sc3 reset.
  "
  | b c l f o p m h q r |
  b := #(2 6 3 1).
  c := 500.
  l := Seq repeats: Float infinity list: ((SinOscFB freq: b / 24 feedback: 0) rounded + (0 to: 9) * 7 + 29.17) midiCps.
  f := DmdFor dur: b reset: (SinOscFB freq: b / 9 feedback: 0) level: l.
  o := SinOscFB freq: f feedback: (((SinOscFB freq: b / 13 feedback: 0) max: 0) / 3 * 2).
  p := AllpassN in: o / 8 maxdelaytime: 1 delaytime: 0.51 decaytime: 8.
  m := ((SinOscFB freq: b / c feedback: 0) max: 0) / c.
  h := Hasher in: ((SinOscFB freq: b / 12 feedback: 0) max: 0) mul: m.
  q := BPF in: h freq: (2 ** (SinOscFB freq: b / c * 2 feedback: 0)) * 10000 rq: 1 / 99.
  r := GVerb in: q roomsize: 99 revtime: 9 damping: 0.2 inputbw: 0.5 spread: 15 drylevel: 1 earlyreflevel: 0.7 taillevel: 0.5 maxroomsize: 300.
  ^((HPF in: r + p freq: 50) clump: 2) mix * 0.2
!
f0Tw1395519538008141835
  "f0 ; <https://twitter.com/redFrik/status/1395519538008141835>"
  "
  Sc3HelpGraph new f0Tw1395519538008141835 play.
  Sc3 reset.
  "
  | c d f p o |
  c := DmdFor dur: 0.004 reset: 0 level: (Dseries length: Float infinity start: 1 step: #(1 2)) % (DmdFor dur: 8.192 reset: 0 level: (Seq repeats: Float infinity list: ((1 to: 6) * 75))).
  d := HPF in: (MantissaMask in: c bits: 3) freq: 5.
  f := [ :x | (SinOscFB freq: x feedback: 0) max: 0 ].
  p := #(250 200) * ((Duty dur: 4.096 reset: 0 doneAction: 0 level: (Dseq repeats: Float infinity list: #(4 6 5 5 5 5 3) / 4)) lag: 0.1).
  o := ((SinOscFB freq: p feedback: (f value: 0.08)) * (f value: 1 / #(99 60))) / 3.
  ^(LPF in: ((((d sin + ((SinOscFB freq: 63 feedback: (f value: 0.8)) * d)) / 9) + o) tanh / 2) freq: 10000) * 0.1
!
f0Tw1395878538297892865
  "f0 ; <https://twitter.com/redFrik/status/1395878538297892865>"
  "
  Sc3HelpGraph new f0Tw1395878538297892865 play.
  Sc3 reset.
  "
  | t g e m o c |
  t := Impulse freq: 5 phase: 0.
  g := #(3 2).
  e := (LagUD in: t lagTimeU: 0.001 lagTimeD: ((SinOscFB freq: g / 99 feedback: 0) + 1.08)).
  m := (#(24 0 3 5 7 10 36) collect: [ :i | i + #(36 48 36 33 60 72) ]) concatenation.
  o :=
    SinOscFB
      freq: (DmdOn trig: t reset: 0 demandUGens: (Seq repeats: Float infinity list: m midiCps) / g)
      feedback: (SinOscFB freq: 0.02 feedback: 0)
      mul: e.
  c := (RLPF in: o freq: (((3 ** (SinOscFB freq: 0.04 feedback: 0)) + e) * 2000) rq: ((3 ** (SinOscFB freq: (g / 9) feedback: 0)) / 3)) * 4.
  ^((CombC in: {c tanh. c} maxdelaytime: 1 delaytime: 1 / #(2 3) decaytime: #(4 5)) sum / 8 + c) tanh * 0.1
!
f0Tw1452318302768963589
  "<https://twitter.com/redFrik/status/1452318302768963589> ; f0"
  "
  Sc3HelpGraph new f0Tw1452318302768963589 play.
  Sc3 reset.
  "
  | b m c t f h a z o |
  b := 2 * Float pi * #(4 3 1 2 5) / 5.
  m := 2 ** (SinOsc freq: 1 / 50 phase: b) * 99.
  c := m roundTo: 50.
  t := b / (2 * Float pi) + ((SinOsc freq: b / 50 phase: (SinOsc freq: ((SinOsc freq: 1 / 99 phase: Float pi) max: 0) phase: 0) ** (2 * Float pi)) max: 0).
  f := (SinOsc freq: b / 9 phase: b) / 3 + c.
  h := CombC in: c / 50 maxdelaytime: 2 delaytime: t decaytime: 5.
  a := 50 / m * ((SinOsc freq: b / 50 phase: 0) max: 0) + 0.5.
  z := Blip freq: f numharm: h mul: a.
  o := HPF in: z splay2 * 1.5 freq: 15 tanh.
  ^o + (GVerb in: o sum / 2 * (Ln start: 0 end: 1 dur: 1) roomsize: 50 revtime: 3 damping: 0.5 inputbw: 0.5 spread: 15 drylevel: 1 earlyreflevel: 0.7 taillevel: 0.5 maxroomsize: 300) / 5
!
f0Tw1453520892374441986
  "https://twitter.com/redFrik/status/1453520892374441986 ; f0"
  "
  Sc3HelpGraph new f0Tw1453520892374441986 play.
  Sc3 reset.
  "
  | b c t x f y h o |
  b := #(1 2 3) / 3.
  c := 2 * Float pi * b.
  t := SinOsc freq: 10 phase: Float pi * b.
  x := (SinOsc freq: b phase: 0) > 0 * (SinOsc freq: b * 50 phase: 0) + 1 / 2 * (300 ** (SinOsc freq: 1 / 32 phase: c) + 99) + ((SinOsc freq: 1 / 256 phase: c) rounded * 50 + 99).
  f := Latch in: x trig: t % (SinOsc freq: 99 ** (SinOsc freq: 1 / 64 phase: 0) phase: 0).
  y := (SinOsc freq: (SinOsc freq: 1 / 16 phase: 0) * 3 + 12 phase: 0) + 1 / 2 * ((SinOsc freq: 1 / 8 phase: c) * 50 + 51).
  h := Latch in: y trig: t * (SinOsc freq: 1 / 4 phase: 0).
  o := (Blip freq: f numharm: h) splay2 / 3.
  ^o + (GVerb in: o sum / 3 / 3 roomsize: 30 revtime: 3 damping: 0.5 inputbw: 0.5 spread: 15 drylevel: 1 earlyreflevel: 0.7 taillevel: 0.5 maxroomsize: 300)
!
f0Tw1467507942664646661
  "https://twitter.com/redFrik/status/1467507942664646661"
  "
  Sc3HelpGraph new f0Tw1467507942664646661 play.
  Sc3 reset.
  "
  | b fb1 d c p q |
  b := #(3 4 5 6 7 8 1 2).
  fb1 := [ :freq | FBSineC freq: freq im: 1 fb: 0.1 a: 1.1 c: 0.5 xi: 0.1 yi: 0.1 ].
  d := (fb1 value: b / 12) > 0 / 80 + 1.051.
  c := (fb1 value: 1 / b) + 1.
  p := FBSineC
    freq: 999 * b
    im: 2 ** (fb1 value: b / 9) * 4 - 1
    fb: (fb1 value: b / 8) / 2 + 1 + (99 ** (fb1 value: 1 / 4) / 99)
    a: d
    c: (fb1 value: 1 / b) / 40 + 0.1
    xi: 0.1
    yi: 0.1.
  q := FBSineC freq: (fb1 value: 27 + b) > 0 + 2 + b * d * 1400 im: 2 fb: c a: c / 2 c: 2 xi: 2 yi: 0.1.
  ^(3 ** (fb1 value: 1 / b) / 5 * p + (q / 12) + ((HPF in: (fb1 value: 32) < 0 freq: 3) * (fb1 value: (b * 70 / d)) * (fb1 value: 1 / 2))) splay2 / 2
!
f0Tw1479212835192332289
  "https://twitter.com/redFrik/status/1479212835192332289 (f0)"
  "
  Sc3HelpGraph new f0Tw1479212835192332289 play.
  Sc3 reset.
  "
  | q q1 b q2 e q3 q4 q5 q6 |
  q := [ :freq | QuadC freq: freq a: 1 b: -1 c: -0.75 xi: 0 ].
  q1 := q value: 5.
  b := #(5 2.5 6).
  q2 := (q value: b / 15) * q1.
  e := 1 / 15.
  q3 := (q value: 500) * q1 / 99.
  q4 := (q value: 999 * b) * ((q value: b / 15) max: 0) / 999 * (q value: b).
  q5 := (q value: #(12 16) * 99) * ((QuadC freq: 0.1 a: 1 b: -1 c: -0.75 xi: #(-0.5 -1)) max: 0) * ((q value: 15) max: 0) / 15.
  q6 := (Pan2 in: (QuadC freq: 99 * b a: 1 b: (q value: 1 / b / #(15 5 15)) > 0 * 0.375 - 1.25 c: -0.75 xi: 0) tanh pos: 0.5 * q2 level: q2) sum / 2.
  ^q3 + q4 splay2 + (AllpassN in: q5 + q6 maxdelaytime: e delaytime: e decaytime: 15)
!
jar14Qx
  "http://sccode.org/1-4Qx (jar)"
  "
  Sc3HelpGraph new jar14Qx play.
  Sc3 reset.
  "
  | f g h s |
  f := LFCub freq: 9.1 iphase: 0 mul: 100 add: #(200 300 400 500).
  g := LFCub freq: {1/2. 1/3. 1/5. 1/7} iphase: 0 mul: 0.5 add: (LFCub freq: 9 iphase: 0).
  h := LFCub freq: 1/2 iphase: 0 mul: 0.4 add: 0.5.
  s := LFCub freq: f kr iphase: 0 mul: (Lag in: g > h lagTime: 0.1) kr.
  ^Splay input: s level: 0.075
!
jarRk20120422
  "rukano; Apr 22, 2012; Re: Epic Pads (jar)"
  "
  Sc3HelpGraph new jarRk20120422 play.
  Sc3 reset.
  "
  | freq gen fmod rqmod sig amp |
  freq := [ (LFNoise2 freq: 1 mul: 0.011 add: 1) * #(60 64 65 67) atRandom midiCps ] dup: 24.
  gen := LFSaw freq: freq iphase: 0 mul: 0.1.
  fmod := (LFCub freq: (1 / 12) iphase: 0) range: 1 -> (MouseX minval: 2 maxval: 16 warp: 0 lag: 0.2).
  rqmod := LFNoise2 freq: 1 mul: 0.011 add: 1.
  sig := RLPF in: gen freq: (freq * fmod) kr rq: rqmod kr.
  amp := MouseY minval: 0 maxval: 0.25 warp: 0 lag: 0.2.
  ^sig splay2 * amp kr
!
jh4157
  "jh ; https://scsynth.org/t/auditory-illusion-with-exponentially-spaced-frequencies/4157"
  "
  Sc3HelpGraph new jh4157 play.
  Sc3 reset.
  "
  ^(SinOsc freq: (4 ** (200 iota * 0.002) * 300) phase: 0) mix * 0.02
!
jl19
  "http://sccode.org/1-9 (jl)"
  "
  Sc3HelpGraph new jl19 play.
  Sc3 reset.
  "
  | n k a f |
  n := 956. "0"
  k := 98. "1"
  a := HPF in: (PinkNoise mul: 0.005) freq: 10 mul: (Ln start: 0 end: 1 dur: 9) kr.
  f := [ :i | Ringz in: (LFNoise1 freq: (0.05 + 0.1 rand) mul: a) freq: 55 * (i + n) + 60 decaytime: 0.2 ].
  ^(GVerb in: ((1 to: k) mixFill: f) roomsize: 70 revtime: 99 damping: 0.5 inputbw: 0.5 spread: 15 drylevel: 1 earlyreflevel: 0.7 taillevel: 0.5 maxroomsize: 300) tanh
!
jlBitwise
  "bitwise ; a0f253ff89f6b244ea29a1e431dd9e5df5571a8b (jl)"
  "
  Sc3HelpGraph new jlBitwise play.
  Sc3 reset.
  "
  | t p1 p2 p3 p4 s |
  t := PulseCount trig: (Impulse freq: 8000 phase: 0) reset: 0.
  p1 := (t * 15) bitAnd: (t bitShiftRight: 5).
  p2 := (t *  5) bitAnd: (t bitShiftRight: #(3 4)).
  p3 := (t *  2) bitAnd: (t bitShiftRight: 9).
  p4 := (t *  8) bitAnd: (t bitShiftRight: 11).
  s := ({p1. p2. p3. p4} inject: 0 into: [:i :j | j bitOr: i]) - 3 % 256.
  ^(HPF in: (((s / 127) - 1) * 3) freq: 20) tanh * 0.02
!
jlRainThunder
  "http://sccode.org/1-e (jl)"
  "
  Sc3HelpGraph new jlRainThunder play.
  Sc3 reset.
  "
  | d0 n0 e0 p0 n1 f1 e1 p1 |
  d0 := LPF in: (Dust2 density: (LFNoise1 freq: 0.2 mul: 5 add: 45) kr) freq: 7000.
  n0 := PinkNoise mul: (LFNoise1 freq: 0.3 mul: 0.02 add: 0.08) add: d0.
  e0 := Line start: 0 end: 1 dur: 10 doneAction: 0.
  p0 := (GVerb in: (HPF in: (n0 + d0) freq: 400) roomsize: 250 revtime: 100 damping: 0.25 inputbw: 0.5 spread: 15 drylevel: 0.3 earlyreflevel: 0.7 taillevel: 0.5 maxroomsize: 300 mul: e0 kr) tanh.
  n1 := PinkNoise mul: ((Clip in: (LFNoise1 freq: 3) lo: 0 hi: 1 mul: (Clip in: (LFNoise1 freq: 2) lo: 0 hi: 1)) kr ** 1.8).
  f1 := LinExp in: (LFNoise1 freq: 1) srclo: -1 srchi: 1 dstlo: 100 dsthi: 2500.
  e1 := Line start: 0 end: 0.7 dur: 30 doneAction: 0.
  p1 := GVerb in: (LPF in: (HPF in: n1 freq: 20 mul: 10) freq: f1 kr) tanh roomsize: 270 revtime: 30 damping: 0.7 inputbw: 0.5 spread: 15 drylevel: 0.5 earlyreflevel: 0.7 taillevel: 0.5 maxroomsize: 300 mul: e1 kr.
  ^Limiter in: (p0 + p1) level: 1 dur: 0.01 mul: 0.2
!
jmccAnalogBubblesMouse
  "analog bubbles with mouse control (jmcc) #3"
  "
  Sc3HelpGraph new jmccAnalogBubblesMouse play.
  Sc3 reset.
  "
  | y x lfo_2 lfo_1 freq |
  y := MouseY minval: 0.1 maxval: 10 warp: 1 lag: 0.2. "lfo 1 rate"
  x := MouseX minval: 2 maxval: 40 warp: 1 lag: 0.2.  "lfo 2 rate"
  lfo_2 := LFSaw freq: x iphase: 0 mul: -3 add: 80. "depth & offset in semitones"
  lfo_1 := LFSaw freq: y iphase: 0 mul: 24 add: lfo_2. "depth in semitones, offset is lfo_2"
  freq := lfo_1 midiCps kr. "convert to frequency"
  ^CombN in: (SinOsc freq: freq phase: 0 mul: 0.04) maxdelaytime: 0.2 delaytime: 0.2 decaytime: 2 "echoing sine wave"
!
jmccAnalogBubbles
  "analog bubbles (jmcc) #1"
  "
  Sc3HelpGraph new jmccAnalogBubbles play.
  Sc3 reset.
  "
  | o f s |
  o := LFSaw freq: #(8 7.23) iphase: 0 mul: 3 add: 80.
  f := LFSaw freq: 0.4 iphase: 0 mul: 24 add: o.
  s := SinOsc freq: (f midiCps kr) phase: 0 mul: 0.04.
  ^CombN in: s maxdelaytime: 0.2 delaytime: 0.2 decaytime: 4 mul: 0.1
!
jmccAnalogueDaze
  "analogue daze (jmcc) #3"
  "
  Sc3HelpGraph new jmccAnalogueDaze play.
  Sc3 reset.
  "
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
  ^(CombN in: g maxdelaytime: 0.375 delaytime: 0.375 decaytime: 5) + g reverse * 0.4.
!
jmccBabblingBrook
  "babbling brook (jmcc) #SC3"
  "
  Sc3HelpGraph new jmccBabblingBrook play.
  Sc3 reset.
  "
  | b |
  b := [ :f :m :a :g |
    | n1 n2 |
    n1 := OnePole in: BrownNoise new coef: 0.99.
    n2 := LPF in: BrownNoise new freq: f mul: m add: a.
    RHPF in: n1 freq: n2 rq: 0.03 mul: g ].
  ^[ b value: 14 value: 400 value: 500 value: 0.06 ] dup + [ b value: 20 value: 800 value: 1000 value: 0.10 ] dup
!
jmccBerlin1977
  "berlin 1977 (jmcc) #4"
  "
  Sc3HelpGraph new jmccBerlin1977 play.
  Sc3 reset.
  "
  | sequ sequR clock_rate clock_time clock patternList note1 clock_16 note2 freq env amp filt pw sig |
  sequ := [:s :tr | DmdOn trig: tr reset: 0 demandUGens: (Seq repeats: Float infinity list: s)]. "somewhat akin to SC2 Sequencer"
  sequR := [:s :tr | DmdOn trig: tr reset: 0 demandUGens: (Shuf repeats: Float infinity list: s)]. "sequ with random selection function"
  clock_rate := MouseX minval: 5 maxval: 20 warp: 1 lag: 0.2. "mouse x controls clock rate"
  clock_time := clock_rate reciprocal.
  clock := Impulse freq: clock_rate phase: 0. "sequencer trigger"
  patternList := #(55 60 63 62 60 67 63 58).
  note1 := sequ value: patternList value: clock. "midi note pattern sequencer"
  clock_16 := PulseDivider trig: clock div: 16 start: 0. "divide clock by 16"
  note2 := (sequR value: #(-12 -7 -5 0 2 5) value: clock_16) + note1. "transpose somewhat randomly"
  freq := note2 midiCps. "convert midi note to cycles per second"
  env := Decay2 in: clock attackTime: (clock_time * 0.05) decayTime: (clock_time * 2).
  amp := env * 0.1 + 0.02. "amplitude envelope"
  filt := env * (FSinOsc freq: 0.17 iphase: 0) * 800 + 1400. "filter frequency"
  pw := SinOsc freq: 0.08 phase: {0. 0.5 * Float pi} mul: 0.45 add: 0.5. "pulse width LFO(s)"
  sig := Pulse freq: freq kr width: pw kr mul: amp kr.
  ^CombN in: (RLPF in: sig freq: filt kr rq: 0.15) maxdelaytime: 0.2 delaytime: #(0.2 0.17) decaytime: 1.5
!
jmccBidirectionalStrummableGuitar
  "bidirectional strummable guitar (jmcc) #11"
  "
  Sc3HelpGraph new jmccBidirectionalStrummableGuitar play.
  Sc3 reset.
  "
  |pitch1 pitch2 mousex nd out|
  pitch1 := #(52 57 62 67 71 76).
  pitch2 := pitch1 + 7.
  mousex := MouseX minval: 0 maxval: 1 warp: 0 lag: 0.2.
  nd := [ :i |
    | trigger pluck1 period1 string1 pluck2 period2 string2 |
    trigger := HPZ1 in: mousex > (0.25 + (i * 0.1)).
    pluck1 := PinkNoise mul: (Decay in: (trigger max: 0) decayTime: 0.05) kr.
    period1 := (pitch1 at: i) midiCps reciprocal.
    string1 := CombL in: pluck1 maxdelaytime: period1 delaytime: period1 decaytime: 4.
    pluck2 := BrownNoise mul: (Decay in: (trigger negated max: 0) decayTime: 0.05) kr.
    period2 := (pitch2 at: i) midiCps reciprocal.
    string2 := CombL in: pluck2 maxdelaytime: period2 delaytime: period2 decaytime: -4.
    Pan2 in: (string1 + string2) pos: i * 0.2 - 0.5 level: 1].
  out := (1 to: pitch1 size) mixFill: nd.
  ^LeakDC in: (LPF in: out freq: 12000) coef: 0.995
!
jmccBirdies
  "birdies (jmcc) #6 ; texture=overlap,7,4,4,inf"
  "
  Sc3HelpGraph new jmccBirdies play.
  Sc3 reset.
  "
  | p1 p2 p3 sw freq amp |
  p1 := LFPulse freq: 1.0 rand + 0.4 iphase: 0 width: 0.8 rand + 0.1 mul: 3.0 rand + 4 add: 2.
  p2 := LFPulse freq: 1.0 rand + 0.4 iphase: 0 width: 0.8 rand + 0.1 mul: 3.0 rand + 4.
  p3 := LFPulse freq: 0.5 rand + 0.2 iphase: 0 width: 0.4 mul: 0.02.
  sw := LFSaw freq: (p1 + p2) iphase: 0 mul: 800.0 rand + 1000 negated add: 1200.0 rand2 + 4000.
  freq := Lag in: sw lagTime: 0.05.
  amp := Lag in: p3 lagTime: 0.3.
  ^Pan2 in: (SinOsc freq: freq kr phase: 0) pos: 1.0 rand2 level: amp kr
!
jmccDemandingStudies
  "demanding studies (jmcc)"
  "
  Sc3HelpGraph new jmccDemandingStudies play.
  Sc3 reset.
  "
  | s1 s2 s3 x tr f o1 o2 o3 |
  s1 := Choose repeats: Float infinity list: #(72 75 79 82).
  s2 := Choose repeats: 1 list: #(82 84 86).
  s3 := Seq repeats: Float infinity list: {72. 75. 79. s2}.
  x := MouseX minval: 5 maxval: 13 warp: 0 lag: 0.2.
  tr := Impulse freq: x phase: 0.
  f := DmdOn trig: tr reset: 0 demandUGens: {(s1 - 12) midiCps dr. s3 midiCps dr}.
  o1 := SinOsc freq: (f + #(0 0.7)) kr phase: 0.
  o2 := Saw freq: (f + #(0 0.7)) kr mul: 0.3.
  o3 := (o1 + o2) distort log distort cubed.
  ^o3 * 0.1
!
jmccHarmonicSwimming
  "harmonic swimming (jmcc) #1"
  "
  Sc3HelpGraph new jmccHarmonicSwimming play.
  Sc3 reset.
  "
  | a f p l o |
  a := 0.02. "amplitude scale"
  f := 50. "fundamental frequency"
  p := 20. "number of partials per channel"
  l := Line start: 0 end: (a negated) dur: 60 doneAction: 0. "amplitude offset, causes sound to separate and fade"
  o := [ :h | "harmonic number (from one)"
    | r n e |
    r := [ Rand lo: -4 hi: 4 ] dup + 6. "amplitude rate"
    n := LFNoise1 freq: r mul: a add: l.
    e := (n max: 0) kr. "clip negative amplitudes to zero"
    FSinOsc freq: (f * h) iphase: 0 mul: e ].
  ^(1 to: p) mixFill: o
!
jmccHarmonicTumbling
  "harmonic tumbling (jmcc) #1"
  "
  Sc3HelpGraph new jmccHarmonicTumbling play.
  Sc3 reset.
  "
  | f t o |
  f := 80.
  t := XLine start: #(10 11) end: 0.1 dur: 60 doneAction: 0. "trigger probability decreases over time"
  o := [ :h |
   | n r e |
   n := Dust density: t mul: 0.02.
   r := Rand lo: 0 hi: 0.5.
   e := Decay2 in: n attackTime: 0.005 decayTime: r.
   FSinOsc freq: (f * h) iphase: 0 mul: e kr ].
  ^(1 to: 10) mixFill: o
!
jmccHarmonicZither
  "harmonic zither (jmcc) #11"
  "
  Sc3HelpGraph new jmccHarmonicZither play.
  Sc3 reset.
  "
  | pitch triggerSpacing panSpacing stringFunc |
  pitch := #(50 53.86 57.02 59.69 62 64.04 65.86 67.51 69.02 71.69 72.88 74).
  triggerSpacing := 0.5 / (pitch size - 1).
  panSpacing := 1.5 / (pitch size - 1).
  stringFunc := [ :i |
   | trigger pluck period string |
   trigger := HPZ1 in: ((MouseX minval: 0.0 maxval: 1.0 warp: 0.0 lag: 0.2) > (0.25 + (i - 1 * triggerSpacing))) abs.
   pluck := PinkNoise mul: (Decay in: trigger decayTime: 0.05) kr.
   period := (pitch at: i) midiCps reciprocal.
   string := CombL in: pluck maxdelaytime: period delaytime: period decaytime: 8.
   Pan2 in: string pos: i - 1 * panSpacing - 0.75 level: 1 ].
  ^LeakDC in: (LPF in: ((1 to: pitch size) mixFill: stringFunc) freq: 12000) coef: 0.995
!
jmccInharmonicWarbulence
  "jmcc - inharmonic warbulence ; graph rewrite"
  "
  Sc3HelpGraph new jmccInharmonicWarbulence play.
  Sc3 reset.
  "
  | gen z |
  gen := [ :tr |
    | f a r n |
    f := (TRand lo: 24 hi: 96 trig: tr) midiCps.
    a := (500 / f) min: 1.
    r := TXLine start: (TExpRand lo: 0.1 hi: 20 trig: tr) end: (TExpRand lo: 0.1 hi: 20 trig: tr) dur: 25.6 trig: tr.
    n := 12.
    (n mixFill: [
      | g m |
      g := (TRand lo: 0 hi: n trig: tr) + 1.
      m := SinOsc freq: r * (TRand lo: 0.9 hi: 1.1 trig: tr) phase: (TRand lo: 0 hi: (2 * Float pi) trig: tr) mul: 0.08 add: -0.04.
      Pan2 in: (SinOsc freq: f * g phase: 0 mul: ((m max: 0) * (2 / g))) pos: (TRand lo: -1 hi: 1 trig: tr) level: 1 ]) * 0.1 ].
  z := OverlapTexture graphFunc: gen sustainTime: 12.8 transitionTime: 6.4 overlap: 6.
  ^(5 mixFill: [ CombN in: z maxdelaytime: 0.3 delaytime: [ Rand lo: 0.1 hi: 0.3 ] dup decaytime: 8 ]) * 0.3
!
jmccLfoModulation
  "lfo modulation (jmcc) #1"
  "
  Sc3HelpGraph new jmccLfoModulation play.
  Sc3 reset.
  "
  | o p s |
  o := FSinOsc freq: 0.05 iphase: 0 mul: 80 add: 160.
  p := FSinOsc freq: #(0.6 0.7) iphase: 0 mul: 3600 add: 4000.
  s := RLPF in: (LFPulse freq: (o kr) iphase: 0 width: 0.4 mul: 0.05) freq: p kr rq: 0.2.
  ^CombL in: s maxdelaytime: 0.3 delaytime: #(0.2 0.25) decaytime: 2
!
jmccModalSpace
  "modal space (jmcc) #8"
  "
  Sc3HelpGraph new jmccModalSpace play.
  Sc3 reset.
  "
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
  ^(c value: 48) + (c value: 72) * 0.25
!
jmccMotoRev
  "moto rev (jmcc) #1"
  "
  Sc3HelpGraph new jmccMotoRev play.
  Sc3 reset.
  "
  | f s |
  f := SinOsc freq: 0.2 phase: 0 mul: 10 add: 21.
  s := LFPulse freq: f kr iphase: #(0 0.1) width: 0.1.
  ^(RLPF in: s freq:100 rq: 0.1) clip2: 0.4
!
jmccNarrowBandFilteredCrackleNoise
  "narrow band filtered crackle noise (jmcc) #2"
  "
  Sc3HelpGraph new jmccNarrowBandFilteredCrackleNoise play.
  Sc3 reset.
  "
  | rf1 rf2 rf c |
  rf1 := (0.0 rand: 2000.0) + 80 .
  rf2 := rf1 + ((-0.2 rand: 0.2) * rf1) .
  rf := XLine start: rf1 end: rf2 dur: 9 doneAction: 0 .
  c := Crackle chaosParam: (1.97 + (0.0 rand: 0.03)) .
  ^Pan2 in: (Resonz in: c freq: rf kr bwr: 0.2) pos: (-1.0 rand: 1.0) level: 0.15
!
jmccPluckedStrings
  "plucked strings (jmcc)"
  "
  Sc3HelpGraph new jmccPluckedStrings play.
  Sc3 reset.
  "
  | str |
  str := [
    | i dt t |
    i := [
      | s0 s1 s2 |
      s0 := Impulse freq: (2.0 rand: 2.2) phase: 0 mul: 0.3.
      s1 := Dust density: 0.5 mul: 0.3.
      s2 := Impulse freq: (SinOsc freq: (0.05 rand: 0.15) phase: (0.0 rand: (Float pi * 2)) mul: 5 add: 5.2) kr phase: 0 mul: 0.3.
      {s0. s1. s2} atRandom ].
    dt := [ 1 / ((60.0 rand: 90.0) floor midiCps) ] value.
    t := Decay in: i value decayTime: 0.1 mul: (PinkNoise mul: 0.1).
    Pan2 in: (CombL in: t maxdelaytime: dt delaytime: dt decaytime: 4) pos: (-1.0 rand: 1.0) level: 1 ].
  ^5 mixFill: str
!
jmccPulsingBottlesIr
  "pulsing bottles (jmcc) #2"
  "
  Sc3HelpGraph new jmccPulsingBottlesIr play.
  Sc3 reset.
  "
  | n f |
  n := 6. "number of bottles"
  f := [
    | a l |
    a := LFPulse freq: (Rand lo: 4 hi: 14) iphase: 0 width: (Rand lo: 0 hi: 0.7) mul: (0.8 / n).
    l := SinOsc freq: (Rand lo: 0.1 hi: 0.5) phase: (Rand lo: 0 hi: (2 * Float pi)).
    Pan2 in: (Resonz in: (WhiteNoise mul: a kr) freq: (Rand lo: 400 hi: 7400) bwr: 0.01) pos: (l kr) level: 1]. "linrand"
  ^n mixFill: f
!
jmccPulsingBottles
  "pulsing bottles (jmcc) #2"
  "
  Sc3HelpGraph new jmccPulsingBottles play.
  Sc3 reset.
  "
  | n f |
  n := 6. "number of bottles"
  f := [
    | a l |
    a := LFPulse freq: (4.0 rand: 14.0) iphase: 0 width: (0.7 rand) mul: (0.8 / n).
    l := SinOsc freq: (0.1 rand: 0.5) phase: (2 * Float pi) rand.
    Pan2 in: (Resonz in: (WhiteNoise mul: a kr) freq: (400.0 rand: 7400.0) bwr: 0.01) pos: (l kr) level: 1]. "linrand"
  ^n mixFill: f
!
jmccRandomPulsations
  "random pulsations (jmcc) #1 ; graph rewrite"
  "
  Sc3HelpGraph new jmccRandomPulsations play.
  Sc3 reset.
  "
  | f |
  f := [ :tr |
    | o1 o2 o3 |
    o1 := FSinOsc freq: (TRand lo: 0 hi: 2000 trig: tr) iphase: 0.
    o2 := SinOsc freq: (TRand lo: 8 hi: 88 trig: tr) phase: 0.
    o3 := SinOsc freq: (0.3 + (TRand lo: 0 hi: 0.5 trig: tr)) phase: (TRand lo: 0 hi: Float pi * 2 trig: tr) mul: 0.7.
    Pan2 in: (o1 amClip: o2) pos: o3 kr level: 0.05 ].
  ^OverlapTexture graphFunc: f sustainTime: 5 transitionTime: 2 overlap: 6
!
jmccRandomSineWaves
  "random sine waves (jmcc) #1 ; texture=overlap,5,2,9"
  "
  Sc3HelpGraph new jmccRandomSineWaves play.
  Sc3 reset.
  "
  ^Pan2 in: (FSinOsc freq: (2000.0 rand) iphase: 0 mul: 0.05) pos: 1.0 rand2 level: 1
!
jmccReverberatedSinePercussion
  "reverberated sine percussion (jmcc) #3"
  "
  Sc3HelpGraph new jmccReverberatedSinePercussion play.
  Sc3 reset.
  "
  | d s z t y x |
  d := 6.
  s := d mixFill: [ Resonz in: (Dust density: 2 / d mul: 50) freq: (Rand lo: 200 hi: 3200) bwr: 0.003 ].
  z := DelayN in: s maxdelaytime: 0.048 delaytime: 0.048.
  t := LFNoise1 freq: ([Rand lo: 0 hi: 0.1] dup: 5) mul: 0.04 add: 0.05.
  y := (CombL in: z maxdelaytime: 0.1 delaytime: t kr decaytime: 15) mix.
  x := y.
  4 timesRepeat: [ x := AllpassN in: x maxdelaytime: 0.050 delaytime: [Rand lo: 0 hi: 0.05] dup decaytime: 1 ].
  ^x * 0.2 + s
!
jmccSampleAndHoldLiquidities
  "sample and hold liquidities (jmcc) #4"
  "
  Sc3HelpGraph new jmccSampleAndHoldLiquidities play.
  Sc3 reset.
  "
  | r t c cf f p s i |
  r := MouseX minval: 1 maxval: 200 warp: 1 lag: 0.2.
  t := r reciprocal.
  c := Impulse freq: r phase: 0 mul: 0.4.
  cf := MouseY minval: 100 maxval: 8000 warp: 1 lag: 0.1.
  f := Latch in: (WhiteNoise new * cf * 0.5 + cf) trig: c.
  p := Latch in: (WhiteNoise new) trig: c.
  s := SinOsc freq: f kr phase: 0 mul: (Decay2 in: c attackTime: t * 0.1 decayTime: t * 0.9) kr.
  i := Pan2 in: s pos: p kr level: 1.
  ^CombN in: i maxdelaytime: 0.3 delaytime: 0.3 decaytime: 2
!
jmccScratchy
  "scratchy (jmcc) #1"
  "
  Sc3HelpGraph new jmccScratchy play.
  Sc3 reset.
  "
  | n f |
  n := (2 mceFill: [ BrownNoise new ]) * 0.5.
  f := ((n - 0.49) max: 0) * 20.
  ^RHPF in: f freq: 5000 rq: 1
!
jmccSprinklerMouse
  "sprinkler mouse (jmcc) #1"
  "
  Sc3HelpGraph new jmccSprinklerMouse play.
  Sc3 reset.
  "
  | f t |
  f := MouseX minval: 0.2 maxval: 50 warp: 0 lag: 0.2.
  t := LFPulse freq: f iphase: 0 width: 0.25 mul: 0.1.
  ^BPZ2 in: (WhiteNoise mul: t kr)
!
jmccSprinkler
  "sprinkler (jmcc) #1"
  "
  Sc3HelpGraph new jmccSprinkler play.
  Sc3 reset.
  "
  | f t |
  f := LFPulse freq: 0.09 iphase: 0 width: 0.16 mul: 10 add: 7.
  t := LFPulse freq: f iphase: 0 width: 0.25 mul: 0.1.
  ^BPZ2 in: (WhiteNoise mul: t kr)
!
jmccStrummableGuitar
  "strummable guitar (jmcc) #11"
  "
  Sc3HelpGraph new jmccStrummableGuitar play.
  Sc3 reset.
  "
  | scale str strs |
  scale := #(52 57 62 67 71 76).
  str := [ :i |
   | x t e n dt s |
   x := MouseX minval: 0 maxval: 1 warp: 0 lag: 0.2.
   t := (HPZ1 in: (x > (0.25 + (i - 1 * 0.1)))) abs.
   e := (Decay in: t decayTime: 0.05) kr.
   n := PinkNoise mul: e.
   dt := (scale at: i) midiCps reciprocal.
   s := CombL in: n maxdelaytime: dt delaytime: dt decaytime: 4.
   Pan2 in: s pos: (i - 1 * 0.2 - 0.5) level: 1].
  strs := (1 to: scale size) mixFill: str.
  ^LeakDC in: (LPF in: strs freq: 12000) coef: 0.995
!
jmccStrummableSilk
  "strummable silk (jmcc) #11"
  "
  Sc3HelpGraph new jmccStrummableSilk play.
  Sc3 reset.
  "
  | x o s |
  x := MouseX minval: 0.0 maxval: 1.0 warp: 0.0 lag: 0.2.
  o := (1 to: 8) mixFill: [ :ix |
    | n trigger env pluck freq z metal |
    n := 15.
    trigger := (HPZ1 in: (x > (0.25 + (ix - 1 * 0.07)))) abs.
    env := Decay in: (Impulse freq: 14 phase: 0 mul: (Lag in: (Trig in: trigger dur: 1) lagTime: 0.2 mul: 0.01) kr) decayTime: 0.04.
    pluck := PinkNoise mul: env.
    freq := ((#(-2 0 3 5 7 10 12 15) at: ix) + 60) midiCps.
    metal := RingzBank input: pluck freq: (n iota collect: [ :j | j * freq ]) amp: #(1) time: ([ 0.3 rand: 1.0 ] dup: n).
    Pan2 in: metal pos: (ix - 1 * 0.2 - 0.5) level: 1 ].
  s := LeakDC in: (LPF in: o freq: 12000) coef: 0.995.
  6 timesRepeat: [ s := AllpassN in: s maxdelaytime: 0.1 delaytime: {0.05 rand. 0.05 rand} decaytime: 4 ]. "rvb"
  ^s
!
jmccSweepyNoise
  "sweepy noise (jmcc) #6"
  "
  Sc3HelpGraph new jmccSweepyNoise play.
  Sc3 reset.
  "
  | lfoDepth lfoRate freq filtered |
  lfoDepth := MouseY minval: 200 maxval: 8000 warp: 1 lag: 0.1.
  lfoRate := MouseX minval: 4 maxval: 60 warp: 1 lag: 0.1.
  freq := LFSaw freq: lfoRate iphase: 0 mul: lfoDepth add: (lfoDepth * 1.2).
  filtered := RLPF in: [ WhiteNoise mul: 0.03 ] dup freq: freq kr rq: 0.1.
  ^(CombN in: filtered maxdelaytime: 0.3 delaytime: 0.3 decaytime: 2) + filtered
!
jmccSyntheticPiano
  "synthetic piano (jmcc) #3"
  "
  Sc3HelpGraph new jmccSyntheticPiano play.
  Sc3 reset.
  "
  | p |
  p := [
   | n f ph s e c l |
   n := 36 rand: 90.
   f := 0.1 rand: 0.5.
   ph := 0.0 rand: 1.0.
   s := Impulse freq: f phase: ph mul: 0.1.
   e := Decay2 in: s attackTime: 0.008 decayTime: 0.04.
   c := [ :ix |
    | n0 o dt |
    n0 := LFNoise2 freq: 3000.
    o := #(-0.05 0.0 0.04) at: ix.
    dt := (n + o) midiCps reciprocal.
    CombL in: (n0 * e) maxdelaytime: dt delaytime: dt decaytime: 6].
   l := ((n - 36) / 27) - 1.
   Pan2 in: ((1 to: 3) mixFill: c) pos: l level: 1].
  ^6 mixFill: p
!
jmccTank
  "tank (jmcc)"
  "
  Sc3HelpGraph new jmccTank play.
  Sc3 reset.
  "
  | r_allpass pling bang tank signal |
  r_allpass := [ :z | AllpassN in: z maxdelaytime: 0.03 delaytime: [Rand lo: 0.005 hi: 0.02] dup decaytime: 1 ].
  pling := [
    | d f p s1 s2 |
    d := Dust density: 0.2.
    f := ExpRand lo: 300 hi: 2200.
    p := Rand lo: -1 hi: 1.
    s1 := (FSinOsc freq: f iphase: 0) cubed.
    s2 := Decay2 in: d attackTime: 0.1 decayTime: 0.5 mul: s1.
    Pan2 in: s2 pos: p level: 0.1 ].
  bang := [
    | d n |
    d := Dust density: 0.01.
    n := BrownNoise new.
    Pan2 in: (Decay2 in: d attackTime: 0.04 decayTime: 0.3 mul: n) pos: 0 level: 1 ] value.
  tank := [ :i |
    | l0 l1 l2 l3 l4 l5 l6 l7 |
    l0 := LocalIn numChan: 2 default: 0 mul: 0.98.
    l1 := OnePole in: l0 coef: 0.33.
    l2 := l1. "RotateStereo in: l1 pos: 0.23."
    l3 := AllpassN in: l2 maxdelaytime: 0.05 delaytime: [Rand lo: 0.01 hi: 0.05] dup decaytime: 2.
    l4 := DelayN in: l3 maxdelaytime: 0.3 delaytime: #(0.17 0.23).
    l5 := AllpassN in: l4 maxdelaytime: 0.05 delaytime: [Rand lo: 0.03 hi: 0.15] dup decaytime: 2.
    l6 := LeakDC in: l5 coef: 0.995.
    l7 := l6 + i.
    Mrg lhs: l7 rhs: (LocalOut channelsArray: l7) ].
  signal := (12 mixFill: pling) + bang.
  4 timesRepeat: [ signal := r_allpass value: signal ].
  ^tank value: signal
!
jmccTheremin
  "theremin (jmcc)"
  "
  Sc3HelpGraph new jmccTheremin play.
  Sc3 reset.
  "
  | mod detune x y f a |
  mod := 7.
  detune := 0.
  x := (MouseX minval: 0 maxval: 0.9 warp: 0 lag: 0.2) kr.
  y := ((MouseY minval: 4000 maxval: 200 warp: 1 lag: 0.8) + detune) kr.
  f := SinOsc freq: mod phase: 0 mul: (y * 0.02) add: y.
  a := SinOsc freq: f phase: 0 mul: x.
  ^Pan2 in: a pos: 0 level: 0.1
!
jmccUplink
  "uplink ; jmcc"
  "
  Sc3HelpGraph new jmccUplink play.
  Sc3 reset.
  "
  | osc freq sig |
   osc := [
   | e |
   e := (LFPulse freq: (Rand lo: 0 hi: 4) iphase: 0 width: (Rand lo: 0 hi: 1)) mul: (Rand lo: 0 hi: 8000) add: (Rand lo: 0 hi: 2000).
   LFPulse freq: (Rand lo: 0 hi: 20) iphase: 0 width: (Rand lo: 0 hi: 1) mul: e ].
  freq := osc value + osc value.
  sig := LFPulse freq: freq kr iphase: 0 width: 0.5 mul: 0.04.
  ^Pan2 in: sig pos: (Rand lo: 0 hi: 0.8) level: 1
!
jmccWhySupercollider
  "why supercollider (jmcc) #0"
  "
  Sc3HelpGraph new jmccWhySupercollider play.
  Sc3 reset.
  "
  | s z x |
  s := 10 mixFill: [ Resonz in: (Dust density: 0.2 mul: 50) freq: (200.0 rand: 3200.0) bwr: 0.003 ].
  z := DelayN in: s maxdelaytime: 0.048 delaytime: 0.048.
  x := 7 mixFill: [ CombL in: z maxdelaytime: 0.1 delaytime: (LFNoise1 freq: 0.1 rand mul: 0.04 add: 0.05) decaytime: 15 ].
  4 timesRepeat: [ x := AllpassN in: x maxdelaytime: 0.050 delaytime: [ 0.05 rand ] dup decaytime: 1 ].
  ^x * 0.2 + s
!
jrhbGcd
  "generative landscape (jrhb) ; https://www.listarc.bham.ac.uk/lists/sc-users/msg68991.html"
  "
  Sc3HelpGraph new jrhbGcd play.
  Sc3 reset.
  "
  | x y i d a z1 z2 freq c |
  x := MouseX minval: -10 maxval: 10 warp: 0 lag: 0.2.
  y := MouseY minval: 0 maxval: 5 warp: 0 lag: 0.2.
  i := LFSaw freq: 0.062 iphase: 0 mul: 10 add: (y * (0 to: 8)).
  d := i frac.
  a := (ModDif x: d y: 0.5 mod: 1) negated + 0.8.
  z1 := (i gcd: x) + d.
  z2 := (i lcm: x) + d.
  freq := (Mce with: z1 with: z2) transpose abs + 1 * 120.
  c := AmpComp freq: freq kr root: 261 exp: 0.33 mul: a.
  ^(SinOsc freq: freq kr phase: 0 mul: c kr) mix * 0.01
!
ncSctutorial
  "https://composerprogrammer.com/teaching/supercollider/sctutorial/tutorial.html 1.1"
  "
  Sc3HelpGraph new ncSctutorial play.
  Sc3 reset.
  "
  | n mk x y |
  n := 11.
  mk := [
   | freq numcps knum gen |
   freq := 50.0 rand: 560.3.
   numcps := 2 rand: 20.
   knum := SinOsc freq: (0.02 exprand: 0.2) phase: 0 mul: numcps / 2 add: numcps / 2.
   gen := Gendy1 ampdist: 6 rand durdist: 6 rand adparam: 1.0 rand ddparam: 1.0 rand minfreq: freq maxfreq: freq ampscale: 1.0 rand durscale: 1.0 rand initCPs: numcps knum: knum kr mul: 0.5 / n sqrt.
   Pan2 in: gen pos: 1.0 rand2 level: 1.0].
  x := MouseX minval: 100 maxval: 2000 warp: 0 lag: 0.2.
  y := MouseY minval: 0.01 maxval: 1.0 warp: 0 lag: 0.2.
  ^Resonz in: (n mixFill: mk) freq: x kr bwr: y kr mul: 0.5
!
nvTw04
  "nv https://swiki.hfbk-hamburg.de/MusicTechnology/899"
  "
  Sc3HelpGraph new nvTw04 play.
  Sc3 reset.
  "
  | s f a |
  s := LocalIn numChan: 2 default: 0 mul: 7.5 add: (Saw freq: #(32 33) mul: 0.2).
  f := (2 ** (LFNoise0 freq: 4/3 mul: 4)) * 300.
  a := CombN in: (BPF in: s freq: f rq: 0.1) distort maxdelaytime: 2 delaytime: 2 decaytime: 40.
  ^Mrg lhs: a rhs: (LocalOut channelsArray: a)
!
nvTw40
  "http://sccode.org/1-V (nv) [Line 40] ; minus freeverb2"
  "
  Sc3HelpGraph new nvTw40 play.
  Sc3 reset.
  "
  | nd s |
  nd := [ :i |
   | t f |
   t := Impulse freq: (2 ** i) / 32 phase: 1 / 2 mul: (0.6 ** i) * 40.
   f := (4 ** (LFNoise0 freq: 1 / 16)) * 300.
   (RLPF in: t freq: f kr rq: 0.005) sin ].
  ^Splay input: ((0 to: 7) collect: nd) level: 0.25
!
nvTw41
  "https://swiki.hfbk-hamburg.de/MusicTechnology/899 (nv) [Line 41]"
  "
  Sc3HelpGraph new nvTw41 play.
  Sc3 reset.
  "
  | n |
  n := [ :i |
    | f p b |
    f := 1.9 ** i / 128.
    p := [ PinkNoise new ] dup.
    b := 4 ** (LFNoise2 freq: 1.2 ** i / 16).
    (BPF in: p freq: b kr * 300 rq: 0.15) * (5 ** (LFNoise2 freq: f) / (i + 8) * 20) ].
  ^((1 to: 15) collect: n) splay2
!
pjForestSounds
  "sc-users, 2007-04-06 (pj) [paul jones]"
  "
  Sc3HelpGraph new pjForestSounds play.
  Sc3 reset.
  "
  | o |
  o := SinOsc freq: (LFNoise2 freq: 50 mul: 50 add: 50) phase: 0 mul: 100 add: 2000.
  ^[ BPF in: BrownNoise new freq: o kr rq: 0.001 mul: 10 ] dup
!
rdChrd
  "chrd (rd) ; graph rewrite"
  "
  Sc3HelpGraph new rdChrd play.
  Sc3 reset.
  "
  | gen |
  gen := [ :tr |
    | chrd scale octaves mnn chd |
    chrd := [ :m |
      | ds d f e p o |
      ds := 3.
      d := #(5 4 5 7 4 5) * ds.
      f := (TXLine start: m end: (TRand lo: 0.05 hi: 0.5 trig: tr) + m dur: d trig: tr) midiCps.
      e := (EnvSine dur: d max level: (TRand lo: 0.005 hi: 0.01 trig: tr)) asEnvGen: tr.
      p := TXLine start: (TRand lo: -1 hi: 1 trig: tr) end: (TRand lo: -1 hi: 1 trig: tr) dur: d trig: tr.
      o := SinOsc freq: f kr phase: 0.
      (Pan2 in: o pos: p level: e) mix ].
    scale := #(0 2 4 5 7 9 11).
    octaves := #(4 5 6 7).
    mnn := (scale collect: [ :n | octaves collect: [ :o | n + (o * 12) ] ]) concatenation.
    chd := [ TChoose trig: tr array: mnn ] dup: 6.
    9 mixFill: [ chrd value: chd ] ].
  ^OverlapTexture graphFunc: gen sustainTime: 21 transitionTime: 0 overlap: 3
!
rdEggcrate
  "eggcrate ; rd"
  "
  Sc3HelpGraph new rdEggcrate play.
  Sc3 reset.
  "
  | eggcrate p x y t f0 f1 f a |
  eggcrate := [ :u :v| (u * Float pi) cos * (v * Float pi) sin ].
  p := #(64 72 96 128 256 6400 7200 8400 9600).
  x := BrownNoise new.
  y := BrownNoise new.
  t := Dust density: 2.4.
  f0 := TChoose trig: t array: p.
  f1 := TChoose trig: t array: p.
  f := (eggcrate value: x value: y) range: f0 -> f1.
  a := x range: 0 -> 0.1.
  ^Pan2 in: (SinOsc freq: f kr phase: 0) pos: y kr level: a kr
!
rdXyInterference
  "xy interference (rd)"
  "
  Sc3HelpGraph new rdXyInterference play.
  Sc3 reset.
  "
  | x y nd |
    x := MouseX minval: 20 maxval: 22000 warp: 0 lag: #(0.005 0.025).
    y := MouseY minval: 20 maxval: 22000 warp: 0 lag: #(0.005 0.075).
    nd := [
      | n a b |
      n := LFNoise0 freq: #(5 9).
      a := SinOsc freq: x + n kr phase: 0.
      b := SinOsc freq: y phase: 0.
      a * b ].
  ^(3 mixFill: nd) * 0.35
!
tb14Tw
  "http://sccode.org/1-4Tw (tb)"
  "
  Sc3HelpGraph new tb14Tw play.
  Sc3 reset.
  "
  | i f n g |
  i := (Impulse freq: 1 / #(1 3 5) phase: #(0 0.133 0.5)) mix.
  f := [:st :z | {TDuty dur: ((Timer trig: (st at: 1)) max: 0.25) reset: 0 doneAction: 0 level: 1 gapFirst: 0} , st].
  n := (1 to: 10) inject: {i} into: f.
  g := [:x :j | SinOsc freq: (4000 + (j * 500)) phase: 0 mul: (Decay2 in: x attackTime: 0.01 decayTime: 0.2)].
  ^(n with: (0 to: 10) collect: g) splay2
!
twTw01
  "<https://swiki.hfbk-hamburg.de/MusicTechnology/899> (tw ; tim walters)"
  "
  Sc3HelpGraph new twTw01 play.
  Sc3 reset.
  "
  | s z |
  s := [ :k |
    | f |
    f := [ :i |
      | x |
      x := Impulse freq: (k == 0 ifTrue: [ 1 ] ifFalse: [ 0.5 ** i / k ]) phase: 0.
      SinOsc freq: i phase: (SinOsc freq: ((i + k) ** i) phase: 0) / (Decay in: x decayTime: (Mce with: i with: i + 1) mul: k) kr ].
    (1 to: 6) mixFill: f ].
  z := (1 to: 16) mixFill: s.
  ^(GVerb in: z roomsize: 1 revtime: 3 damping: 0.5 inputbw: 0.5 spread: 15 drylevel: 1 earlyreflevel: 0.7 taillevel: 0.5 maxroomsize: 300) / 512
!
twTw02
  "<https://swiki.hfbk-hamburg.de/MusicTechnology/899> (tw ; tim walters)"
  "
  Sc3HelpGraph new twTw02 play.
  Sc3 reset.
  "
  | depth nd |
  depth := 8.
  nd := [ :k |
    | x y |
    x := [ :f :m :j |
      SinOsc freq: f + (m * 4 * j) phase: m mul: (LFNoise1 freq: j + 1 / f * 4) / 2 ].
    y := x value: 0.1 value: 0 value: 8.
    1 to: depth do: [ :i | y := x value: #(4 8) * i * k value: y value: i ].
  y ].
  ^((1 to: 8) mixFill: nd) / 4

!
!
