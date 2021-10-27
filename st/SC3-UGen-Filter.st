!AbstractUGen methodsFor: 'filtering'!
  allpassC: maxdelaytime value: delaytime value: decaytime
    ^AllpassC in: self maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
  !
  allpassL: maxdelaytime value: delaytime value: decaytime
    ^AllpassL in: self maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
  !
  allpassN: maxdelaytime value: delaytime value: decaytime
    ^AllpassN in: self maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
  !
  bBandPass: freq value: bw
    ^BBandPass in: self freq: freq bw: bw
  !
  bBandStop: freq value: bw
    ^BBandStop in: self freq: freq bw: bw
  !
  bLowPass: freq value: rq
    ^BLowPass in: self freq: freq rq: rq
  !
  bpf: freq value: rq
    ^BPF in: self freq: freq rq: rq
  !
  bpz2
    ^BPZ2 in: self
  !
  brf: freq value: rq
    ^BRF in: self freq: freq rq: rq
  !
  bufWr: bufnum value: phase value: loop
    ^BufWr bufnum: bufnum phase: phase loop: loop inputArray: self
  !
  clip: lo value: hi
    ^Clip in: self lo: lo hi: hi
  !
  combC: maxdelaytime value: delaytime value: decaytime
    ^CombC in: self maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
  !
  combL: maxdelaytime value: delaytime value: decaytime
    ^CombL in: self maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
  !
  combN: maxdelaytime value: delaytime value: decaytime
    ^CombN in: self maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
  !
  decay: decayTime
    ^Decay in: self decayTime: decayTime
  !
  decay2: attackTime value: decayTime
    ^Decay2 in: self attackTime: attackTime decayTime: decayTime
  !
  degreeToKey: bufnum value: octave
    ^DegreeToKey bufnum: bufnum in: self octave: octave
  !
  delayN: maxdelaytime value: delaytime
    ^DelayN in: self maxdelaytime: maxdelaytime delaytime: delaytime
  !
  demand: reset value: demandUGens
    ^Demand trig: self reset: reset demandUGens: demandUGens
  !
  detectSilence: amp value: time value: doneAction
    ^DetectSilence in: self amp: amp time: time doneAction: doneAction
  !
  fold: lo value: hi
    ^Fold in: self lo: lo hi: hi
  !
  freeVerb: mix value: room value: damp
    ^FreeVerb in: self mix: mix room: room damp: damp
  !
  freeVerb2: in2 value: mix value: room value: damp
    ^FreeVerb2 in: self in2: in2 mix: mix room: room damp: damp
  !
  gVerb: roomsize value: revtime value: damping value: inputbw value: spread value: drylevel value: earlyreflevel value: taillevel value: maxroomsize
    ^GVerb in: self roomsize: roomsize revtime: revtime damping: damping inputbw: inputbw spread: spread drylevel: drylevel earlyreflevel: earlyreflevel taillevel: taillevel maxroomsize: maxroomsize
  !
  hasher
    ^Hasher in: self
  !
  hpf: freq
    ^HPF in: self freq: freq
  !
  hpz1
    ^HPZ1 in: self
  !
  inRange: lo value: hi
    ^InRange in: self lo: lo hi: hi
  !
  klank: freqscale value: freqoffset value: decayscale value: specificationsArrayRef
    ^Klank input: self freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef
  !
  lpf: freq
    ^LPF in: self freq: freq
  !
  lag: lagTime
    ^Lag in: self lagTime: lagTime
  !
  lagUD: lagTimeU value: lagTimeD
    ^LagUD in: self lagTimeU: lagTimeU lagTimeD: lagTimeD
  !
  lag2: lagTime
    ^Lag2 in: self lagTime: lagTime
  !
  lag3: lagTime
    ^Lag3 in: self lagTime: lagTime
  !
  lag3UD: lagTimeU value: lagTimeD
    ^Lag3UD in: self lagTimeU: lagTimeU lagTimeD: lagTimeD
  !
  leakDC: coef
    ^LeakDC in: self coef: coef
  !
  limiter: level value: dur
    ^Limiter in: self level: level dur: dur
  !
  linExp: srclo value: srchi value: dstlo value: dsthi
    ^LinExp in: self srclo: srclo srchi: srchi dstlo: dstlo dsthi: dsthi
  !
  linPan2: pos value: level
    ^LinPan2 in: self pos: pos level: level
  !
  localOut
    ^LocalOut channelsArray: self
  !
  mantissaMask: bits
    ^MantissaMask in: self bits: bits
  !
  modDif: y value: mod
    ^ModDif x: self y: y mod: mod
  !
  moogFF: freq value: gain value: reset
    ^MoogFF in: self freq: freq gain: gain reset: reset
  !
  moogLadder: ffreq value: res
    ^MoogLadder in: self ffreq: ffreq res: res
  !
  normalizer: level value: dur
    ^Normalizer in: self level: level dur: dur
  !
  onePole: coef
    ^OnePole in: self coef: coef
  !
  out: bus
    ^Out bus: bus channelsArray: self
  !
  pan2: pos value: level
    ^Pan2 in: self pos: pos level: level
  !
  pitchShift: windowSize value: pitchRatio value: pitchDispersion value: timeDispersion
    ^PitchShift in: self windowSize: windowSize pitchRatio: pitchRatio pitchDispersion: pitchDispersion timeDispersion: timeDispersion
  !
  pluck: trig value: maxdelaytime value: delaytime value: decaytime value: coef
    ^Pluck in: self trig: trig maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime coef: coef
  !
  pulseCount: reset
    ^PulseCount trig: self reset: reset
  !
  pulseDivider: div value: start
    ^PulseDivider trig: self div: div start: start
  !
  rhpf: freq value: rq
    ^RHPF in: self freq: freq rq: rq
  !
  rlpf: freq value: rq
    ^RLPF in: self freq: freq rq: rq
  !
  resonz: freq value: bwr
    ^Resonz in: self freq: freq bwr: bwr
  !
  ringz: freq value: decaytime
    ^Ringz in: self freq: freq decaytime: decaytime
  !
  runningMax: trig
    ^RunningMax in: self trig: trig
  !
  rtScramble: inputs
    ^RTScramble trigger: self inputs: inputs
  !
  slope
    ^Slope in: self
  !
  stepper: reset value: min value: max value: step value: resetval
    ^Stepper trig: self reset: reset min: min max: max step: step resetval: resetval
  !
  sweep: rate
    ^Sweep trig: self rate: rate
  !
  tExpRand: lo value: hi
    ^TExpRand lo: lo hi: hi trig: self
  !
  timer
    ^Timer trig: self
  !
  tiRand: lo value: hi
    ^TIRand lo: lo hi: hi trig: self
  !
  toggleFF
    ^ToggleFF trig: self
  !
  tRand: lo value: hi
    ^TRand lo: lo hi: hi trig: self
  !
  trig: dur
    ^Trig in: self dur: dur
  !
  trig1: dur
    ^Trig1 in: self dur: dur
  !
  wrap: lo value: hi
    ^Wrap in: self lo: lo hi: hi
  !

!
