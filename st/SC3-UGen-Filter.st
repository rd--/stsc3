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
  balance2: right value: pos value: level
    ^Balance2 left: self right: right pos: pos level: level
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
  crossoverDistortion: amp value: smooth
    ^CrossoverDistortion in: self amp: amp smooth: smooth
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
  delayC: maxdelaytime value: delaytime
    ^DelayC in: self maxdelaytime: maxdelaytime delaytime: delaytime
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
  greyholeRaw: in2 value: damping value: delaytime value: diffusion value: feedback value: moddepth value: modfreq value: size
    ^GreyholeRaw in1: self in2: in2 damping: damping delaytime: delaytime diffusion: diffusion feedback: feedback moddepth: moddepth modfreq: modfreq size: size
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
  integrator: coef
    ^Integrator in: self coef: coef
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
  latch: trig
    ^Latch in: self trig: trig
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
  linXFade2: inB value: pan
    ^LinXFade2 inA: self inB: inB pan: pan
  !
  localOut
    ^LocalOut channelsArray: self
  !
  lpz1
    ^LPZ1 in: self
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
  mulAdd: mul value: add
    ^MulAdd in: self mul: mul add: add
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
  replaceOut: bus
    ^ReplaceOut bus: bus channelsArray: self
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
  rotate2: y value: pos
    ^Rotate2 x: self y: y pos: pos
  !
  select: array
    ^Select which: self array: array
  !
  setResetFF: reset
    ^SetResetFF trig: self reset: reset
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
  tScramble: inputs
    ^TScramble trigger: self inputs: inputs
  !
  twoPole: freq value: radius
    ^TwoPole in: self freq: freq radius: radius
  !
  twoZero: freq value: radius
    ^TwoZero in: self freq: freq radius: radius
  !
  wrap: lo value: hi
    ^Wrap in: self lo: lo hi: hi
  !
  xFade2: inB value: pan value: level
    ^XFade2 inA: self inB: inB pan: pan level: level
  !

!
!Array methodsFor: 'filtering'!
  allpassC: maxdelaytime value: delaytime value: decaytime
    ^AllpassC in: self maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
  !
  allpassL: maxdelaytime value: delaytime value: decaytime
    ^AllpassL in: self maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
  !
  allpassN: maxdelaytime value: delaytime value: decaytime
    ^AllpassN in: self maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
  !
  balance2: right value: pos value: level
    ^Balance2 left: self right: right pos: pos level: level
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
  crossoverDistortion: amp value: smooth
    ^CrossoverDistortion in: self amp: amp smooth: smooth
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
  delayC: maxdelaytime value: delaytime
    ^DelayC in: self maxdelaytime: maxdelaytime delaytime: delaytime
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
  greyholeRaw: in2 value: damping value: delaytime value: diffusion value: feedback value: moddepth value: modfreq value: size
    ^GreyholeRaw in1: self in2: in2 damping: damping delaytime: delaytime diffusion: diffusion feedback: feedback moddepth: moddepth modfreq: modfreq size: size
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
  integrator: coef
    ^Integrator in: self coef: coef
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
  latch: trig
    ^Latch in: self trig: trig
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
  linXFade2: inB value: pan
    ^LinXFade2 inA: self inB: inB pan: pan
  !
  localOut
    ^LocalOut channelsArray: self
  !
  lpz1
    ^LPZ1 in: self
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
  mulAdd: mul value: add
    ^MulAdd in: self mul: mul add: add
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
  replaceOut: bus
    ^ReplaceOut bus: bus channelsArray: self
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
  rotate2: y value: pos
    ^Rotate2 x: self y: y pos: pos
  !
  select: array
    ^Select which: self array: array
  !
  setResetFF: reset
    ^SetResetFF trig: self reset: reset
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
  tScramble: inputs
    ^TScramble trigger: self inputs: inputs
  !
  twoPole: freq value: radius
    ^TwoPole in: self freq: freq radius: radius
  !
  twoZero: freq value: radius
    ^TwoZero in: self freq: freq radius: radius
  !
  wrap: lo value: hi
    ^Wrap in: self lo: lo hi: hi
  !
  xFade2: inB value: pan value: level
    ^XFade2 inA: self inB: inB pan: pan level: level
  !

!
!AbstractUGen methodsFor: 'filtering'!
  ampComp: root value: exp
    ^AmpComp freq: self root: root exp: exp
  !
  ampCompA: root value: minAmp value: rootAmp
    ^AmpCompA freq: self root: root minAmp: minAmp rootAmp: rootAmp
  !
  amplitude: attackTime value: releaseTime
    ^Amplitude in: self attackTime: attackTime releaseTime: releaseTime
  !
  analogFoldOsc: amp
    ^AnalogFoldOsc freq: self amp: amp
  !
  blip: numharm
    ^Blip freq: self numharm: numharm
  !
  bufRd: phase value: loop value: interpolation
    ^BufRd bufnum: self phase: phase loop: loop interpolation: interpolation
  !
  clearBuf
    ^ClearBuf buf: self
  !
  convolution: kernel value: framesize
    ^Convolution in: self kernel: kernel framesize: framesize
  !
  crackle
    ^Crackle chaosParam: self
  !
  cuspL: a value: b value: xi
    ^CuspL freq: self a: a b: b xi: xi
  !
  dbufrd: phase value: loop
    ^Dbufrd bufnum: self phase: phase loop: loop
  !
  dbufwr: phase value: input value: loop
    ^Dbufwr bufnum: self phase: phase input: input loop: loop
  !
  dc
    ^DC in: self
  !
  diwhite: lo value: hi
    ^Diwhite length: self lo: lo hi: hi
  !
  drand: list
    ^Drand repeats: self list: list
  !
  dseq: list
    ^Dseq repeats: self list: list
  !
  dseries: start value: step
    ^Dseries length: self start: start step: step
  !
  dshuf: list
    ^Dshuf repeats: self list: list
  !
  dust
    ^Dust density: self
  !
  dust2
    ^Dust2 density: self
  !
  duty: reset value: doneAction value: level
    ^Duty dur: self reset: reset doneAction: doneAction level: level
  !
  envGen: levelScale value: levelBias value: timeScale value: doneAction value: envelope
    ^EnvGen gate: self levelScale: levelScale levelBias: levelBias timeScale: timeScale doneAction: doneAction envelope: envelope
  !
  expRand: hi
    ^ExpRand lo: self hi: hi
  !
  fbSineC: im value: fb value: a value: c value: xi value: yi
    ^FBSineC freq: self im: im fb: fb a: a c: c xi: xi yi: yi
  !
  fft: in value: hop value: wintype value: active value: winsize
    ^FFT buffer: self in: in hop: hop wintype: wintype active: active winsize: winsize
  !
  formant: formfreq value: bwfreq
    ^Formant fundfreq: self formfreq: formfreq bwfreq: bwfreq
  !
  freqShift: freq value: phase
    ^FreqShift in: self freq: freq phase: phase
  !
  fSinOsc: iphase
    ^FSinOsc freq: self iphase: iphase
  !
  gendy1: durdist value: adparam value: ddparam value: minfreq value: maxfreq value: ampscale value: durscale value: initCPs value: knum
    ^Gendy1 ampdist: self durdist: durdist adparam: adparam ddparam: ddparam minfreq: minfreq maxfreq: maxfreq ampscale: ampscale durscale: durscale initCPs: initCPs knum: knum
  !
  grainFM: dur value: carfreq value: modfreq value: index value: pan value: envbufnum value: maxGrains
    ^GrainFM trigger: self dur: dur carfreq: carfreq modfreq: modfreq index: index pan: pan envbufnum: envbufnum maxGrains: maxGrains
  !
  grainSin: dur value: freq value: pan value: envbufnum value: maxGrains
    ^GrainSin trigger: self dur: dur freq: freq pan: pan envbufnum: envbufnum maxGrains: maxGrains
  !
  ifft: wintype value: winsize
    ^IFFT buffer: self wintype: wintype winsize: winsize
  !
  impulse: phase
    ^Impulse freq: self phase: phase
  !
  in'
    ^In bus: self
  !
  inFeedback
    ^InFeedback bus: self
  !
  iRand: hi
    ^IRand lo: self hi: hi
  !
  k2a
    ^K2A in: self
  !
  klang: freqoffset value: specificationsArrayRef
    ^Klang freqscale: self freqoffset: freqoffset specificationsArrayRef: specificationsArrayRef
  !
  lfCub: iphase
    ^LFCub freq: self iphase: iphase
  !
  lfdNoise1
    ^LFDNoise1 freq: self
  !
  lfdNoise3
    ^LFDNoise3 freq: self
  !
  lfGauss: width value: iphase value: loop value: doneAction
    ^LFGauss duration: self width: width iphase: iphase loop: loop doneAction: doneAction
  !
  lfNoise0
    ^LFNoise0 freq: self
  !
  lfNoise1
    ^LFNoise1 freq: self
  !
  lfNoise2
    ^LFNoise2 freq: self
  !
  lfPar: iphase
    ^LFPar freq: self iphase: iphase
  !
  lfPulse: iphase value: width
    ^LFPulse freq: self iphase: iphase width: width
  !
  lfSaw: iphase
    ^LFSaw freq: self iphase: iphase
  !
  lfTri: iphase
    ^LFTri freq: self iphase: iphase
  !
  line: end value: dur value: doneAction
    ^Line start: self end: end dur: dur doneAction: doneAction
  !
  linRand: hi value: minmax
    ^LinRand lo: self hi: hi minmax: minmax
  !
  localBuf: numFrames
    ^LocalBuf numChannels: self numFrames: numFrames
  !
  localIn
    ^LocalIn default: self
  !
  membraneCircle: tension value: loss
    ^MembraneCircle excitation: self tension: tension loss: loss
  !
  miRings: trig value: pit value: struct value: bright value: damp value: pos value: model value: poly value: intern_exciter value: easteregg value: bypass
    ^MiRings in: self trig: trig pit: pit struct: struct bright: bright damp: damp pos: pos model: model poly: poly intern_exciter: intern_exciter easteregg: easteregg bypass: bypass
  !
  mouseButton: maxval value: lag
    ^MouseButton minval: self maxval: maxval lag: lag
  !
  mouseX: maxval value: warp value: lag
    ^MouseX minval: self maxval: maxval warp: warp lag: lag
  !
  mouseY: maxval value: warp value: lag
    ^MouseY minval: self maxval: maxval warp: warp lag: lag
  !
  phasor: rate value: start value: end value: resetPos
    ^Phasor trig: self rate: rate start: start end: end resetPos: resetPos
  !
  pitch: initFreq value: minFreq value: maxFreq value: execFreq value: maxBinsPerOctave value: median value: ampThreshold value: peakThreshold value: downSample value: clar
    ^Pitch in: self initFreq: initFreq minFreq: minFreq maxFreq: maxFreq execFreq: execFreq maxBinsPerOctave: maxBinsPerOctave median: median ampThreshold: ampThreshold peakThreshold: peakThreshold downSample: downSample clar: clar
  !
  pulse: width
    ^Pulse freq: self width: width
  !
  pv_RandComb: wipe value: trig
    ^PV_RandComb buffer: self wipe: wipe trig: trig
  !
  rand: hi
    ^Rand lo: self hi: hi
  !
  recordBuf: offset value: recLevel value: preLevel value: run value: loop value: trigger value: doneAction value: inputArray
    ^RecordBuf bufnum: self offset: offset recLevel: recLevel preLevel: preLevel run: run loop: loop trigger: trigger doneAction: doneAction inputArray: inputArray
  !
  rBezier: dx value: freq value: phase value: param
    ^RBezier haltAfter: self dx: dx freq: freq phase: phase param: param
  !
  rdx7Env: data value: r1 value: r2 value: r3 value: r4 value: l1 value: l2 value: l3 value: l4 value: ol
    ^RDX7Env gate: self data: data r1: r1 r2: r2 r3: r3 r4: r4 l1: l1 l2: l2 l3: l3 l4: l4 ol: ol
  !
  rExpRandN: hi
    ^RExpRandN lo: self hi: hi
  !
  rRandN: hi
    ^RRandN lo: self hi: hi
  !
  saw
    ^Saw freq: self
  !
  setBuf: offset value: length value: array
    ^SetBuf buf: self offset: offset length: length array: array
  !
  sinOsc: phase
    ^SinOsc freq: self phase: phase
  !
  sinOscFB: feedback
    ^SinOscFB freq: self feedback: feedback
  !
  syncSaw: sawFreq
    ^SyncSaw syncFreq: self sawFreq: sawFreq
  !
  tDuty: reset value: doneAction value: level value: gapFirst
    ^TDuty dur: self reset: reset doneAction: doneAction level: level gapFirst: gapFirst
  !
  tGrains: bufnum value: rate value: centerPos value: dur value: pan value: amp value: interp
    ^TGrains trigger: self bufnum: bufnum rate: rate centerPos: centerPos dur: dur pan: pan amp: amp interp: interp
  !
  varSaw: iphase value: width
    ^VarSaw freq: self iphase: iphase width: width
  !
  vibrato: rate value: depth value: delay value: onset value: rateVariation value: depthVariation value: iphase value: trig
    ^Vibrato freq: self rate: rate depth: depth delay: delay onset: onset rateVariation: rateVariation depthVariation: depthVariation iphase: iphase trig: trig
  !
  xLine: end value: dur value: doneAction
    ^XLine start: self end: end dur: dur doneAction: doneAction
  !

!
