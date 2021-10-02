UGen subclass: #AllpassC
  instanceVariableNames: 'in maxdelaytime delaytime decaytime'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

AllpassC comment: 'Schroeder allpass delay line with cubic interpolation.'!

!AllpassC methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  maxdelaytime ^maxdelaytime!
  maxdelaytime: x maxdelaytime := x!
  delaytime ^delaytime!
  delaytime: x delaytime := x!
  decaytime ^decaytime!
  decaytime: x decaytime := x!
!

!AllpassC methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #AllpassC.
    paramNames := #(#in #maxdelaytime #delaytime #decaytime).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. maxdelaytime := 0.2. delaytime := 0.2. decaytime := 1.0!
!


!AllpassC class methodsFor: 'instance creation'!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
    |u|
    u := self new.
    u in: in.
    u maxdelaytime: maxdelaytime.
    u delaytime: delaytime.
    u decaytime: decaytime.
    ^u!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul
    |u|
    u := self new.
    u in: in.
    u maxdelaytime: maxdelaytime.
    u delaytime: delaytime.
    u decaytime: decaytime.
    u mul: mul.
    ^u!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u maxdelaytime: maxdelaytime.
    u delaytime: delaytime.
    u decaytime: decaytime.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:maxdelaytime:delaytime:decaytime:!
!

UGen subclass: #AllpassL
  instanceVariableNames: 'in maxdelaytime delaytime decaytime'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

AllpassL comment: 'Schroeder allpass delay line with linear interpolation.'!

!AllpassL methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  maxdelaytime ^maxdelaytime!
  maxdelaytime: x maxdelaytime := x!
  delaytime ^delaytime!
  delaytime: x delaytime := x!
  decaytime ^decaytime!
  decaytime: x decaytime := x!
!

!AllpassL methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #AllpassL.
    paramNames := #(#in #maxdelaytime #delaytime #decaytime).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. maxdelaytime := 0.2. delaytime := 0.2. decaytime := 1.0!
!


!AllpassL class methodsFor: 'instance creation'!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
    |u|
    u := self new.
    u in: in.
    u maxdelaytime: maxdelaytime.
    u delaytime: delaytime.
    u decaytime: decaytime.
    ^u!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul
    |u|
    u := self new.
    u in: in.
    u maxdelaytime: maxdelaytime.
    u delaytime: delaytime.
    u decaytime: decaytime.
    u mul: mul.
    ^u!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u maxdelaytime: maxdelaytime.
    u delaytime: delaytime.
    u decaytime: decaytime.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:maxdelaytime:delaytime:decaytime:!
!

UGen subclass: #AllpassN
  instanceVariableNames: 'in maxdelaytime delaytime decaytime'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

AllpassN comment: 'Schroeder allpass delay line with no interpolation.'!

!AllpassN methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  maxdelaytime ^maxdelaytime!
  maxdelaytime: x maxdelaytime := x!
  delaytime ^delaytime!
  delaytime: x delaytime := x!
  decaytime ^decaytime!
  decaytime: x decaytime := x!
!

!AllpassN methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #AllpassN.
    paramNames := #(#in #maxdelaytime #delaytime #decaytime).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. maxdelaytime := 0.2. delaytime := 0.2. decaytime := 1.0!
!


!AllpassN class methodsFor: 'instance creation'!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
    |u|
    u := self new.
    u in: in.
    u maxdelaytime: maxdelaytime.
    u delaytime: delaytime.
    u decaytime: decaytime.
    ^u!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul
    |u|
    u := self new.
    u in: in.
    u maxdelaytime: maxdelaytime.
    u delaytime: delaytime.
    u decaytime: decaytime.
    u mul: mul.
    ^u!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u maxdelaytime: maxdelaytime.
    u delaytime: delaytime.
    u decaytime: decaytime.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:maxdelaytime:delaytime:decaytime:!
!

UGen subclass: #AmpComp
  instanceVariableNames: 'freq root exp'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

AmpComp comment: 'Basic psychoacoustic amplitude compensation.'!

!AmpComp methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
  root ^root!
  root: x root := x!
  exp ^exp!
  exp: x exp := x!
!

!AmpComp methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #AmpComp.
    paramNames := #(#freq #root #exp).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 0.0. root := 0.0. exp := 0.3333!
!


!AmpComp class methodsFor: 'instance creation'!
  freq: freq root: root exp: exp
    |u|
    u := self new.
    u freq: freq.
    u root: root.
    u exp: exp.
    ^u!
  freq: freq root: root exp: exp mul: mul
    |u|
    u := self new.
    u freq: freq.
    u root: root.
    u exp: exp.
    u mul: mul.
    ^u!
  freq: freq root: root exp: exp mul: mul add: add
    |u|
    u := self new.
    u freq: freq.
    u root: root.
    u exp: exp.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#freq:root:exp:!
!

UGen subclass: #AmpCompA
  instanceVariableNames: 'freq root minAmp rootAmp'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

AmpCompA comment: 'Basic psychoacoustic amplitude compensation (ANSI A-weighting curve).'!

!AmpCompA methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
  root ^root!
  root: x root := x!
  minAmp ^minAmp!
  minAmp: x minAmp := x!
  rootAmp ^rootAmp!
  rootAmp: x rootAmp := x!
!

!AmpCompA methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #AmpCompA.
    paramNames := #(#freq #root #minAmp #rootAmp).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 1000.0. root := 0.0. minAmp := 0.32. rootAmp := 1.0!
!


!AmpCompA class methodsFor: 'instance creation'!
  freq: freq root: root minAmp: minAmp rootAmp: rootAmp
    |u|
    u := self new.
    u freq: freq.
    u root: root.
    u minAmp: minAmp.
    u rootAmp: rootAmp.
    ^u!
  freq: freq root: root minAmp: minAmp rootAmp: rootAmp mul: mul
    |u|
    u := self new.
    u freq: freq.
    u root: root.
    u minAmp: minAmp.
    u rootAmp: rootAmp.
    u mul: mul.
    ^u!
  freq: freq root: root minAmp: minAmp rootAmp: rootAmp mul: mul add: add
    |u|
    u := self new.
    u freq: freq.
    u root: root.
    u minAmp: minAmp.
    u rootAmp: rootAmp.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#freq:root:minAmp:rootAmp:!
!

UGen subclass: #Amplitude
  instanceVariableNames: 'in attackTime releaseTime'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Amplitude comment: 'Amplitude follower'!

!Amplitude methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  attackTime ^attackTime!
  attackTime: x attackTime := x!
  releaseTime ^releaseTime!
  releaseTime: x releaseTime := x!
!

!Amplitude methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Amplitude.
    paramNames := #(#in #attackTime #releaseTime).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. attackTime := 0.01. releaseTime := 0.01!
!


!Amplitude class methodsFor: 'instance creation'!
  in: in attackTime: attackTime releaseTime: releaseTime
    |u|
    u := self new.
    u in: in.
    u attackTime: attackTime.
    u releaseTime: releaseTime.
    ^u!
  in: in attackTime: attackTime releaseTime: releaseTime mul: mul
    |u|
    u := self new.
    u in: in.
    u attackTime: attackTime.
    u releaseTime: releaseTime.
    u mul: mul.
    ^u!
  in: in attackTime: attackTime releaseTime: releaseTime mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u attackTime: attackTime.
    u releaseTime: releaseTime.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:attackTime:releaseTime:!
!

UGen subclass: #BBandPass
  instanceVariableNames: 'in freq bw'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

BBandPass comment: 'Band Pass Filter'!

!BBandPass methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  freq ^freq!
  freq: x freq := x!
  bw ^bw!
  bw: x bw := x!
!

!BBandPass methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #BBandPass.
    paramNames := #(#in #freq #bw).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. freq := 1200.0. bw := 1.0!
!


!BBandPass class methodsFor: 'instance creation'!
  in: in freq: freq bw: bw
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u bw: bw.
    ^u!
  in: in freq: freq bw: bw mul: mul
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u bw: bw.
    u mul: mul.
    ^u!
  in: in freq: freq bw: bw mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u bw: bw.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:freq:bw:!
!

UGen subclass: #BBandStop
  instanceVariableNames: 'in freq bw'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

BBandStop comment: 'Band reject filter'!

!BBandStop methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  freq ^freq!
  freq: x freq := x!
  bw ^bw!
  bw: x bw := x!
!

!BBandStop methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #BBandStop.
    paramNames := #(#in #freq #bw).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. freq := 1200.0. bw := 1.0!
!


!BBandStop class methodsFor: 'instance creation'!
  in: in freq: freq bw: bw
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u bw: bw.
    ^u!
  in: in freq: freq bw: bw mul: mul
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u bw: bw.
    u mul: mul.
    ^u!
  in: in freq: freq bw: bw mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u bw: bw.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:freq:bw:!
!

UGen subclass: #Blip
  instanceVariableNames: 'freq numharm'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Blip comment: 'Band limited impulse oscillator.'!

!Blip methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
  numharm ^numharm!
  numharm: x numharm := x!
!

!Blip methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Blip.
    paramNames := #(#freq #numharm).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 440.0. numharm := 200.0!
!


!Blip class methodsFor: 'instance creation'!
  freq: freq numharm: numharm
    |u|
    u := self new.
    u freq: freq.
    u numharm: numharm.
    ^u!
  freq: freq numharm: numharm mul: mul
    |u|
    u := self new.
    u freq: freq.
    u numharm: numharm.
    u mul: mul.
    ^u!
  freq: freq numharm: numharm mul: mul add: add
    |u|
    u := self new.
    u freq: freq.
    u numharm: numharm.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#freq:numharm:!
!

UGen subclass: #BLowPass
  instanceVariableNames: 'in freq rq'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

BLowPass comment: '12db/oct rolloff - 2nd order resonant Low Pass Filter'!

!BLowPass methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  freq ^freq!
  freq: x freq := x!
  rq ^rq!
  rq: x rq := x!
!

!BLowPass methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #BLowPass.
    paramNames := #(#in #freq #rq).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. freq := 1200.0. rq := 1.0!
!


!BLowPass class methodsFor: 'instance creation'!
  in: in freq: freq rq: rq
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u rq: rq.
    ^u!
  in: in freq: freq rq: rq mul: mul
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u rq: rq.
    u mul: mul.
    ^u!
  in: in freq: freq rq: rq mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u rq: rq.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:freq:rq:!
!

UGen subclass: #BPF
  instanceVariableNames: 'in freq rq'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

BPF comment: '2nd order Butterworth bandpass filter.'!

!BPF methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  freq ^freq!
  freq: x freq := x!
  rq ^rq!
  rq: x rq := x!
!

!BPF methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #BPF.
    paramNames := #(#in #freq #rq).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. freq := 440.0. rq := 1.0!
!


!BPF class methodsFor: 'instance creation'!
  in: in freq: freq rq: rq
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u rq: rq.
    ^u!
  in: in freq: freq rq: rq mul: mul
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u rq: rq.
    u mul: mul.
    ^u!
  in: in freq: freq rq: rq mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u rq: rq.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:freq:rq:!
!

UGen subclass: #BPZ2
  instanceVariableNames: 'in'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

BPZ2 comment: 'Two zero fixed midpass.'!

!BPZ2 methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
!

!BPZ2 methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #BPZ2.
    paramNames := #(#in).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0!
!


!BPZ2 class methodsFor: 'instance creation'!
  in: in
    |u|
    u := self new.
    u in: in.
    ^u!
  in: in mul: mul
    |u|
    u := self new.
    u in: in.
    u mul: mul.
    ^u!
  in: in mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:!
!

UGen subclass: #BRF
  instanceVariableNames: 'in freq rq'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

BRF comment: '2nd order Butterworth band reject filter.'!

!BRF methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  freq ^freq!
  freq: x freq := x!
  rq ^rq!
  rq: x rq := x!
!

!BRF methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #BRF.
    paramNames := #(#in #freq #rq).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. freq := 440.0. rq := 1.0!
!


!BRF class methodsFor: 'instance creation'!
  in: in freq: freq rq: rq
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u rq: rq.
    ^u!
  in: in freq: freq rq: rq mul: mul
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u rq: rq.
    u mul: mul.
    ^u!
  in: in freq: freq rq: rq mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u rq: rq.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:freq:rq:!
!

UGen subclass: #BrownNoise
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

BrownNoise comment: 'Brown Noise.'!


!BrownNoise methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #BrownNoise.
    paramNames := #().
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    !
!


!BrownNoise class methodsFor: 'instance creation'!
  mul: mul
    |u|
    u := self new.
    u mul: mul.
    ^u!
  mul: mul add: add
    |u|
    u := self new.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#new!
!

UGen subclass: #BufRd
  instanceVariableNames: 'bufnum phase loop interpolation'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

BufRd comment: 'Buffer reading oscillator.'!

!BufRd methodsFor: 'accessing'!
  bufnum ^bufnum!
  bufnum: x bufnum := x!
  phase ^phase!
  phase: x phase := x!
  loop ^loop!
  loop: x loop := x!
  interpolation ^interpolation!
  interpolation: x interpolation := x!
!

!BufRd methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #BufRd.
    paramNames := #(#bufnum #phase #loop #interpolation).
    mceNames := #().
    ugenRate := #ar.
    numChan := nil.
    specialIndex := 0.
    ugenId := nil.
    bufnum := 0.0. phase := 0.0. loop := 1.0. interpolation := 2.0!
!


!BufRd class methodsFor: 'instance creation'!
  numChan: numChan bufnum: bufnum phase: phase loop: loop interpolation: interpolation
    |u|
    u := self new.
    u numChan: numChan.
    u bufnum: bufnum.
    u phase: phase.
    u loop: loop.
    u interpolation: interpolation.
    ^u!
  numChan: numChan bufnum: bufnum phase: phase loop: loop interpolation: interpolation mul: mul
    |u|
    u := self new.
    u numChan: numChan.
    u bufnum: bufnum.
    u phase: phase.
    u loop: loop.
    u interpolation: interpolation.
    u mul: mul.
    ^u!
  numChan: numChan bufnum: bufnum phase: phase loop: loop interpolation: interpolation mul: mul add: add
    |u|
    u := self new.
    u numChan: numChan.
    u bufnum: bufnum.
    u phase: phase.
    u loop: loop.
    u interpolation: interpolation.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#numChan:bufnum:phase:loop:interpolation:!
!

UGen subclass: #BufWr
  instanceVariableNames: 'bufnum phase loop inputArray'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

BufWr comment: 'Buffer writing oscillator.'!

!BufWr methodsFor: 'accessing'!
  bufnum ^bufnum!
  bufnum: x bufnum := x!
  phase ^phase!
  phase: x phase := x!
  loop ^loop!
  loop: x loop := x!
  inputArray ^inputArray!
  inputArray: x inputArray := x!
!

!BufWr methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #BufWr.
    paramNames := #(#bufnum #phase #loop).
    mceNames := #(#inputArray).
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    bufnum := 0.0. phase := 0.0. loop := 1.0!
!


!BufWr class methodsFor: 'instance creation'!
  bufnum: bufnum phase: phase loop: loop inputArray: inputArray
    |u|
    u := self new.
    u bufnum: bufnum.
    u phase: phase.
    u loop: loop.
    u inputArray: inputArray.
    ^u!
  bufnum: bufnum phase: phase loop: loop inputArray: inputArray mul: mul
    |u|
    u := self new.
    u bufnum: bufnum.
    u phase: phase.
    u loop: loop.
    u inputArray: inputArray.
    u mul: mul.
    ^u!
  bufnum: bufnum phase: phase loop: loop inputArray: inputArray mul: mul add: add
    |u|
    u := self new.
    u bufnum: bufnum.
    u phase: phase.
    u loop: loop.
    u inputArray: inputArray.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#bufnum:phase:loop:inputArray:!
!

UGen subclass: #ClearBuf
  instanceVariableNames: 'buf'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

ClearBuf comment: '(Undocumented class)'!

!ClearBuf methodsFor: 'accessing'!
  buf ^buf!
  buf: x buf := x!
!

!ClearBuf methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #ClearBuf.
    paramNames := #(#buf).
    mceNames := #().
    ugenRate := #ir.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    buf := 0.0!
!


!ClearBuf class methodsFor: 'instance creation'!
  buf: buf
    |u|
    u := self new.
    u buf: buf.
    ^u!
  buf: buf mul: mul
    |u|
    u := self new.
    u buf: buf.
    u mul: mul.
    ^u!
  buf: buf mul: mul add: add
    |u|
    u := self new.
    u buf: buf.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#buf:!
!

UGen subclass: #Clip
  instanceVariableNames: 'in lo hi'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Clip comment: 'Clip a signal outside given thresholds.'!

!Clip methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  lo ^lo!
  lo: x lo := x!
  hi ^hi!
  hi: x hi := x!
!

!Clip methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Clip.
    paramNames := #(#in #lo #hi).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. lo := 0.0. hi := 1.0!
!


!Clip class methodsFor: 'instance creation'!
  in: in lo: lo hi: hi
    |u|
    u := self new.
    u in: in.
    u lo: lo.
    u hi: hi.
    ^u!
  in: in lo: lo hi: hi mul: mul
    |u|
    u := self new.
    u in: in.
    u lo: lo.
    u hi: hi.
    u mul: mul.
    ^u!
  in: in lo: lo hi: hi mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u lo: lo.
    u hi: hi.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:lo:hi:!
!

UGen subclass: #CombC
  instanceVariableNames: 'in maxdelaytime delaytime decaytime'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

CombC comment: 'Comb delay line with cubic interpolation.'!

!CombC methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  maxdelaytime ^maxdelaytime!
  maxdelaytime: x maxdelaytime := x!
  delaytime ^delaytime!
  delaytime: x delaytime := x!
  decaytime ^decaytime!
  decaytime: x decaytime := x!
!

!CombC methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #CombC.
    paramNames := #(#in #maxdelaytime #delaytime #decaytime).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. maxdelaytime := 0.2. delaytime := 0.2. decaytime := 1.0!
!


!CombC class methodsFor: 'instance creation'!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
    |u|
    u := self new.
    u in: in.
    u maxdelaytime: maxdelaytime.
    u delaytime: delaytime.
    u decaytime: decaytime.
    ^u!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul
    |u|
    u := self new.
    u in: in.
    u maxdelaytime: maxdelaytime.
    u delaytime: delaytime.
    u decaytime: decaytime.
    u mul: mul.
    ^u!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u maxdelaytime: maxdelaytime.
    u delaytime: delaytime.
    u decaytime: decaytime.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:maxdelaytime:delaytime:decaytime:!
!

UGen subclass: #CombL
  instanceVariableNames: 'in maxdelaytime delaytime decaytime'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

CombL comment: 'Comb delay line with linear interpolation.'!

!CombL methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  maxdelaytime ^maxdelaytime!
  maxdelaytime: x maxdelaytime := x!
  delaytime ^delaytime!
  delaytime: x delaytime := x!
  decaytime ^decaytime!
  decaytime: x decaytime := x!
!

!CombL methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #CombL.
    paramNames := #(#in #maxdelaytime #delaytime #decaytime).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. maxdelaytime := 0.2. delaytime := 0.2. decaytime := 1.0!
!


!CombL class methodsFor: 'instance creation'!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
    |u|
    u := self new.
    u in: in.
    u maxdelaytime: maxdelaytime.
    u delaytime: delaytime.
    u decaytime: decaytime.
    ^u!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul
    |u|
    u := self new.
    u in: in.
    u maxdelaytime: maxdelaytime.
    u delaytime: delaytime.
    u decaytime: decaytime.
    u mul: mul.
    ^u!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u maxdelaytime: maxdelaytime.
    u delaytime: delaytime.
    u decaytime: decaytime.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:maxdelaytime:delaytime:decaytime:!
!

UGen subclass: #CombN
  instanceVariableNames: 'in maxdelaytime delaytime decaytime'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

CombN comment: 'Comb delay line with no interpolation.'!

!CombN methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  maxdelaytime ^maxdelaytime!
  maxdelaytime: x maxdelaytime := x!
  delaytime ^delaytime!
  delaytime: x delaytime := x!
  decaytime ^decaytime!
  decaytime: x decaytime := x!
!

!CombN methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #CombN.
    paramNames := #(#in #maxdelaytime #delaytime #decaytime).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. maxdelaytime := 0.2. delaytime := 0.2. decaytime := 1.0!
!


!CombN class methodsFor: 'instance creation'!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
    |u|
    u := self new.
    u in: in.
    u maxdelaytime: maxdelaytime.
    u delaytime: delaytime.
    u decaytime: decaytime.
    ^u!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul
    |u|
    u := self new.
    u in: in.
    u maxdelaytime: maxdelaytime.
    u delaytime: delaytime.
    u decaytime: decaytime.
    u mul: mul.
    ^u!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u maxdelaytime: maxdelaytime.
    u delaytime: delaytime.
    u decaytime: decaytime.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:maxdelaytime:delaytime:decaytime:!
!

UGen subclass: #ControlDur
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

ControlDur comment: 'Duration of one block'!


!ControlDur methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #ControlDur.
    paramNames := #().
    mceNames := #().
    ugenRate := #ir.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    !
!


!ControlDur class methodsFor: 'instance creation'!
  mul: mul
    |u|
    u := self new.
    u mul: mul.
    ^u!
  mul: mul add: add
    |u|
    u := self new.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#new!
!

UGen subclass: #Crackle
  instanceVariableNames: 'chaosParam'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Crackle comment: 'Chaotic noise function.'!

!Crackle methodsFor: 'accessing'!
  chaosParam ^chaosParam!
  chaosParam: x chaosParam := x!
!

!Crackle methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Crackle.
    paramNames := #(#chaosParam).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    chaosParam := 1.5!
!


!Crackle class methodsFor: 'instance creation'!
  chaosParam: chaosParam
    |u|
    u := self new.
    u chaosParam: chaosParam.
    ^u!
  chaosParam: chaosParam mul: mul
    |u|
    u := self new.
    u chaosParam: chaosParam.
    u mul: mul.
    ^u!
  chaosParam: chaosParam mul: mul add: add
    |u|
    u := self new.
    u chaosParam: chaosParam.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#chaosParam:!
!

UGen subclass: #CuspL
  instanceVariableNames: 'freq a b xi'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

CuspL comment: 'Cusp map chaotic generator'!

!CuspL methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
  a ^a!
  a: x a := x!
  b ^b!
  b: x b := x!
  xi ^xi!
  xi: x xi := x!
!

!CuspL methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #CuspL.
    paramNames := #(#freq #a #b #xi).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 22050.0. a := 1.0. b := 1.9. xi := 0.0!
!


!CuspL class methodsFor: 'instance creation'!
  freq: freq a: a b: b xi: xi
    |u|
    u := self new.
    u freq: freq.
    u a: a.
    u b: b.
    u xi: xi.
    ^u!
  freq: freq a: a b: b xi: xi mul: mul
    |u|
    u := self new.
    u freq: freq.
    u a: a.
    u b: b.
    u xi: xi.
    u mul: mul.
    ^u!
  freq: freq a: a b: b xi: xi mul: mul add: add
    |u|
    u := self new.
    u freq: freq.
    u a: a.
    u b: b.
    u xi: xi.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#freq:a:b:xi:!
!

UGen subclass: #Dbufrd
  instanceVariableNames: 'bufnum phase loop'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Dbufrd comment: 'Buffer read demand ugen'!

!Dbufrd methodsFor: 'accessing'!
  bufnum ^bufnum!
  bufnum: x bufnum := x!
  phase ^phase!
  phase: x phase := x!
  loop ^loop!
  loop: x loop := x!
!

!Dbufrd methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Dbufrd.
    paramNames := #(#bufnum #phase #loop).
    mceNames := #().
    ugenRate := #dr.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    bufnum := 0.0. phase := 0.0. loop := 1.0!
!


!Dbufrd class methodsFor: 'instance creation'!
  bufnum: bufnum phase: phase loop: loop
    |u|
    u := self new.
    u bufnum: bufnum.
    u phase: phase.
    u loop: loop.
    ^u!
  bufnum: bufnum phase: phase loop: loop mul: mul
    |u|
    u := self new.
    u bufnum: bufnum.
    u phase: phase.
    u loop: loop.
    u mul: mul.
    ^u!
  bufnum: bufnum phase: phase loop: loop mul: mul add: add
    |u|
    u := self new.
    u bufnum: bufnum.
    u phase: phase.
    u loop: loop.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#bufnum:phase:loop:!
!

UGen subclass: #Dbufwr
  instanceVariableNames: 'bufnum phase input loop'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Dbufwr comment: 'Buffer write demand ugen'!

!Dbufwr methodsFor: 'accessing'!
  bufnum ^bufnum!
  bufnum: x bufnum := x!
  phase ^phase!
  phase: x phase := x!
  input ^input!
  input: x input := x!
  loop ^loop!
  loop: x loop := x!
!

!Dbufwr methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Dbufwr.
    paramNames := #(#bufnum #phase #input #loop).
    mceNames := #().
    ugenRate := #dr.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    bufnum := 0.0. phase := 0.0. input := 0.0. loop := 1.0!
!


!Dbufwr class methodsFor: 'instance creation'!
  bufnum: bufnum phase: phase input: input loop: loop
    |u|
    u := self new.
    u bufnum: bufnum.
    u phase: phase.
    u input: input.
    u loop: loop.
    ^u!
  bufnum: bufnum phase: phase input: input loop: loop mul: mul
    |u|
    u := self new.
    u bufnum: bufnum.
    u phase: phase.
    u input: input.
    u loop: loop.
    u mul: mul.
    ^u!
  bufnum: bufnum phase: phase input: input loop: loop mul: mul add: add
    |u|
    u := self new.
    u bufnum: bufnum.
    u phase: phase.
    u input: input.
    u loop: loop.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#bufnum:phase:input:loop:!
!

UGen subclass: #Decay
  instanceVariableNames: 'in decayTime'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Decay comment: 'Exponential decay'!

!Decay methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  decayTime ^decayTime!
  decayTime: x decayTime := x!
!

!Decay methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Decay.
    paramNames := #(#in #decayTime).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. decayTime := 1.0!
!


!Decay class methodsFor: 'instance creation'!
  in: in decayTime: decayTime
    |u|
    u := self new.
    u in: in.
    u decayTime: decayTime.
    ^u!
  in: in decayTime: decayTime mul: mul
    |u|
    u := self new.
    u in: in.
    u decayTime: decayTime.
    u mul: mul.
    ^u!
  in: in decayTime: decayTime mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u decayTime: decayTime.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:decayTime:!
!

UGen subclass: #Decay2
  instanceVariableNames: 'in attackTime decayTime'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Decay2 comment: 'Exponential decay'!

!Decay2 methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  attackTime ^attackTime!
  attackTime: x attackTime := x!
  decayTime ^decayTime!
  decayTime: x decayTime := x!
!

!Decay2 methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Decay2.
    paramNames := #(#in #attackTime #decayTime).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. attackTime := 0.01. decayTime := 1.0!
!


!Decay2 class methodsFor: 'instance creation'!
  in: in attackTime: attackTime decayTime: decayTime
    |u|
    u := self new.
    u in: in.
    u attackTime: attackTime.
    u decayTime: decayTime.
    ^u!
  in: in attackTime: attackTime decayTime: decayTime mul: mul
    |u|
    u := self new.
    u in: in.
    u attackTime: attackTime.
    u decayTime: decayTime.
    u mul: mul.
    ^u!
  in: in attackTime: attackTime decayTime: decayTime mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u attackTime: attackTime.
    u decayTime: decayTime.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:attackTime:decayTime:!
!

UGen subclass: #DegreeToKey
  instanceVariableNames: 'bufnum in octave'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

DegreeToKey comment: 'Convert signal to modal pitch.'!

!DegreeToKey methodsFor: 'accessing'!
  bufnum ^bufnum!
  bufnum: x bufnum := x!
  in ^in!
  in: x in := x!
  octave ^octave!
  octave: x octave := x!
!

!DegreeToKey methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #DegreeToKey.
    paramNames := #(#bufnum #in #octave).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    bufnum := 0.0. in := 0.0. octave := 12.0!
!


!DegreeToKey class methodsFor: 'instance creation'!
  bufnum: bufnum in: in octave: octave
    |u|
    u := self new.
    u bufnum: bufnum.
    u in: in.
    u octave: octave.
    ^u!
  bufnum: bufnum in: in octave: octave mul: mul
    |u|
    u := self new.
    u bufnum: bufnum.
    u in: in.
    u octave: octave.
    u mul: mul.
    ^u!
  bufnum: bufnum in: in octave: octave mul: mul add: add
    |u|
    u := self new.
    u bufnum: bufnum.
    u in: in.
    u octave: octave.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#bufnum:in:octave:!
!

UGen subclass: #DelayN
  instanceVariableNames: 'in maxdelaytime delaytime'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

DelayN comment: 'Simple delay line with no interpolation.'!

!DelayN methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  maxdelaytime ^maxdelaytime!
  maxdelaytime: x maxdelaytime := x!
  delaytime ^delaytime!
  delaytime: x delaytime := x!
!

!DelayN methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #DelayN.
    paramNames := #(#in #maxdelaytime #delaytime).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. maxdelaytime := 0.2. delaytime := 0.2!
!


!DelayN class methodsFor: 'instance creation'!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime
    |u|
    u := self new.
    u in: in.
    u maxdelaytime: maxdelaytime.
    u delaytime: delaytime.
    ^u!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime mul: mul
    |u|
    u := self new.
    u in: in.
    u maxdelaytime: maxdelaytime.
    u delaytime: delaytime.
    u mul: mul.
    ^u!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u maxdelaytime: maxdelaytime.
    u delaytime: delaytime.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:maxdelaytime:delaytime:!
!

UGen subclass: #Demand
  instanceVariableNames: 'trig reset demandUGens'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Demand comment: 'Demand results from demand rate UGens.'!

!Demand methodsFor: 'accessing'!
  trig ^trig!
  trig: x trig := x!
  reset ^reset!
  reset: x reset := x!
  demandUGens ^demandUGens!
  demandUGens: x demandUGens := x!
!

!Demand methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Demand.
    paramNames := #(#trig #reset).
    mceNames := #(#demandUGens).
    ugenRate := #ar.
    numChan := #mceInput.
    specialIndex := 0.
    ugenId := nil.
    trig := 0.0. reset := 0.0!
!


!Demand class methodsFor: 'instance creation'!
  trig: trig reset: reset demandUGens: demandUGens
    |u|
    u := self new.
    u trig: trig.
    u reset: reset.
    u demandUGens: demandUGens.
    ^u!
  trig: trig reset: reset demandUGens: demandUGens mul: mul
    |u|
    u := self new.
    u trig: trig.
    u reset: reset.
    u demandUGens: demandUGens.
    u mul: mul.
    ^u!
  trig: trig reset: reset demandUGens: demandUGens mul: mul add: add
    |u|
    u := self new.
    u trig: trig.
    u reset: reset.
    u demandUGens: demandUGens.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#trig:reset:demandUGens:!
!

UGen subclass: #DetectSilence
  instanceVariableNames: 'in amp time doneAction'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

DetectSilence comment: 'Detect when input falls below an amplitude threshold'!

!DetectSilence methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  amp ^amp!
  amp: x amp := x!
  time ^time!
  time: x time := x!
  doneAction ^doneAction!
  doneAction: x doneAction := x!
!

!DetectSilence methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #DetectSilence.
    paramNames := #(#in #amp #time #doneAction).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. amp := 0.0001. time := 0.1. doneAction := 0.0!
!


!DetectSilence class methodsFor: 'instance creation'!
  in: in amp: amp time: time doneAction: doneAction
    |u|
    u := self new.
    u in: in.
    u amp: amp.
    u time: time.
    u doneAction: doneAction.
    ^u!
  in: in amp: amp time: time doneAction: doneAction mul: mul
    |u|
    u := self new.
    u in: in.
    u amp: amp.
    u time: time.
    u doneAction: doneAction.
    u mul: mul.
    ^u!
  in: in amp: amp time: time doneAction: doneAction mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u amp: amp.
    u time: time.
    u doneAction: doneAction.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:amp:time:doneAction:!
!

UGen subclass: #Diwhite
  instanceVariableNames: 'length lo hi'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Diwhite comment: 'Demand rate white noise random generator.'!

!Diwhite methodsFor: 'accessing'!
  length ^length!
  length: x length := x!
  lo ^lo!
  lo: x lo := x!
  hi ^hi!
  hi: x hi := x!
!

!Diwhite methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Diwhite.
    paramNames := #(#length #lo #hi).
    mceNames := #().
    ugenRate := #dr.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    length := 100000000.0. lo := 0.0. hi := 1.0!
!


!Diwhite class methodsFor: 'instance creation'!
  length: length lo: lo hi: hi
    |u|
    u := self new.
    u length: length.
    u lo: lo.
    u hi: hi.
    ^u!
  length: length lo: lo hi: hi mul: mul
    |u|
    u := self new.
    u length: length.
    u lo: lo.
    u hi: hi.
    u mul: mul.
    ^u!
  length: length lo: lo hi: hi mul: mul add: add
    |u|
    u := self new.
    u length: length.
    u lo: lo.
    u hi: hi.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#length:lo:hi:!
!

UGen subclass: #Drand
  instanceVariableNames: 'repeats list'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Drand comment: 'Demand rate random sequence generator.'!

!Drand methodsFor: 'accessing'!
  repeats ^repeats!
  repeats: x repeats := x!
  list ^list!
  list: x list := x!
!

!Drand methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Drand.
    paramNames := #(#repeats).
    mceNames := #(#list).
    ugenRate := #dr.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    repeats := 1.0!
!


!Drand class methodsFor: 'instance creation'!
  repeats: repeats list: list
    |u|
    u := self new.
    u repeats: repeats.
    u list: list.
    ^u!
  repeats: repeats list: list mul: mul
    |u|
    u := self new.
    u repeats: repeats.
    u list: list.
    u mul: mul.
    ^u!
  repeats: repeats list: list mul: mul add: add
    |u|
    u := self new.
    u repeats: repeats.
    u list: list.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#repeats:list:!
!

UGen subclass: #Dseq
  instanceVariableNames: 'repeats list'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Dseq comment: 'Demand rate sequence generator.'!

!Dseq methodsFor: 'accessing'!
  repeats ^repeats!
  repeats: x repeats := x!
  list ^list!
  list: x list := x!
!

!Dseq methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Dseq.
    paramNames := #(#repeats).
    mceNames := #(#list).
    ugenRate := #dr.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    repeats := 1.0!
!


!Dseq class methodsFor: 'instance creation'!
  repeats: repeats list: list
    |u|
    u := self new.
    u repeats: repeats.
    u list: list.
    ^u!
  repeats: repeats list: list mul: mul
    |u|
    u := self new.
    u repeats: repeats.
    u list: list.
    u mul: mul.
    ^u!
  repeats: repeats list: list mul: mul add: add
    |u|
    u := self new.
    u repeats: repeats.
    u list: list.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#repeats:list:!
!

UGen subclass: #Dseries
  instanceVariableNames: 'length start step'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Dseries comment: 'Demand rate arithmetic series UGen.'!

!Dseries methodsFor: 'accessing'!
  length ^length!
  length: x length := x!
  start ^start!
  start: x start := x!
  step ^step!
  step: x step := x!
!

!Dseries methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Dseries.
    paramNames := #(#length #start #step).
    mceNames := #().
    ugenRate := #dr.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    length := 100000000.0. start := 1.0. step := 1.0!
!


!Dseries class methodsFor: 'instance creation'!
  length: length start: start step: step
    |u|
    u := self new.
    u length: length.
    u start: start.
    u step: step.
    ^u!
  length: length start: start step: step mul: mul
    |u|
    u := self new.
    u length: length.
    u start: start.
    u step: step.
    u mul: mul.
    ^u!
  length: length start: start step: step mul: mul add: add
    |u|
    u := self new.
    u length: length.
    u start: start.
    u step: step.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#length:start:step:!
!

UGen subclass: #Dshuf
  instanceVariableNames: 'repeats list'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Dshuf comment: 'Demand rate random sequence generator'!

!Dshuf methodsFor: 'accessing'!
  repeats ^repeats!
  repeats: x repeats := x!
  list ^list!
  list: x list := x!
!

!Dshuf methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Dshuf.
    paramNames := #(#repeats).
    mceNames := #(#list).
    ugenRate := #dr.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    repeats := 1.0!
!


!Dshuf class methodsFor: 'instance creation'!
  repeats: repeats list: list
    |u|
    u := self new.
    u repeats: repeats.
    u list: list.
    ^u!
  repeats: repeats list: list mul: mul
    |u|
    u := self new.
    u repeats: repeats.
    u list: list.
    u mul: mul.
    ^u!
  repeats: repeats list: list mul: mul add: add
    |u|
    u := self new.
    u repeats: repeats.
    u list: list.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#repeats:list:!
!

UGen subclass: #Dust
  instanceVariableNames: 'density'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Dust comment: 'Random impulses.'!

!Dust methodsFor: 'accessing'!
  density ^density!
  density: x density := x!
!

!Dust methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Dust.
    paramNames := #(#density).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    density := 0.0!
!


!Dust class methodsFor: 'instance creation'!
  density: density
    |u|
    u := self new.
    u density: density.
    ^u!
  density: density mul: mul
    |u|
    u := self new.
    u density: density.
    u mul: mul.
    ^u!
  density: density mul: mul add: add
    |u|
    u := self new.
    u density: density.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#density:!
!

UGen subclass: #Dust2
  instanceVariableNames: 'density'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Dust2 comment: 'Random impulses.'!

!Dust2 methodsFor: 'accessing'!
  density ^density!
  density: x density := x!
!

!Dust2 methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Dust2.
    paramNames := #(#density).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    density := 0.0!
!


!Dust2 class methodsFor: 'instance creation'!
  density: density
    |u|
    u := self new.
    u density: density.
    ^u!
  density: density mul: mul
    |u|
    u := self new.
    u density: density.
    u mul: mul.
    ^u!
  density: density mul: mul add: add
    |u|
    u := self new.
    u density: density.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#density:!
!

UGen subclass: #Duty
  instanceVariableNames: 'dur reset doneAction level'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Duty comment: 'Demand results from demand rate UGens.'!

!Duty methodsFor: 'accessing'!
  dur ^dur!
  dur: x dur := x!
  reset ^reset!
  reset: x reset := x!
  doneAction ^doneAction!
  doneAction: x doneAction := x!
  level ^level!
  level: x level := x!
!

!Duty methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Duty.
    paramNames := #(#dur #reset #doneAction #level).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    dur := 1.0. reset := 0.0. doneAction := 0.0. level := 1.0!
!


!Duty class methodsFor: 'instance creation'!
  dur: dur reset: reset doneAction: doneAction level: level
    |u|
    u := self new.
    u dur: dur.
    u reset: reset.
    u doneAction: doneAction.
    u level: level.
    ^u!
  dur: dur reset: reset doneAction: doneAction level: level mul: mul
    |u|
    u := self new.
    u dur: dur.
    u reset: reset.
    u doneAction: doneAction.
    u level: level.
    u mul: mul.
    ^u!
  dur: dur reset: reset doneAction: doneAction level: level mul: mul add: add
    |u|
    u := self new.
    u dur: dur.
    u reset: reset.
    u doneAction: doneAction.
    u level: level.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#dur:reset:doneAction:level:!
!

UGen subclass: #EnvGen
  instanceVariableNames: 'gate levelScale levelBias timeScale doneAction envelope'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

EnvGen comment: 'Envelope generator'!

!EnvGen methodsFor: 'accessing'!
  gate ^gate!
  gate: x gate := x!
  levelScale ^levelScale!
  levelScale: x levelScale := x!
  levelBias ^levelBias!
  levelBias: x levelBias := x!
  timeScale ^timeScale!
  timeScale: x timeScale := x!
  doneAction ^doneAction!
  doneAction: x doneAction := x!
  envelope ^envelope!
  envelope: x envelope := x!
!

!EnvGen methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #EnvGen.
    paramNames := #(#gate #levelScale #levelBias #timeScale #doneAction).
    mceNames := #(#envelope).
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    gate := 1.0. levelScale := 1.0. levelBias := 0.0. timeScale := 1.0. doneAction := 0.0!
!


!EnvGen class methodsFor: 'instance creation'!
  gate: gate levelScale: levelScale levelBias: levelBias timeScale: timeScale doneAction: doneAction envelope: envelope
    |u|
    u := self new.
    u gate: gate.
    u levelScale: levelScale.
    u levelBias: levelBias.
    u timeScale: timeScale.
    u doneAction: doneAction.
    u envelope: envelope.
    ^u!
  gate: gate levelScale: levelScale levelBias: levelBias timeScale: timeScale doneAction: doneAction envelope: envelope mul: mul
    |u|
    u := self new.
    u gate: gate.
    u levelScale: levelScale.
    u levelBias: levelBias.
    u timeScale: timeScale.
    u doneAction: doneAction.
    u envelope: envelope.
    u mul: mul.
    ^u!
  gate: gate levelScale: levelScale levelBias: levelBias timeScale: timeScale doneAction: doneAction envelope: envelope mul: mul add: add
    |u|
    u := self new.
    u gate: gate.
    u levelScale: levelScale.
    u levelBias: levelBias.
    u timeScale: timeScale.
    u doneAction: doneAction.
    u envelope: envelope.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#gate:levelScale:levelBias:timeScale:doneAction:envelope:!
!

UGen subclass: #ExpRand
  instanceVariableNames: 'lo hi'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

ExpRand comment: 'Exponential single random number generator.'!

!ExpRand methodsFor: 'accessing'!
  lo ^lo!
  lo: x lo := x!
  hi ^hi!
  hi: x hi := x!
!

!ExpRand methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #ExpRand.
    paramNames := #(#lo #hi).
    mceNames := #().
    ugenRate := #ir.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    lo := 0.01. hi := 1.0!
!


!ExpRand class methodsFor: 'instance creation'!
  lo: lo hi: hi
    |u|
    u := self new.
    u lo: lo.
    u hi: hi.
    ^u!
  lo: lo hi: hi mul: mul
    |u|
    u := self new.
    u lo: lo.
    u hi: hi.
    u mul: mul.
    ^u!
  lo: lo hi: hi mul: mul add: add
    |u|
    u := self new.
    u lo: lo.
    u hi: hi.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#lo:hi:!
!

UGen subclass: #Formant
  instanceVariableNames: 'fundfreq formfreq bwfreq'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Formant comment: 'Formant oscillator'!

!Formant methodsFor: 'accessing'!
  fundfreq ^fundfreq!
  fundfreq: x fundfreq := x!
  formfreq ^formfreq!
  formfreq: x formfreq := x!
  bwfreq ^bwfreq!
  bwfreq: x bwfreq := x!
!

!Formant methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Formant.
    paramNames := #(#fundfreq #formfreq #bwfreq).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    fundfreq := 440.0. formfreq := 1760.0. bwfreq := 880.0!
!


!Formant class methodsFor: 'instance creation'!
  fundfreq: fundfreq formfreq: formfreq bwfreq: bwfreq
    |u|
    u := self new.
    u fundfreq: fundfreq.
    u formfreq: formfreq.
    u bwfreq: bwfreq.
    ^u!
  fundfreq: fundfreq formfreq: formfreq bwfreq: bwfreq mul: mul
    |u|
    u := self new.
    u fundfreq: fundfreq.
    u formfreq: formfreq.
    u bwfreq: bwfreq.
    u mul: mul.
    ^u!
  fundfreq: fundfreq formfreq: formfreq bwfreq: bwfreq mul: mul add: add
    |u|
    u := self new.
    u fundfreq: fundfreq.
    u formfreq: formfreq.
    u bwfreq: bwfreq.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#fundfreq:formfreq:bwfreq:!
!

UGen subclass: #FSinOsc
  instanceVariableNames: 'freq iphase'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

FSinOsc comment: 'Fast sine oscillator.'!

!FSinOsc methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
  iphase ^iphase!
  iphase: x iphase := x!
!

!FSinOsc methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #FSinOsc.
    paramNames := #(#freq #iphase).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 440.0. iphase := 0.0!
!


!FSinOsc class methodsFor: 'instance creation'!
  freq: freq iphase: iphase
    |u|
    u := self new.
    u freq: freq.
    u iphase: iphase.
    ^u!
  freq: freq iphase: iphase mul: mul
    |u|
    u := self new.
    u freq: freq.
    u iphase: iphase.
    u mul: mul.
    ^u!
  freq: freq iphase: iphase mul: mul add: add
    |u|
    u := self new.
    u freq: freq.
    u iphase: iphase.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#freq:iphase:!
!

UGen subclass: #FreeVerb
  instanceVariableNames: 'in mix room damp'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

FreeVerb comment: 'A reverb'!

!FreeVerb methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  mix ^mix!
  mix: x mix := x!
  room ^room!
  room: x room := x!
  damp ^damp!
  damp: x damp := x!
!

!FreeVerb methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #FreeVerb.
    paramNames := #(#in #mix #room #damp).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. mix := 0.33. room := 0.5. damp := 0.5!
!


!FreeVerb class methodsFor: 'instance creation'!
  in: in mix: mix room: room damp: damp
    |u|
    u := self new.
    u in: in.
    u mix: mix.
    u room: room.
    u damp: damp.
    ^u!
  in: in mix: mix room: room damp: damp mul: mul
    |u|
    u := self new.
    u in: in.
    u mix: mix.
    u room: room.
    u damp: damp.
    u mul: mul.
    ^u!
  in: in mix: mix room: room damp: damp mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u mix: mix.
    u room: room.
    u damp: damp.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:mix:room:damp:!
!

UGen subclass: #Gendy1
  instanceVariableNames: 'ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Gendy1 comment: 'Dynamic stochastic synthesis generator.'!

!Gendy1 methodsFor: 'accessing'!
  ampdist ^ampdist!
  ampdist: x ampdist := x!
  durdist ^durdist!
  durdist: x durdist := x!
  adparam ^adparam!
  adparam: x adparam := x!
  ddparam ^ddparam!
  ddparam: x ddparam := x!
  minfreq ^minfreq!
  minfreq: x minfreq := x!
  maxfreq ^maxfreq!
  maxfreq: x maxfreq := x!
  ampscale ^ampscale!
  ampscale: x ampscale := x!
  durscale ^durscale!
  durscale: x durscale := x!
  initCPs ^initCPs!
  initCPs: x initCPs := x!
  knum ^knum!
  knum: x knum := x!
!

!Gendy1 methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Gendy1.
    paramNames := #(#ampdist #durdist #adparam #ddparam #minfreq #maxfreq #ampscale #durscale #initCPs #knum).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    ampdist := 1.0. durdist := 1.0. adparam := 1.0. ddparam := 1.0. minfreq := 440.0. maxfreq := 660.0. ampscale := 0.5. durscale := 0.5. initCPs := 12.0. knum := 0.0!
!


!Gendy1 class methodsFor: 'instance creation'!
  ampdist: ampdist durdist: durdist adparam: adparam ddparam: ddparam minfreq: minfreq maxfreq: maxfreq ampscale: ampscale durscale: durscale initCPs: initCPs knum: knum
    |u|
    u := self new.
    u ampdist: ampdist.
    u durdist: durdist.
    u adparam: adparam.
    u ddparam: ddparam.
    u minfreq: minfreq.
    u maxfreq: maxfreq.
    u ampscale: ampscale.
    u durscale: durscale.
    u initCPs: initCPs.
    u knum: knum.
    ^u!
  ampdist: ampdist durdist: durdist adparam: adparam ddparam: ddparam minfreq: minfreq maxfreq: maxfreq ampscale: ampscale durscale: durscale initCPs: initCPs knum: knum mul: mul
    |u|
    u := self new.
    u ampdist: ampdist.
    u durdist: durdist.
    u adparam: adparam.
    u ddparam: ddparam.
    u minfreq: minfreq.
    u maxfreq: maxfreq.
    u ampscale: ampscale.
    u durscale: durscale.
    u initCPs: initCPs.
    u knum: knum.
    u mul: mul.
    ^u!
  ampdist: ampdist durdist: durdist adparam: adparam ddparam: ddparam minfreq: minfreq maxfreq: maxfreq ampscale: ampscale durscale: durscale initCPs: initCPs knum: knum mul: mul add: add
    |u|
    u := self new.
    u ampdist: ampdist.
    u durdist: durdist.
    u adparam: adparam.
    u ddparam: ddparam.
    u minfreq: minfreq.
    u maxfreq: maxfreq.
    u ampscale: ampscale.
    u durscale: durscale.
    u initCPs: initCPs.
    u knum: knum.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#ampdist:durdist:adparam:ddparam:minfreq:maxfreq:ampscale:durscale:initCPs:knum:!
!

UGen subclass: #GrainFM
  instanceVariableNames: 'trigger dur carfreq modfreq index pan envbufnum maxGrains'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

GrainFM comment: 'Granular synthesis with frequency modulated sine tones'!

!GrainFM methodsFor: 'accessing'!
  trigger ^trigger!
  trigger: x trigger := x!
  dur ^dur!
  dur: x dur := x!
  carfreq ^carfreq!
  carfreq: x carfreq := x!
  modfreq ^modfreq!
  modfreq: x modfreq := x!
  index ^index!
  index: x index := x!
  pan ^pan!
  pan: x pan := x!
  envbufnum ^envbufnum!
  envbufnum: x envbufnum := x!
  maxGrains ^maxGrains!
  maxGrains: x maxGrains := x!
!

!GrainFM methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #GrainFM.
    paramNames := #(#trigger #dur #carfreq #modfreq #index #pan #envbufnum #maxGrains).
    mceNames := #().
    ugenRate := #ar.
    numChan := nil.
    specialIndex := 0.
    ugenId := nil.
    trigger := 0.0. dur := 1.0. carfreq := 440.0. modfreq := 200.0. index := 1.0. pan := 0.0. envbufnum := -1.0. maxGrains := 512.0!
!


!GrainFM class methodsFor: 'instance creation'!
  numChan: numChan trigger: trigger dur: dur carfreq: carfreq modfreq: modfreq index: index pan: pan envbufnum: envbufnum maxGrains: maxGrains
    |u|
    u := self new.
    u numChan: numChan.
    u trigger: trigger.
    u dur: dur.
    u carfreq: carfreq.
    u modfreq: modfreq.
    u index: index.
    u pan: pan.
    u envbufnum: envbufnum.
    u maxGrains: maxGrains.
    ^u!
  numChan: numChan trigger: trigger dur: dur carfreq: carfreq modfreq: modfreq index: index pan: pan envbufnum: envbufnum maxGrains: maxGrains mul: mul
    |u|
    u := self new.
    u numChan: numChan.
    u trigger: trigger.
    u dur: dur.
    u carfreq: carfreq.
    u modfreq: modfreq.
    u index: index.
    u pan: pan.
    u envbufnum: envbufnum.
    u maxGrains: maxGrains.
    u mul: mul.
    ^u!
  numChan: numChan trigger: trigger dur: dur carfreq: carfreq modfreq: modfreq index: index pan: pan envbufnum: envbufnum maxGrains: maxGrains mul: mul add: add
    |u|
    u := self new.
    u numChan: numChan.
    u trigger: trigger.
    u dur: dur.
    u carfreq: carfreq.
    u modfreq: modfreq.
    u index: index.
    u pan: pan.
    u envbufnum: envbufnum.
    u maxGrains: maxGrains.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#numChan:trigger:dur:carfreq:modfreq:index:pan:envbufnum:maxGrains:!
!

UGen subclass: #GrainSin
  instanceVariableNames: 'trigger dur freq pan envbufnum maxGrains'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

GrainSin comment: 'Granular synthesis with sine tones'!

!GrainSin methodsFor: 'accessing'!
  trigger ^trigger!
  trigger: x trigger := x!
  dur ^dur!
  dur: x dur := x!
  freq ^freq!
  freq: x freq := x!
  pan ^pan!
  pan: x pan := x!
  envbufnum ^envbufnum!
  envbufnum: x envbufnum := x!
  maxGrains ^maxGrains!
  maxGrains: x maxGrains := x!
!

!GrainSin methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #GrainSin.
    paramNames := #(#trigger #dur #freq #pan #envbufnum #maxGrains).
    mceNames := #().
    ugenRate := #ar.
    numChan := nil.
    specialIndex := 0.
    ugenId := nil.
    trigger := 0.0. dur := 1.0. freq := 440.0. pan := 0.0. envbufnum := -1.0. maxGrains := 512.0!
!


!GrainSin class methodsFor: 'instance creation'!
  numChan: numChan trigger: trigger dur: dur freq: freq pan: pan envbufnum: envbufnum maxGrains: maxGrains
    |u|
    u := self new.
    u numChan: numChan.
    u trigger: trigger.
    u dur: dur.
    u freq: freq.
    u pan: pan.
    u envbufnum: envbufnum.
    u maxGrains: maxGrains.
    ^u!
  numChan: numChan trigger: trigger dur: dur freq: freq pan: pan envbufnum: envbufnum maxGrains: maxGrains mul: mul
    |u|
    u := self new.
    u numChan: numChan.
    u trigger: trigger.
    u dur: dur.
    u freq: freq.
    u pan: pan.
    u envbufnum: envbufnum.
    u maxGrains: maxGrains.
    u mul: mul.
    ^u!
  numChan: numChan trigger: trigger dur: dur freq: freq pan: pan envbufnum: envbufnum maxGrains: maxGrains mul: mul add: add
    |u|
    u := self new.
    u numChan: numChan.
    u trigger: trigger.
    u dur: dur.
    u freq: freq.
    u pan: pan.
    u envbufnum: envbufnum.
    u maxGrains: maxGrains.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#numChan:trigger:dur:freq:pan:envbufnum:maxGrains:!
!

UGen subclass: #GreyholeRaw
  instanceVariableNames: 'in1 in2 damping delaytime diffusion feedback moddepth modfreq size'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

GreyholeRaw comment: 'algorithmic delay'!

!GreyholeRaw methodsFor: 'accessing'!
  in1 ^in1!
  in1: x in1 := x!
  in2 ^in2!
  in2: x in2 := x!
  damping ^damping!
  damping: x damping := x!
  delaytime ^delaytime!
  delaytime: x delaytime := x!
  diffusion ^diffusion!
  diffusion: x diffusion := x!
  feedback ^feedback!
  feedback: x feedback := x!
  moddepth ^moddepth!
  moddepth: x moddepth := x!
  modfreq ^modfreq!
  modfreq: x modfreq := x!
  size ^size!
  size: x size := x!
!

!GreyholeRaw methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #GreyholeRaw.
    paramNames := #(#in1 #in2 #damping #delaytime #diffusion #feedback #moddepth #modfreq #size).
    mceNames := #().
    ugenRate := #ar.
    numChan := 2.
    specialIndex := 0.
    ugenId := nil.
    in1 := 0.0. in2 := 0.0. damping := 0.0. delaytime := 2.0. diffusion := 0.5. feedback := 0.9. moddepth := 0.1. modfreq := 2.0. size := 1.0!
!


!GreyholeRaw class methodsFor: 'instance creation'!
  in1: in1 in2: in2 damping: damping delaytime: delaytime diffusion: diffusion feedback: feedback moddepth: moddepth modfreq: modfreq size: size
    |u|
    u := self new.
    u in1: in1.
    u in2: in2.
    u damping: damping.
    u delaytime: delaytime.
    u diffusion: diffusion.
    u feedback: feedback.
    u moddepth: moddepth.
    u modfreq: modfreq.
    u size: size.
    ^u!
  in1: in1 in2: in2 damping: damping delaytime: delaytime diffusion: diffusion feedback: feedback moddepth: moddepth modfreq: modfreq size: size mul: mul
    |u|
    u := self new.
    u in1: in1.
    u in2: in2.
    u damping: damping.
    u delaytime: delaytime.
    u diffusion: diffusion.
    u feedback: feedback.
    u moddepth: moddepth.
    u modfreq: modfreq.
    u size: size.
    u mul: mul.
    ^u!
  in1: in1 in2: in2 damping: damping delaytime: delaytime diffusion: diffusion feedback: feedback moddepth: moddepth modfreq: modfreq size: size mul: mul add: add
    |u|
    u := self new.
    u in1: in1.
    u in2: in2.
    u damping: damping.
    u delaytime: delaytime.
    u diffusion: diffusion.
    u feedback: feedback.
    u moddepth: moddepth.
    u modfreq: modfreq.
    u size: size.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in1:in2:damping:delaytime:diffusion:feedback:moddepth:modfreq:size:!
!

UGen subclass: #GVerb
  instanceVariableNames: 'in roomsize revtime damping inputbw spread drylevel earlyreflevel taillevel maxroomsize'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

GVerb comment: 'A two-channel reverb'!

!GVerb methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  roomsize ^roomsize!
  roomsize: x roomsize := x!
  revtime ^revtime!
  revtime: x revtime := x!
  damping ^damping!
  damping: x damping := x!
  inputbw ^inputbw!
  inputbw: x inputbw := x!
  spread ^spread!
  spread: x spread := x!
  drylevel ^drylevel!
  drylevel: x drylevel := x!
  earlyreflevel ^earlyreflevel!
  earlyreflevel: x earlyreflevel := x!
  taillevel ^taillevel!
  taillevel: x taillevel := x!
  maxroomsize ^maxroomsize!
  maxroomsize: x maxroomsize := x!
!

!GVerb methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #GVerb.
    paramNames := #(#in #roomsize #revtime #damping #inputbw #spread #drylevel #earlyreflevel #taillevel #maxroomsize).
    mceNames := #().
    ugenRate := #ar.
    numChan := 2.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. roomsize := 10.0. revtime := 3.0. damping := 0.5. inputbw := 0.5. spread := 15.0. drylevel := 1.0. earlyreflevel := 0.7. taillevel := 0.5. maxroomsize := 300.0!
!


!GVerb class methodsFor: 'instance creation'!
  in: in roomsize: roomsize revtime: revtime damping: damping inputbw: inputbw spread: spread drylevel: drylevel earlyreflevel: earlyreflevel taillevel: taillevel maxroomsize: maxroomsize
    |u|
    u := self new.
    u in: in.
    u roomsize: roomsize.
    u revtime: revtime.
    u damping: damping.
    u inputbw: inputbw.
    u spread: spread.
    u drylevel: drylevel.
    u earlyreflevel: earlyreflevel.
    u taillevel: taillevel.
    u maxroomsize: maxroomsize.
    ^u!
  in: in roomsize: roomsize revtime: revtime damping: damping inputbw: inputbw spread: spread drylevel: drylevel earlyreflevel: earlyreflevel taillevel: taillevel maxroomsize: maxroomsize mul: mul
    |u|
    u := self new.
    u in: in.
    u roomsize: roomsize.
    u revtime: revtime.
    u damping: damping.
    u inputbw: inputbw.
    u spread: spread.
    u drylevel: drylevel.
    u earlyreflevel: earlyreflevel.
    u taillevel: taillevel.
    u maxroomsize: maxroomsize.
    u mul: mul.
    ^u!
  in: in roomsize: roomsize revtime: revtime damping: damping inputbw: inputbw spread: spread drylevel: drylevel earlyreflevel: earlyreflevel taillevel: taillevel maxroomsize: maxroomsize mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u roomsize: roomsize.
    u revtime: revtime.
    u damping: damping.
    u inputbw: inputbw.
    u spread: spread.
    u drylevel: drylevel.
    u earlyreflevel: earlyreflevel.
    u taillevel: taillevel.
    u maxroomsize: maxroomsize.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:roomsize:revtime:damping:inputbw:spread:drylevel:earlyreflevel:taillevel:maxroomsize:!
!

UGen subclass: #Hasher
  instanceVariableNames: 'in'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Hasher comment: 'Scrambled value with a hash function.'!

!Hasher methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
!

!Hasher methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Hasher.
    paramNames := #(#in).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0!
!


!Hasher class methodsFor: 'instance creation'!
  in: in
    |u|
    u := self new.
    u in: in.
    ^u!
  in: in mul: mul
    |u|
    u := self new.
    u in: in.
    u mul: mul.
    ^u!
  in: in mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:!
!

UGen subclass: #HPF
  instanceVariableNames: 'in freq'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

HPF comment: '2nd order Butterworth highpass filter.'!

!HPF methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  freq ^freq!
  freq: x freq := x!
!

!HPF methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #HPF.
    paramNames := #(#in #freq).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. freq := 440.0!
!


!HPF class methodsFor: 'instance creation'!
  in: in freq: freq
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    ^u!
  in: in freq: freq mul: mul
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u mul: mul.
    ^u!
  in: in freq: freq mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:freq:!
!

UGen subclass: #HPZ1
  instanceVariableNames: 'in'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

HPZ1 comment: 'Two point difference filter'!

!HPZ1 methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
!

!HPZ1 methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #HPZ1.
    paramNames := #(#in).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0!
!


!HPZ1 class methodsFor: 'instance creation'!
  in: in
    |u|
    u := self new.
    u in: in.
    ^u!
  in: in mul: mul
    |u|
    u := self new.
    u in: in.
    u mul: mul.
    ^u!
  in: in mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:!
!

UGen subclass: #IRand
  instanceVariableNames: 'lo hi'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

IRand comment: 'Single integer random number generator.'!

!IRand methodsFor: 'accessing'!
  lo ^lo!
  lo: x lo := x!
  hi ^hi!
  hi: x hi := x!
!

!IRand methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #IRand.
    paramNames := #(#lo #hi).
    mceNames := #().
    ugenRate := #ir.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    lo := 0.0. hi := 127.0!
!


!IRand class methodsFor: 'instance creation'!
  lo: lo hi: hi
    |u|
    u := self new.
    u lo: lo.
    u hi: hi.
    ^u!
  lo: lo hi: hi mul: mul
    |u|
    u := self new.
    u lo: lo.
    u hi: hi.
    u mul: mul.
    ^u!
  lo: lo hi: hi mul: mul add: add
    |u|
    u := self new.
    u lo: lo.
    u hi: hi.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#lo:hi:!
!

UGen subclass: #Impulse
  instanceVariableNames: 'freq phase'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Impulse comment: 'Impulse oscillator.'!

!Impulse methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
  phase ^phase!
  phase: x phase := x!
!

!Impulse methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Impulse.
    paramNames := #(#freq #phase).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 440.0. phase := 0.0!
!


!Impulse class methodsFor: 'instance creation'!
  freq: freq phase: phase
    |u|
    u := self new.
    u freq: freq.
    u phase: phase.
    ^u!
  freq: freq phase: phase mul: mul
    |u|
    u := self new.
    u freq: freq.
    u phase: phase.
    u mul: mul.
    ^u!
  freq: freq phase: phase mul: mul add: add
    |u|
    u := self new.
    u freq: freq.
    u phase: phase.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#freq:phase:!
!

UGen subclass: #In
  instanceVariableNames: 'bus'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

In comment: 'Read a signal from a bus.'!

!In methodsFor: 'accessing'!
  bus ^bus!
  bus: x bus := x!
!

!In methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #In.
    paramNames := #(#bus).
    mceNames := #().
    ugenRate := #ar.
    numChan := nil.
    specialIndex := 0.
    ugenId := nil.
    bus := 0.0!
!


!In class methodsFor: 'instance creation'!
  numChan: numChan bus: bus
    |u|
    u := self new.
    u numChan: numChan.
    u bus: bus.
    ^u!
  numChan: numChan bus: bus mul: mul
    |u|
    u := self new.
    u numChan: numChan.
    u bus: bus.
    u mul: mul.
    ^u!
  numChan: numChan bus: bus mul: mul add: add
    |u|
    u := self new.
    u numChan: numChan.
    u bus: bus.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#numChan:bus:!
!

UGen subclass: #InFeedback
  instanceVariableNames: 'bus'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

InFeedback comment: 'Read signal from a bus with a current or one cycle old timestamp.'!

!InFeedback methodsFor: 'accessing'!
  bus ^bus!
  bus: x bus := x!
!

!InFeedback methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #InFeedback.
    paramNames := #(#bus).
    mceNames := #().
    ugenRate := #ar.
    numChan := nil.
    specialIndex := 0.
    ugenId := nil.
    bus := 0.0!
!


!InFeedback class methodsFor: 'instance creation'!
  numChan: numChan bus: bus
    |u|
    u := self new.
    u numChan: numChan.
    u bus: bus.
    ^u!
  numChan: numChan bus: bus mul: mul
    |u|
    u := self new.
    u numChan: numChan.
    u bus: bus.
    u mul: mul.
    ^u!
  numChan: numChan bus: bus mul: mul add: add
    |u|
    u := self new.
    u numChan: numChan.
    u bus: bus.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#numChan:bus:!
!

UGen subclass: #InRange
  instanceVariableNames: 'in lo hi'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

InRange comment: 'Tests if a signal is within a given range.'!

!InRange methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  lo ^lo!
  lo: x lo := x!
  hi ^hi!
  hi: x hi := x!
!

!InRange methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #InRange.
    paramNames := #(#in #lo #hi).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. lo := 0.0. hi := 1.0!
!


!InRange class methodsFor: 'instance creation'!
  in: in lo: lo hi: hi
    |u|
    u := self new.
    u in: in.
    u lo: lo.
    u hi: hi.
    ^u!
  in: in lo: lo hi: hi mul: mul
    |u|
    u := self new.
    u in: in.
    u lo: lo.
    u hi: hi.
    u mul: mul.
    ^u!
  in: in lo: lo hi: hi mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u lo: lo.
    u hi: hi.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:lo:hi:!
!

UGen subclass: #K2A
  instanceVariableNames: 'in'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

K2A comment: 'Control to audio rate converter.'!

!K2A methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
!

!K2A methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #K2A.
    paramNames := #(#in).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0!
!


!K2A class methodsFor: 'instance creation'!
  in: in
    |u|
    u := self new.
    u in: in.
    ^u!
  in: in mul: mul
    |u|
    u := self new.
    u in: in.
    u mul: mul.
    ^u!
  in: in mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:!
!

UGen subclass: #Klang
  instanceVariableNames: 'freqscale freqoffset specificationsArrayRef'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Klang comment: 'Sine oscillator bank'!

!Klang methodsFor: 'accessing'!
  freqscale ^freqscale!
  freqscale: x freqscale := x!
  freqoffset ^freqoffset!
  freqoffset: x freqoffset := x!
  specificationsArrayRef ^specificationsArrayRef!
  specificationsArrayRef: x specificationsArrayRef := x!
!

!Klang methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Klang.
    paramNames := #(#freqscale #freqoffset).
    mceNames := #(#specificationsArrayRef).
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freqscale := 1.0. freqoffset := 0.0!
!


!Klang class methodsFor: 'instance creation'!
  freqscale: freqscale freqoffset: freqoffset specificationsArrayRef: specificationsArrayRef
    |u|
    u := self new.
    u freqscale: freqscale.
    u freqoffset: freqoffset.
    u specificationsArrayRef: specificationsArrayRef.
    ^u!
  freqscale: freqscale freqoffset: freqoffset specificationsArrayRef: specificationsArrayRef mul: mul
    |u|
    u := self new.
    u freqscale: freqscale.
    u freqoffset: freqoffset.
    u specificationsArrayRef: specificationsArrayRef.
    u mul: mul.
    ^u!
  freqscale: freqscale freqoffset: freqoffset specificationsArrayRef: specificationsArrayRef mul: mul add: add
    |u|
    u := self new.
    u freqscale: freqscale.
    u freqoffset: freqoffset.
    u specificationsArrayRef: specificationsArrayRef.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#freqscale:freqoffset:specificationsArrayRef:!
!

UGen subclass: #Klank
  instanceVariableNames: 'input freqscale freqoffset decayscale specificationsArrayRef'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Klank comment: 'Bank of resonators'!

!Klank methodsFor: 'accessing'!
  input ^input!
  input: x input := x!
  freqscale ^freqscale!
  freqscale: x freqscale := x!
  freqoffset ^freqoffset!
  freqoffset: x freqoffset := x!
  decayscale ^decayscale!
  decayscale: x decayscale := x!
  specificationsArrayRef ^specificationsArrayRef!
  specificationsArrayRef: x specificationsArrayRef := x!
!

!Klank methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Klank.
    paramNames := #(#input #freqscale #freqoffset #decayscale).
    mceNames := #(#specificationsArrayRef).
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    input := 0.0. freqscale := 1.0. freqoffset := 0.0. decayscale := 1.0!
!


!Klank class methodsFor: 'instance creation'!
  input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef
    |u|
    u := self new.
    u input: input.
    u freqscale: freqscale.
    u freqoffset: freqoffset.
    u decayscale: decayscale.
    u specificationsArrayRef: specificationsArrayRef.
    ^u!
  input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef mul: mul
    |u|
    u := self new.
    u input: input.
    u freqscale: freqscale.
    u freqoffset: freqoffset.
    u decayscale: decayscale.
    u specificationsArrayRef: specificationsArrayRef.
    u mul: mul.
    ^u!
  input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef mul: mul add: add
    |u|
    u := self new.
    u input: input.
    u freqscale: freqscale.
    u freqoffset: freqoffset.
    u decayscale: decayscale.
    u specificationsArrayRef: specificationsArrayRef.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#input:freqscale:freqoffset:decayscale:specificationsArrayRef:!
!

UGen subclass: #LFCub
  instanceVariableNames: 'freq iphase'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

LFCub comment: 'A sine like shape made of two cubic pieces'!

!LFCub methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
  iphase ^iphase!
  iphase: x iphase := x!
!

!LFCub methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #LFCub.
    paramNames := #(#freq #iphase).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 440.0. iphase := 0.0!
!


!LFCub class methodsFor: 'instance creation'!
  freq: freq iphase: iphase
    |u|
    u := self new.
    u freq: freq.
    u iphase: iphase.
    ^u!
  freq: freq iphase: iphase mul: mul
    |u|
    u := self new.
    u freq: freq.
    u iphase: iphase.
    u mul: mul.
    ^u!
  freq: freq iphase: iphase mul: mul add: add
    |u|
    u := self new.
    u freq: freq.
    u iphase: iphase.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#freq:iphase:!
!

UGen subclass: #LFDNoise3
  instanceVariableNames: 'freq'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

LFDNoise3 comment: 'Dynamic cubic noise'!

!LFDNoise3 methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
!

!LFDNoise3 methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #LFDNoise3.
    paramNames := #(#freq).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 500.0!
!


!LFDNoise3 class methodsFor: 'instance creation'!
  freq: freq
    |u|
    u := self new.
    u freq: freq.
    ^u!
  freq: freq mul: mul
    |u|
    u := self new.
    u freq: freq.
    u mul: mul.
    ^u!
  freq: freq mul: mul add: add
    |u|
    u := self new.
    u freq: freq.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#freq:!
!

UGen subclass: #LFNoise0
  instanceVariableNames: 'freq'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

LFNoise0 comment: 'Step noise'!

!LFNoise0 methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
!

!LFNoise0 methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #LFNoise0.
    paramNames := #(#freq).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 500.0!
!


!LFNoise0 class methodsFor: 'instance creation'!
  freq: freq
    |u|
    u := self new.
    u freq: freq.
    ^u!
  freq: freq mul: mul
    |u|
    u := self new.
    u freq: freq.
    u mul: mul.
    ^u!
  freq: freq mul: mul add: add
    |u|
    u := self new.
    u freq: freq.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#freq:!
!

UGen subclass: #LFNoise1
  instanceVariableNames: 'freq'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

LFNoise1 comment: 'Ramp noise'!

!LFNoise1 methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
!

!LFNoise1 methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #LFNoise1.
    paramNames := #(#freq).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 500.0!
!


!LFNoise1 class methodsFor: 'instance creation'!
  freq: freq
    |u|
    u := self new.
    u freq: freq.
    ^u!
  freq: freq mul: mul
    |u|
    u := self new.
    u freq: freq.
    u mul: mul.
    ^u!
  freq: freq mul: mul add: add
    |u|
    u := self new.
    u freq: freq.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#freq:!
!

UGen subclass: #LFNoise2
  instanceVariableNames: 'freq'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

LFNoise2 comment: 'Quadratic noise.'!

!LFNoise2 methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
!

!LFNoise2 methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #LFNoise2.
    paramNames := #(#freq).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 500.0!
!


!LFNoise2 class methodsFor: 'instance creation'!
  freq: freq
    |u|
    u := self new.
    u freq: freq.
    ^u!
  freq: freq mul: mul
    |u|
    u := self new.
    u freq: freq.
    u mul: mul.
    ^u!
  freq: freq mul: mul add: add
    |u|
    u := self new.
    u freq: freq.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#freq:!
!

UGen subclass: #LFPar
  instanceVariableNames: 'freq iphase'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

LFPar comment: 'Parabolic oscillator'!

!LFPar methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
  iphase ^iphase!
  iphase: x iphase := x!
!

!LFPar methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #LFPar.
    paramNames := #(#freq #iphase).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 440.0. iphase := 0.0!
!


!LFPar class methodsFor: 'instance creation'!
  freq: freq iphase: iphase
    |u|
    u := self new.
    u freq: freq.
    u iphase: iphase.
    ^u!
  freq: freq iphase: iphase mul: mul
    |u|
    u := self new.
    u freq: freq.
    u iphase: iphase.
    u mul: mul.
    ^u!
  freq: freq iphase: iphase mul: mul add: add
    |u|
    u := self new.
    u freq: freq.
    u iphase: iphase.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#freq:iphase:!
!

UGen subclass: #LFPulse
  instanceVariableNames: 'freq iphase width'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

LFPulse comment: 'pulse oscillator'!

!LFPulse methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
  iphase ^iphase!
  iphase: x iphase := x!
  width ^width!
  width: x width := x!
!

!LFPulse methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #LFPulse.
    paramNames := #(#freq #iphase #width).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 440.0. iphase := 0.0. width := 0.5!
!


!LFPulse class methodsFor: 'instance creation'!
  freq: freq iphase: iphase width: width
    |u|
    u := self new.
    u freq: freq.
    u iphase: iphase.
    u width: width.
    ^u!
  freq: freq iphase: iphase width: width mul: mul
    |u|
    u := self new.
    u freq: freq.
    u iphase: iphase.
    u width: width.
    u mul: mul.
    ^u!
  freq: freq iphase: iphase width: width mul: mul add: add
    |u|
    u := self new.
    u freq: freq.
    u iphase: iphase.
    u width: width.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#freq:iphase:width:!
!

UGen subclass: #LFSaw
  instanceVariableNames: 'freq iphase'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

LFSaw comment: 'Sawtooth oscillator'!

!LFSaw methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
  iphase ^iphase!
  iphase: x iphase := x!
!

!LFSaw methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #LFSaw.
    paramNames := #(#freq #iphase).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 440.0. iphase := 0.0!
!


!LFSaw class methodsFor: 'instance creation'!
  freq: freq iphase: iphase
    |u|
    u := self new.
    u freq: freq.
    u iphase: iphase.
    ^u!
  freq: freq iphase: iphase mul: mul
    |u|
    u := self new.
    u freq: freq.
    u iphase: iphase.
    u mul: mul.
    ^u!
  freq: freq iphase: iphase mul: mul add: add
    |u|
    u := self new.
    u freq: freq.
    u iphase: iphase.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#freq:iphase:!
!

UGen subclass: #LFTri
  instanceVariableNames: 'freq iphase'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

LFTri comment: 'Triangle oscillator'!

!LFTri methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
  iphase ^iphase!
  iphase: x iphase := x!
!

!LFTri methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #LFTri.
    paramNames := #(#freq #iphase).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 440.0. iphase := 0.0!
!


!LFTri class methodsFor: 'instance creation'!
  freq: freq iphase: iphase
    |u|
    u := self new.
    u freq: freq.
    u iphase: iphase.
    ^u!
  freq: freq iphase: iphase mul: mul
    |u|
    u := self new.
    u freq: freq.
    u iphase: iphase.
    u mul: mul.
    ^u!
  freq: freq iphase: iphase mul: mul add: add
    |u|
    u := self new.
    u freq: freq.
    u iphase: iphase.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#freq:iphase:!
!

UGen subclass: #LPF
  instanceVariableNames: 'in freq'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

LPF comment: '2nd order Butterworth lowpass filter'!

!LPF methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  freq ^freq!
  freq: x freq := x!
!

!LPF methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #LPF.
    paramNames := #(#in #freq).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. freq := 440.0!
!


!LPF class methodsFor: 'instance creation'!
  in: in freq: freq
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    ^u!
  in: in freq: freq mul: mul
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u mul: mul.
    ^u!
  in: in freq: freq mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:freq:!
!

UGen subclass: #Lag
  instanceVariableNames: 'in lagTime'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Lag comment: 'Exponential lag'!

!Lag methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  lagTime ^lagTime!
  lagTime: x lagTime := x!
!

!Lag methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Lag.
    paramNames := #(#in #lagTime).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. lagTime := 0.1!
!


!Lag class methodsFor: 'instance creation'!
  in: in lagTime: lagTime
    |u|
    u := self new.
    u in: in.
    u lagTime: lagTime.
    ^u!
  in: in lagTime: lagTime mul: mul
    |u|
    u := self new.
    u in: in.
    u lagTime: lagTime.
    u mul: mul.
    ^u!
  in: in lagTime: lagTime mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u lagTime: lagTime.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:lagTime:!
!

UGen subclass: #LagUD
  instanceVariableNames: 'in lagTimeU lagTimeD'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

LagUD comment: 'Exponential lag'!

!LagUD methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  lagTimeU ^lagTimeU!
  lagTimeU: x lagTimeU := x!
  lagTimeD ^lagTimeD!
  lagTimeD: x lagTimeD := x!
!

!LagUD methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #LagUD.
    paramNames := #(#in #lagTimeU #lagTimeD).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. lagTimeU := 0.1. lagTimeD := 0.1!
!


!LagUD class methodsFor: 'instance creation'!
  in: in lagTimeU: lagTimeU lagTimeD: lagTimeD
    |u|
    u := self new.
    u in: in.
    u lagTimeU: lagTimeU.
    u lagTimeD: lagTimeD.
    ^u!
  in: in lagTimeU: lagTimeU lagTimeD: lagTimeD mul: mul
    |u|
    u := self new.
    u in: in.
    u lagTimeU: lagTimeU.
    u lagTimeD: lagTimeD.
    u mul: mul.
    ^u!
  in: in lagTimeU: lagTimeU lagTimeD: lagTimeD mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u lagTimeU: lagTimeU.
    u lagTimeD: lagTimeD.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:lagTimeU:lagTimeD:!
!

UGen subclass: #Lag2
  instanceVariableNames: 'in lagTime'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Lag2 comment: 'Exponential lag'!

!Lag2 methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  lagTime ^lagTime!
  lagTime: x lagTime := x!
!

!Lag2 methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Lag2.
    paramNames := #(#in #lagTime).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. lagTime := 0.1!
!


!Lag2 class methodsFor: 'instance creation'!
  in: in lagTime: lagTime
    |u|
    u := self new.
    u in: in.
    u lagTime: lagTime.
    ^u!
  in: in lagTime: lagTime mul: mul
    |u|
    u := self new.
    u in: in.
    u lagTime: lagTime.
    u mul: mul.
    ^u!
  in: in lagTime: lagTime mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u lagTime: lagTime.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:lagTime:!
!

UGen subclass: #Lag3
  instanceVariableNames: 'in lagTime'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Lag3 comment: 'Exponential lag'!

!Lag3 methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  lagTime ^lagTime!
  lagTime: x lagTime := x!
!

!Lag3 methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Lag3.
    paramNames := #(#in #lagTime).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. lagTime := 0.1!
!


!Lag3 class methodsFor: 'instance creation'!
  in: in lagTime: lagTime
    |u|
    u := self new.
    u in: in.
    u lagTime: lagTime.
    ^u!
  in: in lagTime: lagTime mul: mul
    |u|
    u := self new.
    u in: in.
    u lagTime: lagTime.
    u mul: mul.
    ^u!
  in: in lagTime: lagTime mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u lagTime: lagTime.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:lagTime:!
!

UGen subclass: #Lag3UD
  instanceVariableNames: 'in lagTimeU lagTimeD'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Lag3UD comment: 'Exponential lag'!

!Lag3UD methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  lagTimeU ^lagTimeU!
  lagTimeU: x lagTimeU := x!
  lagTimeD ^lagTimeD!
  lagTimeD: x lagTimeD := x!
!

!Lag3UD methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Lag3UD.
    paramNames := #(#in #lagTimeU #lagTimeD).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. lagTimeU := 0.1. lagTimeD := 0.1!
!


!Lag3UD class methodsFor: 'instance creation'!
  in: in lagTimeU: lagTimeU lagTimeD: lagTimeD
    |u|
    u := self new.
    u in: in.
    u lagTimeU: lagTimeU.
    u lagTimeD: lagTimeD.
    ^u!
  in: in lagTimeU: lagTimeU lagTimeD: lagTimeD mul: mul
    |u|
    u := self new.
    u in: in.
    u lagTimeU: lagTimeU.
    u lagTimeD: lagTimeD.
    u mul: mul.
    ^u!
  in: in lagTimeU: lagTimeU lagTimeD: lagTimeD mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u lagTimeU: lagTimeU.
    u lagTimeD: lagTimeD.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:lagTimeU:lagTimeD:!
!

UGen subclass: #Latch
  instanceVariableNames: 'in trig'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Latch comment: 'Sample and hold'!

!Latch methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  trig ^trig!
  trig: x trig := x!
!

!Latch methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Latch.
    paramNames := #(#in #trig).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. trig := 0.0!
!


!Latch class methodsFor: 'instance creation'!
  in: in trig: trig
    |u|
    u := self new.
    u in: in.
    u trig: trig.
    ^u!
  in: in trig: trig mul: mul
    |u|
    u := self new.
    u in: in.
    u trig: trig.
    u mul: mul.
    ^u!
  in: in trig: trig mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u trig: trig.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:trig:!
!

UGen subclass: #LeakDC
  instanceVariableNames: 'in coef'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

LeakDC comment: 'Remove DC'!

!LeakDC methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  coef ^coef!
  coef: x coef := x!
!

!LeakDC methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #LeakDC.
    paramNames := #(#in #coef).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. coef := 0.995!
!


!LeakDC class methodsFor: 'instance creation'!
  in: in coef: coef
    |u|
    u := self new.
    u in: in.
    u coef: coef.
    ^u!
  in: in coef: coef mul: mul
    |u|
    u := self new.
    u in: in.
    u coef: coef.
    u mul: mul.
    ^u!
  in: in coef: coef mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u coef: coef.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:coef:!
!

UGen subclass: #Limiter
  instanceVariableNames: 'in level dur'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Limiter comment: 'Peak limiter'!

!Limiter methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  level ^level!
  level: x level := x!
  dur ^dur!
  dur: x dur := x!
!

!Limiter methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Limiter.
    paramNames := #(#in #level #dur).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. level := 1.0. dur := 0.01!
!


!Limiter class methodsFor: 'instance creation'!
  in: in level: level dur: dur
    |u|
    u := self new.
    u in: in.
    u level: level.
    u dur: dur.
    ^u!
  in: in level: level dur: dur mul: mul
    |u|
    u := self new.
    u in: in.
    u level: level.
    u dur: dur.
    u mul: mul.
    ^u!
  in: in level: level dur: dur mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u level: level.
    u dur: dur.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:level:dur:!
!

UGen subclass: #Line
  instanceVariableNames: 'start end dur doneAction'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Line comment: 'Line generator.'!

!Line methodsFor: 'accessing'!
  start ^start!
  start: x start := x!
  end ^end!
  end: x end := x!
  dur ^dur!
  dur: x dur := x!
  doneAction ^doneAction!
  doneAction: x doneAction := x!
!

!Line methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Line.
    paramNames := #(#start #end #dur #doneAction).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    start := 0.0. end := 1.0. dur := 1.0. doneAction := 0.0!
!


!Line class methodsFor: 'instance creation'!
  start: start end: end dur: dur doneAction: doneAction
    |u|
    u := self new.
    u start: start.
    u end: end.
    u dur: dur.
    u doneAction: doneAction.
    ^u!
  start: start end: end dur: dur doneAction: doneAction mul: mul
    |u|
    u := self new.
    u start: start.
    u end: end.
    u dur: dur.
    u doneAction: doneAction.
    u mul: mul.
    ^u!
  start: start end: end dur: dur doneAction: doneAction mul: mul add: add
    |u|
    u := self new.
    u start: start.
    u end: end.
    u dur: dur.
    u doneAction: doneAction.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#start:end:dur:doneAction:!
!

UGen subclass: #LinExp
  instanceVariableNames: 'in srclo srchi dstlo dsthi'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

LinExp comment: 'Map a linear range to an exponential range'!

!LinExp methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  srclo ^srclo!
  srclo: x srclo := x!
  srchi ^srchi!
  srchi: x srchi := x!
  dstlo ^dstlo!
  dstlo: x dstlo := x!
  dsthi ^dsthi!
  dsthi: x dsthi := x!
!

!LinExp methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #LinExp.
    paramNames := #(#in #srclo #srchi #dstlo #dsthi).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. srclo := 0.0. srchi := 1.0. dstlo := 1.0. dsthi := 2.0!
!


!LinExp class methodsFor: 'instance creation'!
  in: in srclo: srclo srchi: srchi dstlo: dstlo dsthi: dsthi
    |u|
    u := self new.
    u in: in.
    u srclo: srclo.
    u srchi: srchi.
    u dstlo: dstlo.
    u dsthi: dsthi.
    ^u!
  in: in srclo: srclo srchi: srchi dstlo: dstlo dsthi: dsthi mul: mul
    |u|
    u := self new.
    u in: in.
    u srclo: srclo.
    u srchi: srchi.
    u dstlo: dstlo.
    u dsthi: dsthi.
    u mul: mul.
    ^u!
  in: in srclo: srclo srchi: srchi dstlo: dstlo dsthi: dsthi mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u srclo: srclo.
    u srchi: srchi.
    u dstlo: dstlo.
    u dsthi: dsthi.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:srclo:srchi:dstlo:dsthi:!
!

UGen subclass: #LinRand
  instanceVariableNames: 'lo hi minmax'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

LinRand comment: 'Skewed random number generator.'!

!LinRand methodsFor: 'accessing'!
  lo ^lo!
  lo: x lo := x!
  hi ^hi!
  hi: x hi := x!
  minmax ^minmax!
  minmax: x minmax := x!
!

!LinRand methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #LinRand.
    paramNames := #(#lo #hi #minmax).
    mceNames := #().
    ugenRate := #ir.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    lo := 0.0. hi := 1.0. minmax := 0.0!
!


!LinRand class methodsFor: 'instance creation'!
  lo: lo hi: hi minmax: minmax
    |u|
    u := self new.
    u lo: lo.
    u hi: hi.
    u minmax: minmax.
    ^u!
  lo: lo hi: hi minmax: minmax mul: mul
    |u|
    u := self new.
    u lo: lo.
    u hi: hi.
    u minmax: minmax.
    u mul: mul.
    ^u!
  lo: lo hi: hi minmax: minmax mul: mul add: add
    |u|
    u := self new.
    u lo: lo.
    u hi: hi.
    u minmax: minmax.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#lo:hi:minmax:!
!

UGen subclass: #LocalBuf
  instanceVariableNames: 'numChannels numFrames'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

LocalBuf comment: 'Allocate a buffer local to the synth'!

!LocalBuf methodsFor: 'accessing'!
  numChannels ^numChannels!
  numChannels: x numChannels := x!
  numFrames ^numFrames!
  numFrames: x numFrames := x!
!

!LocalBuf methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #LocalBuf.
    paramNames := #(#numChannels #numFrames).
    mceNames := #().
    ugenRate := #ir.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    numChannels := 1.0. numFrames := 1.0!
!


!LocalBuf class methodsFor: 'instance creation'!
  numChannels: numChannels numFrames: numFrames
    |u|
    u := self new.
    u numChannels: numChannels.
    u numFrames: numFrames.
    ^u!
  numChannels: numChannels numFrames: numFrames mul: mul
    |u|
    u := self new.
    u numChannels: numChannels.
    u numFrames: numFrames.
    u mul: mul.
    ^u!
  numChannels: numChannels numFrames: numFrames mul: mul add: add
    |u|
    u := self new.
    u numChannels: numChannels.
    u numFrames: numFrames.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#numChannels:numFrames:!
!

UGen subclass: #LocalIn
  instanceVariableNames: 'default'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

LocalIn comment: 'Define and read from buses local to a synth.'!

!LocalIn methodsFor: 'accessing'!
  default ^default!
  default: x default := x!
!

!LocalIn methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #LocalIn.
    paramNames := #().
    mceNames := #(#default).
    ugenRate := #ar.
    numChan := nil.
    specialIndex := 0.
    ugenId := nil.
    !
!


!LocalIn class methodsFor: 'instance creation'!
  numChan: numChan default: default
    |u|
    u := self new.
    u numChan: numChan.
    u default: default.
    ^u!
  numChan: numChan default: default mul: mul
    |u|
    u := self new.
    u numChan: numChan.
    u default: default.
    u mul: mul.
    ^u!
  numChan: numChan default: default mul: mul add: add
    |u|
    u := self new.
    u numChan: numChan.
    u default: default.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#numChan:default:!
!

UGen subclass: #LocalOut
  instanceVariableNames: 'channelsArray'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

LocalOut comment: 'Write to buses local to a synth.'!

!LocalOut methodsFor: 'accessing'!
  channelsArray ^channelsArray!
  channelsArray: x channelsArray := x!
!

!LocalOut methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #LocalOut.
    paramNames := #().
    mceNames := #(#channelsArray).
    ugenRate := #ar.
    numChan := 0.
    specialIndex := 0.
    ugenId := nil.
    !
!


!LocalOut class methodsFor: 'instance creation'!
  channelsArray: channelsArray
    |u|
    u := self new.
    u channelsArray: channelsArray.
    ^u!
  channelsArray: channelsArray mul: mul
    |u|
    u := self new.
    u channelsArray: channelsArray.
    u mul: mul.
    ^u!
  channelsArray: channelsArray mul: mul add: add
    |u|
    u := self new.
    u channelsArray: channelsArray.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#channelsArray:!
!

UGen subclass: #MantissaMask
  instanceVariableNames: 'in bits'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

MantissaMask comment: 'Reduce precision.'!

!MantissaMask methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  bits ^bits!
  bits: x bits := x!
!

!MantissaMask methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #MantissaMask.
    paramNames := #(#in #bits).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. bits := 3.0!
!


!MantissaMask class methodsFor: 'instance creation'!
  in: in bits: bits
    |u|
    u := self new.
    u in: in.
    u bits: bits.
    ^u!
  in: in bits: bits mul: mul
    |u|
    u := self new.
    u in: in.
    u bits: bits.
    u mul: mul.
    ^u!
  in: in bits: bits mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u bits: bits.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:bits:!
!

UGen subclass: #MembraneCircle
  instanceVariableNames: 'excitation tension loss'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

MembraneCircle comment: 'Waveguide mesh physical models of drum membranes'!

!MembraneCircle methodsFor: 'accessing'!
  excitation ^excitation!
  excitation: x excitation := x!
  tension ^tension!
  tension: x tension := x!
  loss ^loss!
  loss: x loss := x!
!

!MembraneCircle methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #MembraneCircle.
    paramNames := #(#excitation #tension #loss).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    excitation := 0.0. tension := 0.05. loss := 0.99999!
!


!MembraneCircle class methodsFor: 'instance creation'!
  excitation: excitation tension: tension loss: loss
    |u|
    u := self new.
    u excitation: excitation.
    u tension: tension.
    u loss: loss.
    ^u!
  excitation: excitation tension: tension loss: loss mul: mul
    |u|
    u := self new.
    u excitation: excitation.
    u tension: tension.
    u loss: loss.
    u mul: mul.
    ^u!
  excitation: excitation tension: tension loss: loss mul: mul add: add
    |u|
    u := self new.
    u excitation: excitation.
    u tension: tension.
    u loss: loss.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#excitation:tension:loss:!
!

UGen subclass: #MiRings
  instanceVariableNames: 'in trig pit struct bright damp pos model poly intern_exciter easteregg bypass'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

MiRings comment: 'a resonator'!

!MiRings methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  trig ^trig!
  trig: x trig := x!
  pit ^pit!
  pit: x pit := x!
  struct ^struct!
  struct: x struct := x!
  bright ^bright!
  bright: x bright := x!
  damp ^damp!
  damp: x damp := x!
  pos ^pos!
  pos: x pos := x!
  model ^model!
  model: x model := x!
  poly ^poly!
  poly: x poly := x!
  intern_exciter ^intern_exciter!
  intern_exciter: x intern_exciter := x!
  easteregg ^easteregg!
  easteregg: x easteregg := x!
  bypass ^bypass!
  bypass: x bypass := x!
!

!MiRings methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #MiRings.
    paramNames := #(#in #trig #pit #struct #bright #damp #pos #model #poly #intern_exciter #easteregg #bypass).
    mceNames := #().
    ugenRate := #ar.
    numChan := 2.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. trig := 0.0. pit := 60.0. struct := 0.25. bright := 0.5. damp := 0.7. pos := 0.25. model := 0.0. poly := 1.0. intern_exciter := 0.0. easteregg := 0.0. bypass := 0.0!
!


!MiRings class methodsFor: 'instance creation'!
  in: in trig: trig pit: pit struct: struct bright: bright damp: damp pos: pos model: model poly: poly intern_exciter: intern_exciter easteregg: easteregg bypass: bypass
    |u|
    u := self new.
    u in: in.
    u trig: trig.
    u pit: pit.
    u struct: struct.
    u bright: bright.
    u damp: damp.
    u pos: pos.
    u model: model.
    u poly: poly.
    u intern_exciter: intern_exciter.
    u easteregg: easteregg.
    u bypass: bypass.
    ^u!
  in: in trig: trig pit: pit struct: struct bright: bright damp: damp pos: pos model: model poly: poly intern_exciter: intern_exciter easteregg: easteregg bypass: bypass mul: mul
    |u|
    u := self new.
    u in: in.
    u trig: trig.
    u pit: pit.
    u struct: struct.
    u bright: bright.
    u damp: damp.
    u pos: pos.
    u model: model.
    u poly: poly.
    u intern_exciter: intern_exciter.
    u easteregg: easteregg.
    u bypass: bypass.
    u mul: mul.
    ^u!
  in: in trig: trig pit: pit struct: struct bright: bright damp: damp pos: pos model: model poly: poly intern_exciter: intern_exciter easteregg: easteregg bypass: bypass mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u trig: trig.
    u pit: pit.
    u struct: struct.
    u bright: bright.
    u damp: damp.
    u pos: pos.
    u model: model.
    u poly: poly.
    u intern_exciter: intern_exciter.
    u easteregg: easteregg.
    u bypass: bypass.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:trig:pit:struct:bright:damp:pos:model:poly:intern_exciter:easteregg:bypass:!
!

UGen subclass: #ModDif
  instanceVariableNames: 'x y mod'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

ModDif comment: 'Minimum difference of two values in modulo arithmetics'!

!ModDif methodsFor: 'accessing'!
  x ^x!
  x: xValue x := xValue!
  y ^y!
  y: x y := x!
  mod ^mod!
  mod: x mod := x!
!

!ModDif methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #ModDif.
    paramNames := #(#x #y #mod).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    x := 0.0. y := 0.0. mod := 1.0!
!


!ModDif class methodsFor: 'instance creation'!
  x: x y: y mod: mod
    |u|
    u := self new.
    u x: x.
    u y: y.
    u mod: mod.
    ^u!
  x: x y: y mod: mod mul: mul
    |u|
    u := self new.
    u x: x.
    u y: y.
    u mod: mod.
    u mul: mul.
    ^u!
  x: x y: y mod: mod mul: mul add: add
    |u|
    u := self new.
    u x: x.
    u y: y.
    u mod: mod.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#x:y:mod:!
!

UGen subclass: #MoogFF
  instanceVariableNames: 'in freq gain reset'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

MoogFF comment: 'Moog VCF implementation, designed by Federico Fontana'!

!MoogFF methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  freq ^freq!
  freq: x freq := x!
  gain ^gain!
  gain: x gain := x!
  reset ^reset!
  reset: x reset := x!
!

!MoogFF methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #MoogFF.
    paramNames := #(#in #freq #gain #reset).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. freq := 100.0. gain := 2.0. reset := 0.0!
!


!MoogFF class methodsFor: 'instance creation'!
  in: in freq: freq gain: gain reset: reset
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u gain: gain.
    u reset: reset.
    ^u!
  in: in freq: freq gain: gain reset: reset mul: mul
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u gain: gain.
    u reset: reset.
    u mul: mul.
    ^u!
  in: in freq: freq gain: gain reset: reset mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u gain: gain.
    u reset: reset.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:freq:gain:reset:!
!

UGen subclass: #MoogLadder
  instanceVariableNames: 'in ffreq res'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

MoogLadder comment: 'Moog Filter Emulation'!

!MoogLadder methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  ffreq ^ffreq!
  ffreq: x ffreq := x!
  res ^res!
  res: x res := x!
!

!MoogLadder methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #MoogLadder.
    paramNames := #(#in #ffreq #res).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. ffreq := 440.0. res := 0.0!
!


!MoogLadder class methodsFor: 'instance creation'!
  in: in ffreq: ffreq res: res
    |u|
    u := self new.
    u in: in.
    u ffreq: ffreq.
    u res: res.
    ^u!
  in: in ffreq: ffreq res: res mul: mul
    |u|
    u := self new.
    u in: in.
    u ffreq: ffreq.
    u res: res.
    u mul: mul.
    ^u!
  in: in ffreq: ffreq res: res mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u ffreq: ffreq.
    u res: res.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:ffreq:res:!
!

UGen subclass: #MouseButton
  instanceVariableNames: 'minval maxval lag'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

MouseButton comment: 'Mouse button UGen.'!

!MouseButton methodsFor: 'accessing'!
  minval ^minval!
  minval: x minval := x!
  maxval ^maxval!
  maxval: x maxval := x!
  lag ^lag!
  lag: x lag := x!
!

!MouseButton methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #MouseButton.
    paramNames := #(#minval #maxval #lag).
    mceNames := #().
    ugenRate := #kr.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    minval := 0.0. maxval := 1.0. lag := 0.2!
!


!MouseButton class methodsFor: 'instance creation'!
  minval: minval maxval: maxval lag: lag
    |u|
    u := self new.
    u minval: minval.
    u maxval: maxval.
    u lag: lag.
    ^u!
  minval: minval maxval: maxval lag: lag mul: mul
    |u|
    u := self new.
    u minval: minval.
    u maxval: maxval.
    u lag: lag.
    u mul: mul.
    ^u!
  minval: minval maxval: maxval lag: lag mul: mul add: add
    |u|
    u := self new.
    u minval: minval.
    u maxval: maxval.
    u lag: lag.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#minval:maxval:lag:!
!

UGen subclass: #MouseX
  instanceVariableNames: 'minval maxval warp lag'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

MouseX comment: 'Cursor tracking UGen.'!

!MouseX methodsFor: 'accessing'!
  minval ^minval!
  minval: x minval := x!
  maxval ^maxval!
  maxval: x maxval := x!
  warp ^warp!
  warp: x warp := x!
  lag ^lag!
  lag: x lag := x!
!

!MouseX methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #MouseX.
    paramNames := #(#minval #maxval #warp #lag).
    mceNames := #().
    ugenRate := #kr.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    minval := 0.0. maxval := 1.0. warp := 0.0. lag := 0.2!
!


!MouseX class methodsFor: 'instance creation'!
  minval: minval maxval: maxval warp: warp lag: lag
    |u|
    u := self new.
    u minval: minval.
    u maxval: maxval.
    u warp: warp.
    u lag: lag.
    ^u!
  minval: minval maxval: maxval warp: warp lag: lag mul: mul
    |u|
    u := self new.
    u minval: minval.
    u maxval: maxval.
    u warp: warp.
    u lag: lag.
    u mul: mul.
    ^u!
  minval: minval maxval: maxval warp: warp lag: lag mul: mul add: add
    |u|
    u := self new.
    u minval: minval.
    u maxval: maxval.
    u warp: warp.
    u lag: lag.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#minval:maxval:warp:lag:!
!

UGen subclass: #MouseY
  instanceVariableNames: 'minval maxval warp lag'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

MouseY comment: 'Cursor tracking UGen.'!

!MouseY methodsFor: 'accessing'!
  minval ^minval!
  minval: x minval := x!
  maxval ^maxval!
  maxval: x maxval := x!
  warp ^warp!
  warp: x warp := x!
  lag ^lag!
  lag: x lag := x!
!

!MouseY methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #MouseY.
    paramNames := #(#minval #maxval #warp #lag).
    mceNames := #().
    ugenRate := #kr.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    minval := 0.0. maxval := 1.0. warp := 0.0. lag := 0.2!
!


!MouseY class methodsFor: 'instance creation'!
  minval: minval maxval: maxval warp: warp lag: lag
    |u|
    u := self new.
    u minval: minval.
    u maxval: maxval.
    u warp: warp.
    u lag: lag.
    ^u!
  minval: minval maxval: maxval warp: warp lag: lag mul: mul
    |u|
    u := self new.
    u minval: minval.
    u maxval: maxval.
    u warp: warp.
    u lag: lag.
    u mul: mul.
    ^u!
  minval: minval maxval: maxval warp: warp lag: lag mul: mul add: add
    |u|
    u := self new.
    u minval: minval.
    u maxval: maxval.
    u warp: warp.
    u lag: lag.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#minval:maxval:warp:lag:!
!

UGen subclass: #Normalizer
  instanceVariableNames: 'in level dur'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Normalizer comment: 'Flattens dynamics.'!

!Normalizer methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  level ^level!
  level: x level := x!
  dur ^dur!
  dur: x dur := x!
!

!Normalizer methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Normalizer.
    paramNames := #(#in #level #dur).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. level := 1.0. dur := 0.01!
!


!Normalizer class methodsFor: 'instance creation'!
  in: in level: level dur: dur
    |u|
    u := self new.
    u in: in.
    u level: level.
    u dur: dur.
    ^u!
  in: in level: level dur: dur mul: mul
    |u|
    u := self new.
    u in: in.
    u level: level.
    u dur: dur.
    u mul: mul.
    ^u!
  in: in level: level dur: dur mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u level: level.
    u dur: dur.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:level:dur:!
!

UGen subclass: #OnePole
  instanceVariableNames: 'in coef'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

OnePole comment: 'One pole filter.'!

!OnePole methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  coef ^coef!
  coef: x coef := x!
!

!OnePole methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #OnePole.
    paramNames := #(#in #coef).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. coef := 0.5!
!


!OnePole class methodsFor: 'instance creation'!
  in: in coef: coef
    |u|
    u := self new.
    u in: in.
    u coef: coef.
    ^u!
  in: in coef: coef mul: mul
    |u|
    u := self new.
    u in: in.
    u coef: coef.
    u mul: mul.
    ^u!
  in: in coef: coef mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u coef: coef.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:coef:!
!

UGen subclass: #Out
  instanceVariableNames: 'bus channelsArray'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Out comment: 'Write a signal to a bus.'!

!Out methodsFor: 'accessing'!
  bus ^bus!
  bus: x bus := x!
  channelsArray ^channelsArray!
  channelsArray: x channelsArray := x!
!

!Out methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Out.
    paramNames := #(#bus).
    mceNames := #(#channelsArray).
    ugenRate := #ar.
    numChan := 0.
    specialIndex := 0.
    ugenId := nil.
    bus := 0.0!
!


!Out class methodsFor: 'instance creation'!
  bus: bus channelsArray: channelsArray
    |u|
    u := self new.
    u bus: bus.
    u channelsArray: channelsArray.
    ^u!
  bus: bus channelsArray: channelsArray mul: mul
    |u|
    u := self new.
    u bus: bus.
    u channelsArray: channelsArray.
    u mul: mul.
    ^u!
  bus: bus channelsArray: channelsArray mul: mul add: add
    |u|
    u := self new.
    u bus: bus.
    u channelsArray: channelsArray.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#bus:channelsArray:!
!

UGen subclass: #Pan2
  instanceVariableNames: 'in pos level'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Pan2 comment: 'Two channel equal power pan.'!

!Pan2 methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  pos ^pos!
  pos: x pos := x!
  level ^level!
  level: x level := x!
!

!Pan2 methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Pan2.
    paramNames := #(#in #pos #level).
    mceNames := #().
    ugenRate := #ar.
    numChan := 2.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. pos := 0.0. level := 1.0!
!


!Pan2 class methodsFor: 'instance creation'!
  in: in pos: pos level: level
    |u|
    u := self new.
    u in: in.
    u pos: pos.
    u level: level.
    ^u!
  in: in pos: pos level: level mul: mul
    |u|
    u := self new.
    u in: in.
    u pos: pos.
    u level: level.
    u mul: mul.
    ^u!
  in: in pos: pos level: level mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u pos: pos.
    u level: level.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:pos:level:!
!

UGen subclass: #Phasor
  instanceVariableNames: 'trig rate start end resetPos'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Phasor comment: 'A resettable linear ramp between two levels.'!

!Phasor methodsFor: 'accessing'!
  trig ^trig!
  trig: x trig := x!
  rate ^rate!
  rate: x rate := x!
  start ^start!
  start: x start := x!
  end ^end!
  end: x end := x!
  resetPos ^resetPos!
  resetPos: x resetPos := x!
!

!Phasor methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Phasor.
    paramNames := #(#trig #rate #start #end #resetPos).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    trig := 0.0. rate := 1.0. start := 0.0. end := 1.0. resetPos := 0.0!
!


!Phasor class methodsFor: 'instance creation'!
  trig: trig rate: rate start: start end: end resetPos: resetPos
    |u|
    u := self new.
    u trig: trig.
    u rate: rate.
    u start: start.
    u end: end.
    u resetPos: resetPos.
    ^u!
  trig: trig rate: rate start: start end: end resetPos: resetPos mul: mul
    |u|
    u := self new.
    u trig: trig.
    u rate: rate.
    u start: start.
    u end: end.
    u resetPos: resetPos.
    u mul: mul.
    ^u!
  trig: trig rate: rate start: start end: end resetPos: resetPos mul: mul add: add
    |u|
    u := self new.
    u trig: trig.
    u rate: rate.
    u start: start.
    u end: end.
    u resetPos: resetPos.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#trig:rate:start:end:resetPos:!
!

UGen subclass: #PinkNoise
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

PinkNoise comment: 'Pink Noise.'!


!PinkNoise methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #PinkNoise.
    paramNames := #().
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    !
!


!PinkNoise class methodsFor: 'instance creation'!
  mul: mul
    |u|
    u := self new.
    u mul: mul.
    ^u!
  mul: mul add: add
    |u|
    u := self new.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#new!
!

UGen subclass: #Pitch
  instanceVariableNames: 'in initFreq minFreq maxFreq execFreq maxBinsPerOctave median ampThreshold peakThreshold downSample clar'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Pitch comment: 'Autocorrelation pitch follower'!

!Pitch methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  initFreq ^initFreq!
  initFreq: x initFreq := x!
  minFreq ^minFreq!
  minFreq: x minFreq := x!
  maxFreq ^maxFreq!
  maxFreq: x maxFreq := x!
  execFreq ^execFreq!
  execFreq: x execFreq := x!
  maxBinsPerOctave ^maxBinsPerOctave!
  maxBinsPerOctave: x maxBinsPerOctave := x!
  median ^median!
  median: x median := x!
  ampThreshold ^ampThreshold!
  ampThreshold: x ampThreshold := x!
  peakThreshold ^peakThreshold!
  peakThreshold: x peakThreshold := x!
  downSample ^downSample!
  downSample: x downSample := x!
  clar ^clar!
  clar: x clar := x!
!

!Pitch methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Pitch.
    paramNames := #(#in #initFreq #minFreq #maxFreq #execFreq #maxBinsPerOctave #median #ampThreshold #peakThreshold #downSample #clar).
    mceNames := #().
    ugenRate := #kr.
    numChan := 2.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. initFreq := 440.0. minFreq := 60.0. maxFreq := 4000.0. execFreq := 100.0. maxBinsPerOctave := 16.0. median := 1.0. ampThreshold := 0.01. peakThreshold := 0.5. downSample := 1.0. clar := 0.0!
!


!Pitch class methodsFor: 'instance creation'!
  in: in initFreq: initFreq minFreq: minFreq maxFreq: maxFreq execFreq: execFreq maxBinsPerOctave: maxBinsPerOctave median: median ampThreshold: ampThreshold peakThreshold: peakThreshold downSample: downSample clar: clar
    |u|
    u := self new.
    u in: in.
    u initFreq: initFreq.
    u minFreq: minFreq.
    u maxFreq: maxFreq.
    u execFreq: execFreq.
    u maxBinsPerOctave: maxBinsPerOctave.
    u median: median.
    u ampThreshold: ampThreshold.
    u peakThreshold: peakThreshold.
    u downSample: downSample.
    u clar: clar.
    ^u!
  in: in initFreq: initFreq minFreq: minFreq maxFreq: maxFreq execFreq: execFreq maxBinsPerOctave: maxBinsPerOctave median: median ampThreshold: ampThreshold peakThreshold: peakThreshold downSample: downSample clar: clar mul: mul
    |u|
    u := self new.
    u in: in.
    u initFreq: initFreq.
    u minFreq: minFreq.
    u maxFreq: maxFreq.
    u execFreq: execFreq.
    u maxBinsPerOctave: maxBinsPerOctave.
    u median: median.
    u ampThreshold: ampThreshold.
    u peakThreshold: peakThreshold.
    u downSample: downSample.
    u clar: clar.
    u mul: mul.
    ^u!
  in: in initFreq: initFreq minFreq: minFreq maxFreq: maxFreq execFreq: execFreq maxBinsPerOctave: maxBinsPerOctave median: median ampThreshold: ampThreshold peakThreshold: peakThreshold downSample: downSample clar: clar mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u initFreq: initFreq.
    u minFreq: minFreq.
    u maxFreq: maxFreq.
    u execFreq: execFreq.
    u maxBinsPerOctave: maxBinsPerOctave.
    u median: median.
    u ampThreshold: ampThreshold.
    u peakThreshold: peakThreshold.
    u downSample: downSample.
    u clar: clar.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:initFreq:minFreq:maxFreq:execFreq:maxBinsPerOctave:median:ampThreshold:peakThreshold:downSample:clar:!
!

UGen subclass: #PitchShift
  instanceVariableNames: 'in windowSize pitchRatio pitchDispersion timeDispersion'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

PitchShift comment: 'Time domain pitch shifter.'!

!PitchShift methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  windowSize ^windowSize!
  windowSize: x windowSize := x!
  pitchRatio ^pitchRatio!
  pitchRatio: x pitchRatio := x!
  pitchDispersion ^pitchDispersion!
  pitchDispersion: x pitchDispersion := x!
  timeDispersion ^timeDispersion!
  timeDispersion: x timeDispersion := x!
!

!PitchShift methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #PitchShift.
    paramNames := #(#in #windowSize #pitchRatio #pitchDispersion #timeDispersion).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. windowSize := 0.2. pitchRatio := 1.0. pitchDispersion := 0.0. timeDispersion := 0.0!
!


!PitchShift class methodsFor: 'instance creation'!
  in: in windowSize: windowSize pitchRatio: pitchRatio pitchDispersion: pitchDispersion timeDispersion: timeDispersion
    |u|
    u := self new.
    u in: in.
    u windowSize: windowSize.
    u pitchRatio: pitchRatio.
    u pitchDispersion: pitchDispersion.
    u timeDispersion: timeDispersion.
    ^u!
  in: in windowSize: windowSize pitchRatio: pitchRatio pitchDispersion: pitchDispersion timeDispersion: timeDispersion mul: mul
    |u|
    u := self new.
    u in: in.
    u windowSize: windowSize.
    u pitchRatio: pitchRatio.
    u pitchDispersion: pitchDispersion.
    u timeDispersion: timeDispersion.
    u mul: mul.
    ^u!
  in: in windowSize: windowSize pitchRatio: pitchRatio pitchDispersion: pitchDispersion timeDispersion: timeDispersion mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u windowSize: windowSize.
    u pitchRatio: pitchRatio.
    u pitchDispersion: pitchDispersion.
    u timeDispersion: timeDispersion.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:windowSize:pitchRatio:pitchDispersion:timeDispersion:!
!

UGen subclass: #Pluck
  instanceVariableNames: 'in trig maxdelaytime delaytime decaytime coef'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Pluck comment: 'A Karplus-Strong UGen'!

!Pluck methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  trig ^trig!
  trig: x trig := x!
  maxdelaytime ^maxdelaytime!
  maxdelaytime: x maxdelaytime := x!
  delaytime ^delaytime!
  delaytime: x delaytime := x!
  decaytime ^decaytime!
  decaytime: x decaytime := x!
  coef ^coef!
  coef: x coef := x!
!

!Pluck methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Pluck.
    paramNames := #(#in #trig #maxdelaytime #delaytime #decaytime #coef).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. trig := 1.0. maxdelaytime := 0.2. delaytime := 0.2. decaytime := 1.0. coef := 0.5!
!


!Pluck class methodsFor: 'instance creation'!
  in: in trig: trig maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime coef: coef
    |u|
    u := self new.
    u in: in.
    u trig: trig.
    u maxdelaytime: maxdelaytime.
    u delaytime: delaytime.
    u decaytime: decaytime.
    u coef: coef.
    ^u!
  in: in trig: trig maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime coef: coef mul: mul
    |u|
    u := self new.
    u in: in.
    u trig: trig.
    u maxdelaytime: maxdelaytime.
    u delaytime: delaytime.
    u decaytime: decaytime.
    u coef: coef.
    u mul: mul.
    ^u!
  in: in trig: trig maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime coef: coef mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u trig: trig.
    u maxdelaytime: maxdelaytime.
    u delaytime: delaytime.
    u decaytime: decaytime.
    u coef: coef.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:trig:maxdelaytime:delaytime:decaytime:coef:!
!

UGen subclass: #Pulse
  instanceVariableNames: 'freq width'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Pulse comment: 'Band limited pulse wave.'!

!Pulse methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
  width ^width!
  width: x width := x!
!

!Pulse methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Pulse.
    paramNames := #(#freq #width).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 440.0. width := 0.5!
!


!Pulse class methodsFor: 'instance creation'!
  freq: freq width: width
    |u|
    u := self new.
    u freq: freq.
    u width: width.
    ^u!
  freq: freq width: width mul: mul
    |u|
    u := self new.
    u freq: freq.
    u width: width.
    u mul: mul.
    ^u!
  freq: freq width: width mul: mul add: add
    |u|
    u := self new.
    u freq: freq.
    u width: width.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#freq:width:!
!

UGen subclass: #PulseCount
  instanceVariableNames: 'trig reset'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

PulseCount comment: 'Pulse counter.'!

!PulseCount methodsFor: 'accessing'!
  trig ^trig!
  trig: x trig := x!
  reset ^reset!
  reset: x reset := x!
!

!PulseCount methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #PulseCount.
    paramNames := #(#trig #reset).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    trig := 0.0. reset := 0.0!
!


!PulseCount class methodsFor: 'instance creation'!
  trig: trig reset: reset
    |u|
    u := self new.
    u trig: trig.
    u reset: reset.
    ^u!
  trig: trig reset: reset mul: mul
    |u|
    u := self new.
    u trig: trig.
    u reset: reset.
    u mul: mul.
    ^u!
  trig: trig reset: reset mul: mul add: add
    |u|
    u := self new.
    u trig: trig.
    u reset: reset.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#trig:reset:!
!

UGen subclass: #PulseDivider
  instanceVariableNames: 'trig div start'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

PulseDivider comment: 'Pulse divider.'!

!PulseDivider methodsFor: 'accessing'!
  trig ^trig!
  trig: x trig := x!
  div ^div!
  div: x div := x!
  start ^start!
  start: x start := x!
!

!PulseDivider methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #PulseDivider.
    paramNames := #(#trig #div #start).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    trig := 0.0. div := 2.0. start := 0.0!
!


!PulseDivider class methodsFor: 'instance creation'!
  trig: trig div: div start: start
    |u|
    u := self new.
    u trig: trig.
    u div: div.
    u start: start.
    ^u!
  trig: trig div: div start: start mul: mul
    |u|
    u := self new.
    u trig: trig.
    u div: div.
    u start: start.
    u mul: mul.
    ^u!
  trig: trig div: div start: start mul: mul add: add
    |u|
    u := self new.
    u trig: trig.
    u div: div.
    u start: start.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#trig:div:start:!
!

UGen subclass: #RHPF
  instanceVariableNames: 'in freq rq'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

RHPF comment: 'A resonant high pass filter.'!

!RHPF methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  freq ^freq!
  freq: x freq := x!
  rq ^rq!
  rq: x rq := x!
!

!RHPF methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #RHPF.
    paramNames := #(#in #freq #rq).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. freq := 440.0. rq := 1.0!
!


!RHPF class methodsFor: 'instance creation'!
  in: in freq: freq rq: rq
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u rq: rq.
    ^u!
  in: in freq: freq rq: rq mul: mul
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u rq: rq.
    u mul: mul.
    ^u!
  in: in freq: freq rq: rq mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u rq: rq.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:freq:rq:!
!

UGen subclass: #RLPF
  instanceVariableNames: 'in freq rq'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

RLPF comment: 'A resonant low pass filter.'!

!RLPF methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  freq ^freq!
  freq: x freq := x!
  rq ^rq!
  rq: x rq := x!
!

!RLPF methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #RLPF.
    paramNames := #(#in #freq #rq).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. freq := 440.0. rq := 1.0!
!


!RLPF class methodsFor: 'instance creation'!
  in: in freq: freq rq: rq
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u rq: rq.
    ^u!
  in: in freq: freq rq: rq mul: mul
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u rq: rq.
    u mul: mul.
    ^u!
  in: in freq: freq rq: rq mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u rq: rq.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:freq:rq:!
!

UGen subclass: #Rand
  instanceVariableNames: 'lo hi'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Rand comment: 'Single random number generator.'!

!Rand methodsFor: 'accessing'!
  lo ^lo!
  lo: x lo := x!
  hi ^hi!
  hi: x hi := x!
!

!Rand methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Rand.
    paramNames := #(#lo #hi).
    mceNames := #().
    ugenRate := #ir.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    lo := 0.0. hi := 1.0!
!


!Rand class methodsFor: 'instance creation'!
  lo: lo hi: hi
    |u|
    u := self new.
    u lo: lo.
    u hi: hi.
    ^u!
  lo: lo hi: hi mul: mul
    |u|
    u := self new.
    u lo: lo.
    u hi: hi.
    u mul: mul.
    ^u!
  lo: lo hi: hi mul: mul add: add
    |u|
    u := self new.
    u lo: lo.
    u hi: hi.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#lo:hi:!
!

UGen subclass: #RecordBuf
  instanceVariableNames: 'bufnum offset recLevel preLevel run loop trigger doneAction inputArray'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

RecordBuf comment: 'Record or overdub into a Buffer.'!

!RecordBuf methodsFor: 'accessing'!
  bufnum ^bufnum!
  bufnum: x bufnum := x!
  offset ^offset!
  offset: x offset := x!
  recLevel ^recLevel!
  recLevel: x recLevel := x!
  preLevel ^preLevel!
  preLevel: x preLevel := x!
  run ^run!
  run: x run := x!
  loop ^loop!
  loop: x loop := x!
  trigger ^trigger!
  trigger: x trigger := x!
  doneAction ^doneAction!
  doneAction: x doneAction := x!
  inputArray ^inputArray!
  inputArray: x inputArray := x!
!

!RecordBuf methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #RecordBuf.
    paramNames := #(#bufnum #offset #recLevel #preLevel #run #loop #trigger #doneAction).
    mceNames := #(#inputArray).
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    bufnum := 0.0. offset := 0.0. recLevel := 1.0. preLevel := 0.0. run := 1.0. loop := 1.0. trigger := 1.0. doneAction := 0.0!
!


!RecordBuf class methodsFor: 'instance creation'!
  bufnum: bufnum offset: offset recLevel: recLevel preLevel: preLevel run: run loop: loop trigger: trigger doneAction: doneAction inputArray: inputArray
    |u|
    u := self new.
    u bufnum: bufnum.
    u offset: offset.
    u recLevel: recLevel.
    u preLevel: preLevel.
    u run: run.
    u loop: loop.
    u trigger: trigger.
    u doneAction: doneAction.
    u inputArray: inputArray.
    ^u!
  bufnum: bufnum offset: offset recLevel: recLevel preLevel: preLevel run: run loop: loop trigger: trigger doneAction: doneAction inputArray: inputArray mul: mul
    |u|
    u := self new.
    u bufnum: bufnum.
    u offset: offset.
    u recLevel: recLevel.
    u preLevel: preLevel.
    u run: run.
    u loop: loop.
    u trigger: trigger.
    u doneAction: doneAction.
    u inputArray: inputArray.
    u mul: mul.
    ^u!
  bufnum: bufnum offset: offset recLevel: recLevel preLevel: preLevel run: run loop: loop trigger: trigger doneAction: doneAction inputArray: inputArray mul: mul add: add
    |u|
    u := self new.
    u bufnum: bufnum.
    u offset: offset.
    u recLevel: recLevel.
    u preLevel: preLevel.
    u run: run.
    u loop: loop.
    u trigger: trigger.
    u doneAction: doneAction.
    u inputArray: inputArray.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#bufnum:offset:recLevel:preLevel:run:loop:trigger:doneAction:inputArray:!
!

UGen subclass: #Resonz
  instanceVariableNames: 'in freq bwr'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Resonz comment: 'Resonant filter.'!

!Resonz methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  freq ^freq!
  freq: x freq := x!
  bwr ^bwr!
  bwr: x bwr := x!
!

!Resonz methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Resonz.
    paramNames := #(#in #freq #bwr).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. freq := 440.0. bwr := 1.0!
!


!Resonz class methodsFor: 'instance creation'!
  in: in freq: freq bwr: bwr
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u bwr: bwr.
    ^u!
  in: in freq: freq bwr: bwr mul: mul
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u bwr: bwr.
    u mul: mul.
    ^u!
  in: in freq: freq bwr: bwr mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u bwr: bwr.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:freq:bwr:!
!

UGen subclass: #Ringz
  instanceVariableNames: 'in freq decaytime'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Ringz comment: 'Ringing filter.'!

!Ringz methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  freq ^freq!
  freq: x freq := x!
  decaytime ^decaytime!
  decaytime: x decaytime := x!
!

!Ringz methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Ringz.
    paramNames := #(#in #freq #decaytime).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. freq := 440.0. decaytime := 1.0!
!


!Ringz class methodsFor: 'instance creation'!
  in: in freq: freq decaytime: decaytime
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u decaytime: decaytime.
    ^u!
  in: in freq: freq decaytime: decaytime mul: mul
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u decaytime: decaytime.
    u mul: mul.
    ^u!
  in: in freq: freq decaytime: decaytime mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u decaytime: decaytime.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:freq:decaytime:!
!

UGen subclass: #RunningMax
  instanceVariableNames: 'in trig'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

RunningMax comment: 'Track maximum level.'!

!RunningMax methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  trig ^trig!
  trig: x trig := x!
!

!RunningMax methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #RunningMax.
    paramNames := #(#in #trig).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. trig := 0.0!
!


!RunningMax class methodsFor: 'instance creation'!
  in: in trig: trig
    |u|
    u := self new.
    u in: in.
    u trig: trig.
    ^u!
  in: in trig: trig mul: mul
    |u|
    u := self new.
    u in: in.
    u trig: trig.
    u mul: mul.
    ^u!
  in: in trig: trig mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u trig: trig.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:trig:!
!

UGen subclass: #RBezier
  instanceVariableNames: 'haltAfter dx freq phase param'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

RBezier comment: '(Undocumented class)'!

!RBezier methodsFor: 'accessing'!
  haltAfter ^haltAfter!
  haltAfter: x haltAfter := x!
  dx ^dx!
  dx: x dx := x!
  freq ^freq!
  freq: x freq := x!
  phase ^phase!
  phase: x phase := x!
  param ^param!
  param: x param := x!
!

!RBezier methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #RBezier.
    paramNames := #(#haltAfter #dx #freq #phase).
    mceNames := #(#param).
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    haltAfter := 100.0. dx := 0.0001. freq := 440.0. phase := 0.0!
!


!RBezier class methodsFor: 'instance creation'!
  haltAfter: haltAfter dx: dx freq: freq phase: phase param: param
    |u|
    u := self new.
    u haltAfter: haltAfter.
    u dx: dx.
    u freq: freq.
    u phase: phase.
    u param: param.
    ^u!
  haltAfter: haltAfter dx: dx freq: freq phase: phase param: param mul: mul
    |u|
    u := self new.
    u haltAfter: haltAfter.
    u dx: dx.
    u freq: freq.
    u phase: phase.
    u param: param.
    u mul: mul.
    ^u!
  haltAfter: haltAfter dx: dx freq: freq phase: phase param: param mul: mul add: add
    |u|
    u := self new.
    u haltAfter: haltAfter.
    u dx: dx.
    u freq: freq.
    u phase: phase.
    u param: param.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#haltAfter:dx:freq:phase:param:!
!

UGen subclass: #RDX7Env
  instanceVariableNames: 'gate data r1 r2 r3 r4 l1 l2 l3 l4 ol'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

RDX7Env comment: '(Undocumented class)'!

!RDX7Env methodsFor: 'accessing'!
  gate ^gate!
  gate: x gate := x!
  data ^data!
  data: x data := x!
  r1 ^r1!
  r1: x r1 := x!
  r2 ^r2!
  r2: x r2 := x!
  r3 ^r3!
  r3: x r3 := x!
  r4 ^r4!
  r4: x r4 := x!
  l1 ^l1!
  l1: x l1 := x!
  l2 ^l2!
  l2: x l2 := x!
  l3 ^l3!
  l3: x l3 := x!
  l4 ^l4!
  l4: x l4 := x!
  ol ^ol!
  ol: x ol := x!
!

!RDX7Env methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #RDX7Env.
    paramNames := #(#gate #data #r1 #r2 #r3 #r4 #l1 #l2 #l3 #l4 #ol).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    gate := 0.0. data := 0.0. r1 := 0.0. r2 := 0.0. r3 := 0.0. r4 := 0.0. l1 := 0.0. l2 := 0.0. l3 := 0.0. l4 := 0.0. ol := 0.0!
!


!RDX7Env class methodsFor: 'instance creation'!
  gate: gate data: data r1: r1 r2: r2 r3: r3 r4: r4 l1: l1 l2: l2 l3: l3 l4: l4 ol: ol
    |u|
    u := self new.
    u gate: gate.
    u data: data.
    u r1: r1.
    u r2: r2.
    u r3: r3.
    u r4: r4.
    u l1: l1.
    u l2: l2.
    u l3: l3.
    u l4: l4.
    u ol: ol.
    ^u!
  gate: gate data: data r1: r1 r2: r2 r3: r3 r4: r4 l1: l1 l2: l2 l3: l3 l4: l4 ol: ol mul: mul
    |u|
    u := self new.
    u gate: gate.
    u data: data.
    u r1: r1.
    u r2: r2.
    u r3: r3.
    u r4: r4.
    u l1: l1.
    u l2: l2.
    u l3: l3.
    u l4: l4.
    u ol: ol.
    u mul: mul.
    ^u!
  gate: gate data: data r1: r1 r2: r2 r3: r3 r4: r4 l1: l1 l2: l2 l3: l3 l4: l4 ol: ol mul: mul add: add
    |u|
    u := self new.
    u gate: gate.
    u data: data.
    u r1: r1.
    u r2: r2.
    u r3: r3.
    u r4: r4.
    u l1: l1.
    u l2: l2.
    u l3: l3.
    u l4: l4.
    u ol: ol.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#gate:data:r1:r2:r3:r4:l1:l2:l3:l4:ol:!
!

UGen subclass: #RExpRandN
  instanceVariableNames: 'lo hi'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

RExpRandN comment: '(Undocumented class)'!

!RExpRandN methodsFor: 'accessing'!
  lo ^lo!
  lo: x lo := x!
  hi ^hi!
  hi: x hi := x!
!

!RExpRandN methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #RExpRandN.
    paramNames := #(#lo #hi).
    mceNames := #().
    ugenRate := #ir.
    numChan := nil.
    specialIndex := 0.
    ugenId := nil.
    lo := 0.0. hi := 1.0!
!


!RExpRandN class methodsFor: 'instance creation'!
  numChan: numChan lo: lo hi: hi
    |u|
    u := self new.
    u numChan: numChan.
    u lo: lo.
    u hi: hi.
    ^u!
  numChan: numChan lo: lo hi: hi mul: mul
    |u|
    u := self new.
    u numChan: numChan.
    u lo: lo.
    u hi: hi.
    u mul: mul.
    ^u!
  numChan: numChan lo: lo hi: hi mul: mul add: add
    |u|
    u := self new.
    u numChan: numChan.
    u lo: lo.
    u hi: hi.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#numChan:lo:hi:!
!

UGen subclass: #RRandN
  instanceVariableNames: 'lo hi'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

RRandN comment: '(Undocumented class)'!

!RRandN methodsFor: 'accessing'!
  lo ^lo!
  lo: x lo := x!
  hi ^hi!
  hi: x hi := x!
!

!RRandN methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #RRandN.
    paramNames := #(#lo #hi).
    mceNames := #().
    ugenRate := #ir.
    numChan := nil.
    specialIndex := 0.
    ugenId := nil.
    lo := 0.0. hi := 1.0!
!


!RRandN class methodsFor: 'instance creation'!
  numChan: numChan lo: lo hi: hi
    |u|
    u := self new.
    u numChan: numChan.
    u lo: lo.
    u hi: hi.
    ^u!
  numChan: numChan lo: lo hi: hi mul: mul
    |u|
    u := self new.
    u numChan: numChan.
    u lo: lo.
    u hi: hi.
    u mul: mul.
    ^u!
  numChan: numChan lo: lo hi: hi mul: mul add: add
    |u|
    u := self new.
    u numChan: numChan.
    u lo: lo.
    u hi: hi.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#numChan:lo:hi:!
!

UGen subclass: #RTScramble
  instanceVariableNames: 'trigger inputs'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

RTScramble comment: '(Undocumented class)'!

!RTScramble methodsFor: 'accessing'!
  trigger ^trigger!
  trigger: x trigger := x!
  inputs ^inputs!
  inputs: x inputs := x!
!

!RTScramble methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #RTScramble.
    paramNames := #(#trigger).
    mceNames := #(#inputs).
    ugenRate := #kr.
    numChan := #mceInput.
    specialIndex := 0.
    ugenId := nil.
    trigger := 0.0!
!


!RTScramble class methodsFor: 'instance creation'!
  trigger: trigger inputs: inputs
    |u|
    u := self new.
    u trigger: trigger.
    u inputs: inputs.
    ^u!
  trigger: trigger inputs: inputs mul: mul
    |u|
    u := self new.
    u trigger: trigger.
    u inputs: inputs.
    u mul: mul.
    ^u!
  trigger: trigger inputs: inputs mul: mul add: add
    |u|
    u := self new.
    u trigger: trigger.
    u inputs: inputs.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#trigger:inputs:!
!

UGen subclass: #SampleDur
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

SampleDur comment: 'Duration of one sample.'!


!SampleDur methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #SampleDur.
    paramNames := #().
    mceNames := #().
    ugenRate := #ir.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    !
!


!SampleDur class methodsFor: 'instance creation'!
  mul: mul
    |u|
    u := self new.
    u mul: mul.
    ^u!
  mul: mul add: add
    |u|
    u := self new.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#new!
!

UGen subclass: #SampleRate
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

SampleRate comment: 'Server sample rate.'!


!SampleRate methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #SampleRate.
    paramNames := #().
    mceNames := #().
    ugenRate := #ir.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    !
!


!SampleRate class methodsFor: 'instance creation'!
  mul: mul
    |u|
    u := self new.
    u mul: mul.
    ^u!
  mul: mul add: add
    |u|
    u := self new.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#new!
!

UGen subclass: #Saw
  instanceVariableNames: 'freq'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Saw comment: 'Band limited sawtooth.'!

!Saw methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
!

!Saw methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Saw.
    paramNames := #(#freq).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 440.0!
!


!Saw class methodsFor: 'instance creation'!
  freq: freq
    |u|
    u := self new.
    u freq: freq.
    ^u!
  freq: freq mul: mul
    |u|
    u := self new.
    u freq: freq.
    u mul: mul.
    ^u!
  freq: freq mul: mul add: add
    |u|
    u := self new.
    u freq: freq.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#freq:!
!

UGen subclass: #Select
  instanceVariableNames: 'which array'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Select comment: 'Select output from an array of inputs.'!

!Select methodsFor: 'accessing'!
  which ^which!
  which: x which := x!
  array ^array!
  array: x array := x!
!

!Select methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Select.
    paramNames := #(#which).
    mceNames := #(#array).
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    which := 0.0!
!


!Select class methodsFor: 'instance creation'!
  which: which array: array
    |u|
    u := self new.
    u which: which.
    u array: array.
    ^u!
  which: which array: array mul: mul
    |u|
    u := self new.
    u which: which.
    u array: array.
    u mul: mul.
    ^u!
  which: which array: array mul: mul add: add
    |u|
    u := self new.
    u which: which.
    u array: array.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#which:array:!
!

UGen subclass: #SetBuf
  instanceVariableNames: 'buf offset length array'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

SetBuf comment: 'Set local buffer'!

!SetBuf methodsFor: 'accessing'!
  buf ^buf!
  buf: x buf := x!
  offset ^offset!
  offset: x offset := x!
  length ^length!
  length: x length := x!
  array ^array!
  array: x array := x!
!

!SetBuf methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #SetBuf.
    paramNames := #(#buf #offset #length).
    mceNames := #(#array).
    ugenRate := #ir.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    buf := 0.0. offset := 0.0. length := 0.0!
!


!SetBuf class methodsFor: 'instance creation'!
  buf: buf offset: offset length: length array: array
    |u|
    u := self new.
    u buf: buf.
    u offset: offset.
    u length: length.
    u array: array.
    ^u!
  buf: buf offset: offset length: length array: array mul: mul
    |u|
    u := self new.
    u buf: buf.
    u offset: offset.
    u length: length.
    u array: array.
    u mul: mul.
    ^u!
  buf: buf offset: offset length: length array: array mul: mul add: add
    |u|
    u := self new.
    u buf: buf.
    u offset: offset.
    u length: length.
    u array: array.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#buf:offset:length:array:!
!

UGen subclass: #SetResetFF
  instanceVariableNames: 'trig reset'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

SetResetFF comment: 'Set-reset flip flop.'!

!SetResetFF methodsFor: 'accessing'!
  trig ^trig!
  trig: x trig := x!
  reset ^reset!
  reset: x reset := x!
!

!SetResetFF methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #SetResetFF.
    paramNames := #(#trig #reset).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    trig := 0.0. reset := 0.0!
!


!SetResetFF class methodsFor: 'instance creation'!
  trig: trig reset: reset
    |u|
    u := self new.
    u trig: trig.
    u reset: reset.
    ^u!
  trig: trig reset: reset mul: mul
    |u|
    u := self new.
    u trig: trig.
    u reset: reset.
    u mul: mul.
    ^u!
  trig: trig reset: reset mul: mul add: add
    |u|
    u := self new.
    u trig: trig.
    u reset: reset.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#trig:reset:!
!

UGen subclass: #SinOsc
  instanceVariableNames: 'freq phase'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

SinOsc comment: 'Interpolating sine wavetable oscillator.'!

!SinOsc methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
  phase ^phase!
  phase: x phase := x!
!

!SinOsc methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #SinOsc.
    paramNames := #(#freq #phase).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 440.0. phase := 0.0!
!


!SinOsc class methodsFor: 'instance creation'!
  freq: freq phase: phase
    |u|
    u := self new.
    u freq: freq.
    u phase: phase.
    ^u!
  freq: freq phase: phase mul: mul
    |u|
    u := self new.
    u freq: freq.
    u phase: phase.
    u mul: mul.
    ^u!
  freq: freq phase: phase mul: mul add: add
    |u|
    u := self new.
    u freq: freq.
    u phase: phase.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#freq:phase:!
!

UGen subclass: #SinOscFB
  instanceVariableNames: 'freq feedback'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

SinOscFB comment: 'Feedback FM oscillator'!

!SinOscFB methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
  feedback ^feedback!
  feedback: x feedback := x!
!

!SinOscFB methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #SinOscFB.
    paramNames := #(#freq #feedback).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 440.0. feedback := 0.0!
!


!SinOscFB class methodsFor: 'instance creation'!
  freq: freq feedback: feedback
    |u|
    u := self new.
    u freq: freq.
    u feedback: feedback.
    ^u!
  freq: freq feedback: feedback mul: mul
    |u|
    u := self new.
    u freq: freq.
    u feedback: feedback.
    u mul: mul.
    ^u!
  freq: freq feedback: feedback mul: mul add: add
    |u|
    u := self new.
    u freq: freq.
    u feedback: feedback.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#freq:feedback:!
!

UGen subclass: #Slope
  instanceVariableNames: 'in'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Slope comment: 'Slope of signal'!

!Slope methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
!

!Slope methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Slope.
    paramNames := #(#in).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0!
!


!Slope class methodsFor: 'instance creation'!
  in: in
    |u|
    u := self new.
    u in: in.
    ^u!
  in: in mul: mul
    |u|
    u := self new.
    u in: in.
    u mul: mul.
    ^u!
  in: in mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:!
!

UGen subclass: #Stepper
  instanceVariableNames: 'trig reset min max step resetval'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Stepper comment: 'Pulse counter.'!

!Stepper methodsFor: 'accessing'!
  trig ^trig!
  trig: x trig := x!
  reset ^reset!
  reset: x reset := x!
  min ^min!
  min: x min := x!
  max ^max!
  max: x max := x!
  step ^step!
  step: x step := x!
  resetval ^resetval!
  resetval: x resetval := x!
!

!Stepper methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Stepper.
    paramNames := #(#trig #reset #min #max #step #resetval).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    trig := 0.0. reset := 0.0. min := 0.0. max := 7.0. step := 1.0. resetval := 0.0!
!


!Stepper class methodsFor: 'instance creation'!
  trig: trig reset: reset min: min max: max step: step resetval: resetval
    |u|
    u := self new.
    u trig: trig.
    u reset: reset.
    u min: min.
    u max: max.
    u step: step.
    u resetval: resetval.
    ^u!
  trig: trig reset: reset min: min max: max step: step resetval: resetval mul: mul
    |u|
    u := self new.
    u trig: trig.
    u reset: reset.
    u min: min.
    u max: max.
    u step: step.
    u resetval: resetval.
    u mul: mul.
    ^u!
  trig: trig reset: reset min: min max: max step: step resetval: resetval mul: mul add: add
    |u|
    u := self new.
    u trig: trig.
    u reset: reset.
    u min: min.
    u max: max.
    u step: step.
    u resetval: resetval.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#trig:reset:min:max:step:resetval:!
!

UGen subclass: #Sweep
  instanceVariableNames: 'trig rate'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Sweep comment: 'Triggered linear ramp'!

!Sweep methodsFor: 'accessing'!
  trig ^trig!
  trig: x trig := x!
  rate ^rate!
  rate: x rate := x!
!

!Sweep methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Sweep.
    paramNames := #(#trig #rate).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    trig := 0.0. rate := 1.0!
!


!Sweep class methodsFor: 'instance creation'!
  trig: trig rate: rate
    |u|
    u := self new.
    u trig: trig.
    u rate: rate.
    ^u!
  trig: trig rate: rate mul: mul
    |u|
    u := self new.
    u trig: trig.
    u rate: rate.
    u mul: mul.
    ^u!
  trig: trig rate: rate mul: mul add: add
    |u|
    u := self new.
    u trig: trig.
    u rate: rate.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#trig:rate:!
!

UGen subclass: #SyncSaw
  instanceVariableNames: 'syncFreq sawFreq'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

SyncSaw comment: 'Hard sync sawtooth wave.'!

!SyncSaw methodsFor: 'accessing'!
  syncFreq ^syncFreq!
  syncFreq: x syncFreq := x!
  sawFreq ^sawFreq!
  sawFreq: x sawFreq := x!
!

!SyncSaw methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #SyncSaw.
    paramNames := #(#syncFreq #sawFreq).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    syncFreq := 440.0. sawFreq := 440.0!
!


!SyncSaw class methodsFor: 'instance creation'!
  syncFreq: syncFreq sawFreq: sawFreq
    |u|
    u := self new.
    u syncFreq: syncFreq.
    u sawFreq: sawFreq.
    ^u!
  syncFreq: syncFreq sawFreq: sawFreq mul: mul
    |u|
    u := self new.
    u syncFreq: syncFreq.
    u sawFreq: sawFreq.
    u mul: mul.
    ^u!
  syncFreq: syncFreq sawFreq: sawFreq mul: mul add: add
    |u|
    u := self new.
    u syncFreq: syncFreq.
    u sawFreq: sawFreq.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#syncFreq:sawFreq:!
!

UGen subclass: #TDuty
  instanceVariableNames: 'dur reset doneAction level gapFirst'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

TDuty comment: 'Demand results as trigger from demand rate UGens.'!

!TDuty methodsFor: 'accessing'!
  dur ^dur!
  dur: x dur := x!
  reset ^reset!
  reset: x reset := x!
  doneAction ^doneAction!
  doneAction: x doneAction := x!
  level ^level!
  level: x level := x!
  gapFirst ^gapFirst!
  gapFirst: x gapFirst := x!
!

!TDuty methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #TDuty.
    paramNames := #(#dur #reset #doneAction #level #gapFirst).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    dur := 1.0. reset := 0.0. doneAction := 0.0. level := 1.0. gapFirst := 0.0!
!


!TDuty class methodsFor: 'instance creation'!
  dur: dur reset: reset doneAction: doneAction level: level gapFirst: gapFirst
    |u|
    u := self new.
    u dur: dur.
    u reset: reset.
    u doneAction: doneAction.
    u level: level.
    u gapFirst: gapFirst.
    ^u!
  dur: dur reset: reset doneAction: doneAction level: level gapFirst: gapFirst mul: mul
    |u|
    u := self new.
    u dur: dur.
    u reset: reset.
    u doneAction: doneAction.
    u level: level.
    u gapFirst: gapFirst.
    u mul: mul.
    ^u!
  dur: dur reset: reset doneAction: doneAction level: level gapFirst: gapFirst mul: mul add: add
    |u|
    u := self new.
    u dur: dur.
    u reset: reset.
    u doneAction: doneAction.
    u level: level.
    u gapFirst: gapFirst.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#dur:reset:doneAction:level:gapFirst:!
!

UGen subclass: #TExpRand
  instanceVariableNames: 'lo hi trig'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

TExpRand comment: 'Triggered exponential random number generator.'!

!TExpRand methodsFor: 'accessing'!
  lo ^lo!
  lo: x lo := x!
  hi ^hi!
  hi: x hi := x!
  trig ^trig!
  trig: x trig := x!
!

!TExpRand methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #TExpRand.
    paramNames := #(#lo #hi #trig).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    lo := 0.01. hi := 1.0. trig := 0.0!
!


!TExpRand class methodsFor: 'instance creation'!
  lo: lo hi: hi trig: trig
    |u|
    u := self new.
    u lo: lo.
    u hi: hi.
    u trig: trig.
    ^u!
  lo: lo hi: hi trig: trig mul: mul
    |u|
    u := self new.
    u lo: lo.
    u hi: hi.
    u trig: trig.
    u mul: mul.
    ^u!
  lo: lo hi: hi trig: trig mul: mul add: add
    |u|
    u := self new.
    u lo: lo.
    u hi: hi.
    u trig: trig.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#lo:hi:trig:!
!

UGen subclass: #TGrains
  instanceVariableNames: 'trigger bufnum rate centerPos dur pan amp interp'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

TGrains comment: 'Buffer granulator.'!

!TGrains methodsFor: 'accessing'!
  trigger ^trigger!
  trigger: x trigger := x!
  bufnum ^bufnum!
  bufnum: x bufnum := x!
  rate ^rate!
  rate: x rate := x!
  centerPos ^centerPos!
  centerPos: x centerPos := x!
  dur ^dur!
  dur: x dur := x!
  pan ^pan!
  pan: x pan := x!
  amp ^amp!
  amp: x amp := x!
  interp ^interp!
  interp: x interp := x!
!

!TGrains methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #TGrains.
    paramNames := #(#trigger #bufnum #rate #centerPos #dur #pan #amp #interp).
    mceNames := #().
    ugenRate := #ar.
    numChan := nil.
    specialIndex := 0.
    ugenId := nil.
    trigger := 0.0. bufnum := 0.0. rate := 1.0. centerPos := 0.0. dur := 0.1. pan := 0.0. amp := 0.1. interp := 4.0!
!


!TGrains class methodsFor: 'instance creation'!
  numChan: numChan trigger: trigger bufnum: bufnum rate: rate centerPos: centerPos dur: dur pan: pan amp: amp interp: interp
    |u|
    u := self new.
    u numChan: numChan.
    u trigger: trigger.
    u bufnum: bufnum.
    u rate: rate.
    u centerPos: centerPos.
    u dur: dur.
    u pan: pan.
    u amp: amp.
    u interp: interp.
    ^u!
  numChan: numChan trigger: trigger bufnum: bufnum rate: rate centerPos: centerPos dur: dur pan: pan amp: amp interp: interp mul: mul
    |u|
    u := self new.
    u numChan: numChan.
    u trigger: trigger.
    u bufnum: bufnum.
    u rate: rate.
    u centerPos: centerPos.
    u dur: dur.
    u pan: pan.
    u amp: amp.
    u interp: interp.
    u mul: mul.
    ^u!
  numChan: numChan trigger: trigger bufnum: bufnum rate: rate centerPos: centerPos dur: dur pan: pan amp: amp interp: interp mul: mul add: add
    |u|
    u := self new.
    u numChan: numChan.
    u trigger: trigger.
    u bufnum: bufnum.
    u rate: rate.
    u centerPos: centerPos.
    u dur: dur.
    u pan: pan.
    u amp: amp.
    u interp: interp.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#numChan:trigger:bufnum:rate:centerPos:dur:pan:amp:interp:!
!

UGen subclass: #Timer
  instanceVariableNames: 'trig'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Timer comment: 'Returns time since last triggered.'!

!Timer methodsFor: 'accessing'!
  trig ^trig!
  trig: x trig := x!
!

!Timer methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Timer.
    paramNames := #(#trig).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    trig := 0.0!
!


!Timer class methodsFor: 'instance creation'!
  trig: trig
    |u|
    u := self new.
    u trig: trig.
    ^u!
  trig: trig mul: mul
    |u|
    u := self new.
    u trig: trig.
    u mul: mul.
    ^u!
  trig: trig mul: mul add: add
    |u|
    u := self new.
    u trig: trig.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#trig:!
!

UGen subclass: #TIRand
  instanceVariableNames: 'lo hi trig'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

TIRand comment: 'Triggered integer random number generator.'!

!TIRand methodsFor: 'accessing'!
  lo ^lo!
  lo: x lo := x!
  hi ^hi!
  hi: x hi := x!
  trig ^trig!
  trig: x trig := x!
!

!TIRand methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #TIRand.
    paramNames := #(#lo #hi #trig).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    lo := 0.0. hi := 127.0. trig := 0.0!
!


!TIRand class methodsFor: 'instance creation'!
  lo: lo hi: hi trig: trig
    |u|
    u := self new.
    u lo: lo.
    u hi: hi.
    u trig: trig.
    ^u!
  lo: lo hi: hi trig: trig mul: mul
    |u|
    u := self new.
    u lo: lo.
    u hi: hi.
    u trig: trig.
    u mul: mul.
    ^u!
  lo: lo hi: hi trig: trig mul: mul add: add
    |u|
    u := self new.
    u lo: lo.
    u hi: hi.
    u trig: trig.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#lo:hi:trig:!
!

UGen subclass: #ToggleFF
  instanceVariableNames: 'trig'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

ToggleFF comment: 'Toggle flip flop.'!

!ToggleFF methodsFor: 'accessing'!
  trig ^trig!
  trig: x trig := x!
!

!ToggleFF methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #ToggleFF.
    paramNames := #(#trig).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    trig := 0.0!
!


!ToggleFF class methodsFor: 'instance creation'!
  trig: trig
    |u|
    u := self new.
    u trig: trig.
    ^u!
  trig: trig mul: mul
    |u|
    u := self new.
    u trig: trig.
    u mul: mul.
    ^u!
  trig: trig mul: mul add: add
    |u|
    u := self new.
    u trig: trig.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#trig:!
!

UGen subclass: #TRand
  instanceVariableNames: 'lo hi trig'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

TRand comment: 'Triggered random number generator.'!

!TRand methodsFor: 'accessing'!
  lo ^lo!
  lo: x lo := x!
  hi ^hi!
  hi: x hi := x!
  trig ^trig!
  trig: x trig := x!
!

!TRand methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #TRand.
    paramNames := #(#lo #hi #trig).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    lo := 0.0. hi := 1.0. trig := 0.0!
!


!TRand class methodsFor: 'instance creation'!
  lo: lo hi: hi trig: trig
    |u|
    u := self new.
    u lo: lo.
    u hi: hi.
    u trig: trig.
    ^u!
  lo: lo hi: hi trig: trig mul: mul
    |u|
    u := self new.
    u lo: lo.
    u hi: hi.
    u trig: trig.
    u mul: mul.
    ^u!
  lo: lo hi: hi trig: trig mul: mul add: add
    |u|
    u := self new.
    u lo: lo.
    u hi: hi.
    u trig: trig.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#lo:hi:trig:!
!

UGen subclass: #Trig
  instanceVariableNames: 'in dur'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Trig comment: 'Timed trigger.'!

!Trig methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  dur ^dur!
  dur: x dur := x!
!

!Trig methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Trig.
    paramNames := #(#in #dur).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. dur := 0.1!
!


!Trig class methodsFor: 'instance creation'!
  in: in dur: dur
    |u|
    u := self new.
    u in: in.
    u dur: dur.
    ^u!
  in: in dur: dur mul: mul
    |u|
    u := self new.
    u in: in.
    u dur: dur.
    u mul: mul.
    ^u!
  in: in dur: dur mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u dur: dur.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:dur:!
!

UGen subclass: #Trig1
  instanceVariableNames: 'in dur'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Trig1 comment: 'Timed trigger.'!

!Trig1 methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  dur ^dur!
  dur: x dur := x!
!

!Trig1 methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Trig1.
    paramNames := #(#in #dur).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. dur := 0.1!
!


!Trig1 class methodsFor: 'instance creation'!
  in: in dur: dur
    |u|
    u := self new.
    u in: in.
    u dur: dur.
    ^u!
  in: in dur: dur mul: mul
    |u|
    u := self new.
    u in: in.
    u dur: dur.
    u mul: mul.
    ^u!
  in: in dur: dur mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u dur: dur.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:dur:!
!

UGen subclass: #VarSaw
  instanceVariableNames: 'freq iphase width'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

VarSaw comment: 'Variable duty saw'!

!VarSaw methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
  iphase ^iphase!
  iphase: x iphase := x!
  width ^width!
  width: x width := x!
!

!VarSaw methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #VarSaw.
    paramNames := #(#freq #iphase #width).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 440.0. iphase := 0.0. width := 0.5!
!


!VarSaw class methodsFor: 'instance creation'!
  freq: freq iphase: iphase width: width
    |u|
    u := self new.
    u freq: freq.
    u iphase: iphase.
    u width: width.
    ^u!
  freq: freq iphase: iphase width: width mul: mul
    |u|
    u := self new.
    u freq: freq.
    u iphase: iphase.
    u width: width.
    u mul: mul.
    ^u!
  freq: freq iphase: iphase width: width mul: mul add: add
    |u|
    u := self new.
    u freq: freq.
    u iphase: iphase.
    u width: width.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#freq:iphase:width:!
!

UGen subclass: #Vibrato
  instanceVariableNames: 'freq rate depth delay onset rateVariation depthVariation iphase trig'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Vibrato comment: 'The Vibrato oscillator models a slow frequency modulation.'!

!Vibrato methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
  rate ^rate!
  rate: x rate := x!
  depth ^depth!
  depth: x depth := x!
  delay ^delay!
  delay: x delay := x!
  onset ^onset!
  onset: x onset := x!
  rateVariation ^rateVariation!
  rateVariation: x rateVariation := x!
  depthVariation ^depthVariation!
  depthVariation: x depthVariation := x!
  iphase ^iphase!
  iphase: x iphase := x!
  trig ^trig!
  trig: x trig := x!
!

!Vibrato methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Vibrato.
    paramNames := #(#freq #rate #depth #delay #onset #rateVariation #depthVariation #iphase #trig).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 440.0. rate := 6.0. depth := 0.02. delay := 0.0. onset := 0.0. rateVariation := 0.04. depthVariation := 0.1. iphase := 0.0. trig := 0.0!
!


!Vibrato class methodsFor: 'instance creation'!
  freq: freq rate: rate depth: depth delay: delay onset: onset rateVariation: rateVariation depthVariation: depthVariation iphase: iphase trig: trig
    |u|
    u := self new.
    u freq: freq.
    u rate: rate.
    u depth: depth.
    u delay: delay.
    u onset: onset.
    u rateVariation: rateVariation.
    u depthVariation: depthVariation.
    u iphase: iphase.
    u trig: trig.
    ^u!
  freq: freq rate: rate depth: depth delay: delay onset: onset rateVariation: rateVariation depthVariation: depthVariation iphase: iphase trig: trig mul: mul
    |u|
    u := self new.
    u freq: freq.
    u rate: rate.
    u depth: depth.
    u delay: delay.
    u onset: onset.
    u rateVariation: rateVariation.
    u depthVariation: depthVariation.
    u iphase: iphase.
    u trig: trig.
    u mul: mul.
    ^u!
  freq: freq rate: rate depth: depth delay: delay onset: onset rateVariation: rateVariation depthVariation: depthVariation iphase: iphase trig: trig mul: mul add: add
    |u|
    u := self new.
    u freq: freq.
    u rate: rate.
    u depth: depth.
    u delay: delay.
    u onset: onset.
    u rateVariation: rateVariation.
    u depthVariation: depthVariation.
    u iphase: iphase.
    u trig: trig.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#freq:rate:depth:delay:onset:rateVariation:depthVariation:iphase:trig:!
!

UGen subclass: #WhiteNoise
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

WhiteNoise comment: 'White noise.'!


!WhiteNoise methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #WhiteNoise.
    paramNames := #().
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    !
!


!WhiteNoise class methodsFor: 'instance creation'!
  mul: mul
    |u|
    u := self new.
    u mul: mul.
    ^u!
  mul: mul add: add
    |u|
    u := self new.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#new!
!

UGen subclass: #Wrap
  instanceVariableNames: 'in lo hi'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Wrap comment: 'Wrap a signal outside given thresholds.'!

!Wrap methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  lo ^lo!
  lo: x lo := x!
  hi ^hi!
  hi: x hi := x!
!

!Wrap methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Wrap.
    paramNames := #(#in #lo #hi).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. lo := 0.0. hi := 1.0!
!


!Wrap class methodsFor: 'instance creation'!
  in: in lo: lo hi: hi
    |u|
    u := self new.
    u in: in.
    u lo: lo.
    u hi: hi.
    ^u!
  in: in lo: lo hi: hi mul: mul
    |u|
    u := self new.
    u in: in.
    u lo: lo.
    u hi: hi.
    u mul: mul.
    ^u!
  in: in lo: lo hi: hi mul: mul add: add
    |u|
    u := self new.
    u in: in.
    u lo: lo.
    u hi: hi.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:lo:hi:!
!

UGen subclass: #XFade2
  instanceVariableNames: 'inA inB pan level'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

XFade2 comment: 'Equal power two channel cross fade.'!

!XFade2 methodsFor: 'accessing'!
  inA ^inA!
  inA: x inA := x!
  inB ^inB!
  inB: x inB := x!
  pan ^pan!
  pan: x pan := x!
  level ^level!
  level: x level := x!
!

!XFade2 methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #XFade2.
    paramNames := #(#inA #inB #pan #level).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    inA := 0.0. inB := 0.0. pan := 0.0. level := 1.0!
!


!XFade2 class methodsFor: 'instance creation'!
  inA: inA inB: inB pan: pan level: level
    |u|
    u := self new.
    u inA: inA.
    u inB: inB.
    u pan: pan.
    u level: level.
    ^u!
  inA: inA inB: inB pan: pan level: level mul: mul
    |u|
    u := self new.
    u inA: inA.
    u inB: inB.
    u pan: pan.
    u level: level.
    u mul: mul.
    ^u!
  inA: inA inB: inB pan: pan level: level mul: mul add: add
    |u|
    u := self new.
    u inA: inA.
    u inB: inB.
    u pan: pan.
    u level: level.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#inA:inB:pan:level:!
!

UGen subclass: #XLine
  instanceVariableNames: 'start end dur doneAction'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

XLine comment: 'Exponential line generator.'!

!XLine methodsFor: 'accessing'!
  start ^start!
  start: x start := x!
  end ^end!
  end: x end := x!
  dur ^dur!
  dur: x dur := x!
  doneAction ^doneAction!
  doneAction: x doneAction := x!
!

!XLine methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #XLine.
    paramNames := #(#start #end #dur #doneAction).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    start := 1.0. end := 2.0. dur := 1.0. doneAction := 0.0!
!


!XLine class methodsFor: 'instance creation'!
  start: start end: end dur: dur doneAction: doneAction
    |u|
    u := self new.
    u start: start.
    u end: end.
    u dur: dur.
    u doneAction: doneAction.
    ^u!
  start: start end: end dur: dur doneAction: doneAction mul: mul
    |u|
    u := self new.
    u start: start.
    u end: end.
    u dur: dur.
    u doneAction: doneAction.
    u mul: mul.
    ^u!
  start: start end: end dur: dur doneAction: doneAction mul: mul add: add
    |u|
    u := self new.
    u start: start.
    u end: end.
    u dur: dur.
    u doneAction: doneAction.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#start:end:dur:doneAction:!
!


!AbstractUGen methodsFor: 'arithmetic'!
  + b ^ BinaryOpUGen specialIndex: 0 a: self b: b!
  - b ^ BinaryOpUGen specialIndex: 1 a: self b: b!
  * b ^ BinaryOpUGen specialIndex: 2 a: self b: b!
  / b ^ BinaryOpUGen specialIndex: 4 a: self b: b!
  % b ^ BinaryOpUGen specialIndex: 5 a: self b: b!
  /= b ^ BinaryOpUGen specialIndex: 7 a: self b: b!
  < b ^ BinaryOpUGen specialIndex: 8 a: self b: b!
  > b ^ BinaryOpUGen specialIndex: 9 a: self b: b!
  <= b ^ BinaryOpUGen specialIndex: 10 a: self b: b!
  >= b ^ BinaryOpUGen specialIndex: 11 a: self b: b!
  min: b ^ BinaryOpUGen specialIndex: 12 a: self b: b!
  max: b ^ BinaryOpUGen specialIndex: 13 a: self b: b!
  bitAnd: b ^ BinaryOpUGen specialIndex: 14 a: self b: b!
  bitOr: b ^ BinaryOpUGen specialIndex: 15 a: self b: b!
  lcm: b ^ BinaryOpUGen specialIndex: 17 a: self b: b!
  gcd: b ^ BinaryOpUGen specialIndex: 18 a: self b: b!
  round: b ^ BinaryOpUGen specialIndex: 19 a: self b: b!
  truncateTo: b ^ BinaryOpUGen specialIndex: 21 a: self b: b!
  raisedTo: b ^ BinaryOpUGen specialIndex: 25 a: self b: b!
  bitShiftLeft: b ^ BinaryOpUGen specialIndex: 26 a: self b: b!
  bitShiftRight: b ^ BinaryOpUGen specialIndex: 27 a: self b: b!
  amclip: b ^ BinaryOpUGen specialIndex: 40 a: self b: b!
  clip2: b ^ BinaryOpUGen specialIndex: 42 a: self b: b!
!

!AbstractUGen methodsFor: 'arithmetic'!
  negated ^ UnaryOpUGen specialIndex: 0 a: self!
  abs ^ UnaryOpUGen specialIndex: 5 a: self!
  ceil ^ UnaryOpUGen specialIndex: 8 a: self!
  floor ^ UnaryOpUGen specialIndex: 9 a: self!
  frac ^ UnaryOpUGen specialIndex: 10 a: self!
  squared ^ UnaryOpUGen specialIndex: 12 a: self!
  cubed ^ UnaryOpUGen specialIndex: 13 a: self!
  sqrt ^ UnaryOpUGen specialIndex: 14 a: self!
  exp ^ UnaryOpUGen specialIndex: 15 a: self!
  reciprocal ^ UnaryOpUGen specialIndex: 16 a: self!
  midicps ^ UnaryOpUGen specialIndex: 17 a: self!
  ln ^ UnaryOpUGen specialIndex: 25 a: self!
  log2 ^ UnaryOpUGen specialIndex: 26 a: self!
  log ^ UnaryOpUGen specialIndex: 27 a: self!
  sin ^ UnaryOpUGen specialIndex: 28 a: self!
  cos ^ UnaryOpUGen specialIndex: 29 a: self!
  tanh ^ UnaryOpUGen specialIndex: 36 a: self!
  distort ^ UnaryOpUGen specialIndex: 42 a: self!
  softclip ^ UnaryOpUGen specialIndex: 43 a: self!
!

