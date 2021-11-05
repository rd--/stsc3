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
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul ^(AllpassC in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add ^(AllpassC in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul + add!
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
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul ^(AllpassL in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add ^(AllpassL in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul + add!
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
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul ^(AllpassN in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add ^(AllpassN in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul + add!
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
  freq: freq root: root exp: exp mul: mul ^(AmpComp freq: freq root: root exp: exp) * mul!
  freq: freq root: root exp: exp mul: mul add: add ^(AmpComp freq: freq root: root exp: exp) * mul + add!
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
  freq: freq root: root minAmp: minAmp rootAmp: rootAmp mul: mul ^(AmpCompA freq: freq root: root minAmp: minAmp rootAmp: rootAmp) * mul!
  freq: freq root: root minAmp: minAmp rootAmp: rootAmp mul: mul add: add ^(AmpCompA freq: freq root: root minAmp: minAmp rootAmp: rootAmp) * mul + add!
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
  in: in attackTime: attackTime releaseTime: releaseTime mul: mul ^(Amplitude in: in attackTime: attackTime releaseTime: releaseTime) * mul!
  in: in attackTime: attackTime releaseTime: releaseTime mul: mul add: add ^(Amplitude in: in attackTime: attackTime releaseTime: releaseTime) * mul + add!
  primaryFactoryMethod ^#in:attackTime:releaseTime:!
!

UGen subclass: #AnalogFoldOsc
  instanceVariableNames: 'freq amp'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

AnalogFoldOsc comment: '(Undocumented class)'!

!AnalogFoldOsc methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
  amp ^amp!
  amp: x amp := x!
!

!AnalogFoldOsc methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #AnalogFoldOsc.
    paramNames := #(#freq #amp).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 100.0. amp := 1.0!
!


!AnalogFoldOsc class methodsFor: 'instance creation'!
  freq: freq amp: amp
    |u|
    u := self new.
    u freq: freq.
    u amp: amp.
    ^u!
  freq: freq amp: amp mul: mul ^(AnalogFoldOsc freq: freq amp: amp) * mul!
  freq: freq amp: amp mul: mul add: add ^(AnalogFoldOsc freq: freq amp: amp) * mul + add!
  primaryFactoryMethod ^#freq:amp:!
!

UGen subclass: #Balance2
  instanceVariableNames: 'left right pos level'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Balance2 comment: 'Stereo signal balancer'!

!Balance2 methodsFor: 'accessing'!
  left ^left!
  left: x left := x!
  right ^right!
  right: x right := x!
  pos ^pos!
  pos: x pos := x!
  level ^level!
  level: x level := x!
!

!Balance2 methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Balance2.
    paramNames := #(#left #right #pos #level).
    mceNames := #().
    ugenRate := #ar.
    numChan := 2.
    specialIndex := 0.
    ugenId := nil.
    left := 0.0. right := 0.0. pos := 0.0. level := 1.0!
!


!Balance2 class methodsFor: 'instance creation'!
  left: left right: right pos: pos level: level
    |u|
    u := self new.
    u left: left.
    u right: right.
    u pos: pos.
    u level: level.
    ^u!
  left: left right: right pos: pos level: level mul: mul ^(Balance2 left: left right: right pos: pos level: level) * mul!
  left: left right: right pos: pos level: level mul: mul add: add ^(Balance2 left: left right: right pos: pos level: level) * mul + add!
  primaryFactoryMethod ^#left:right:pos:level:!
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
  in: in freq: freq bw: bw mul: mul ^(BBandPass in: in freq: freq bw: bw) * mul!
  in: in freq: freq bw: bw mul: mul add: add ^(BBandPass in: in freq: freq bw: bw) * mul + add!
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
  in: in freq: freq bw: bw mul: mul ^(BBandStop in: in freq: freq bw: bw) * mul!
  in: in freq: freq bw: bw mul: mul add: add ^(BBandStop in: in freq: freq bw: bw) * mul + add!
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
  freq: freq numharm: numharm mul: mul ^(Blip freq: freq numharm: numharm) * mul!
  freq: freq numharm: numharm mul: mul add: add ^(Blip freq: freq numharm: numharm) * mul + add!
  primaryFactoryMethod ^#freq:numharm:!
!

UGen subclass: #BlockSize
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

BlockSize comment: '(Undocumented class)'!


!BlockSize methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #BlockSize.
    paramNames := #().
    mceNames := #().
    ugenRate := #ir.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    !
!


!BlockSize class methodsFor: 'instance creation'!
  mul: mul ^(BlockSize new) * mul!
  mul: mul add: add ^(BlockSize new) * mul + add!
  primaryFactoryMethod ^#new!
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
  in: in freq: freq rq: rq mul: mul ^(BLowPass in: in freq: freq rq: rq) * mul!
  in: in freq: freq rq: rq mul: mul add: add ^(BLowPass in: in freq: freq rq: rq) * mul + add!
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
  in: in freq: freq rq: rq mul: mul ^(BPF in: in freq: freq rq: rq) * mul!
  in: in freq: freq rq: rq mul: mul add: add ^(BPF in: in freq: freq rq: rq) * mul + add!
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
  in: in mul: mul ^(BPZ2 in: in) * mul!
  in: in mul: mul add: add ^(BPZ2 in: in) * mul + add!
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
  in: in freq: freq rq: rq mul: mul ^(BRF in: in freq: freq rq: rq) * mul!
  in: in freq: freq rq: rq mul: mul add: add ^(BRF in: in freq: freq rq: rq) * mul + add!
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
  mul: mul ^(BrownNoise new) * mul!
  mul: mul add: add ^(BrownNoise new) * mul + add!
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
  numChan: numChan bufnum: bufnum phase: phase loop: loop interpolation: interpolation mul: mul ^(BufRd numChan: numChan bufnum: bufnum phase: phase loop: loop interpolation: interpolation) * mul!
  numChan: numChan bufnum: bufnum phase: phase loop: loop interpolation: interpolation mul: mul add: add ^(BufRd numChan: numChan bufnum: bufnum phase: phase loop: loop interpolation: interpolation) * mul + add!
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
    u inputArray: inputArr