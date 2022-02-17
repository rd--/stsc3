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
    ugenRate := #(0).
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
    ugenRate := #(0).
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
    ugenRate := #(0).
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
    ugenRate := #(0 1).
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
    ugenRate := #(0).
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
    ugenRate := #(0).
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
    ugenRate := #(0).
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
    ugenRate := #(0).
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
    ugenRate := #(0).
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
    ugenRate := #(0).
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

UGen subclass: #BufDur
  instanceVariableNames: 'bufnum'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

BufDur comment: 'Current duration of soundfile in buffer.'!

!BufDur methodsFor: 'accessing'!
  bufnum ^bufnum!
  bufnum: x bufnum := x!
!

!BufDur methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #BufDur.
    paramNames := #(#bufnum).
    mceNames := #().
    ugenRate := #kr.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    bufnum := 0.0!
!


!BufDur class methodsFor: 'instance creation'!
  bufnum: bufnum
    |u|
    u := self new.
    u bufnum: bufnum.
    ^u!
  bufnum: bufnum mul: mul ^(BufDur bufnum: bufnum) * mul!
  bufnum: bufnum mul: mul add: add ^(BufDur bufnum: bufnum) * mul + add!
  primaryFactoryMethod ^#bufnum:!
!

UGen subclass: #BufFrames
  instanceVariableNames: 'bufnum'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

BufFrames comment: 'Current number of frames allocated in the buffer.'!

!BufFrames methodsFor: 'accessing'!
  bufnum ^bufnum!
  bufnum: x bufnum := x!
!

!BufFrames methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #BufFrames.
    paramNames := #(#bufnum).
    mceNames := #().
    ugenRate := #kr.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    bufnum := 0.0!
!


!BufFrames class methodsFor: 'instance creation'!
  bufnum: bufnum
    |u|
    u := self new.
    u bufnum: bufnum.
    ^u!
  bufnum: bufnum mul: mul ^(BufFrames bufnum: bufnum) * mul!
  bufnum: bufnum mul: mul add: add ^(BufFrames bufnum: bufnum) * mul + add!
  primaryFactoryMethod ^#bufnum:!
!

UGen subclass: #BufRateScale
  instanceVariableNames: 'bufnum'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

BufRateScale comment: 'Buffer rate scaling in respect to server samplerate.'!

!BufRateScale methodsFor: 'accessing'!
  bufnum ^bufnum!
  bufnum: x bufnum := x!
!

!BufRateScale methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #BufRateScale.
    paramNames := #(#bufnum).
    mceNames := #().
    ugenRate := #kr.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    bufnum := 0.0!
!


!BufRateScale class methodsFor: 'instance creation'!
  bufnum: bufnum
    |u|
    u := self new.
    u bufnum: bufnum.
    ^u!
  bufnum: bufnum mul: mul ^(BufRateScale bufnum: bufnum) * mul!
  bufnum: bufnum mul: mul add: add ^(BufRateScale bufnum: bufnum) * mul + add!
  primaryFactoryMethod ^#bufnum:!
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
    ugenRate := #(3).
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
  bufnum: bufnum phase: phase loop: loop inputArray: inputArray mul: mul ^(BufWr bufnum: bufnum phase: phase loop: loop inputArray: inputArray) * mul!
  bufnum: bufnum phase: phase loop: loop inputArray: inputArray mul: mul add: add ^(BufWr bufnum: bufnum phase: phase loop: loop inputArray: inputArray) * mul + add!
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
  buf: buf mul: mul ^(ClearBuf buf: buf) * mul!
  buf: buf mul: mul add: add ^(ClearBuf buf: buf) * mul + add!
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
    ugenRate := #(0).
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
  in: in lo: lo hi: hi mul: mul ^(Clip in: in lo: lo hi: hi) * mul!
  in: in lo: lo hi: hi mul: mul add: add ^(Clip in: in lo: lo hi: hi) * mul + add!
  primaryFactoryMethod ^#in:lo:hi:!
!

UGen subclass: #CoinGate
  instanceVariableNames: 'prob in'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

CoinGate comment: 'Statistical gate.'!

!CoinGate methodsFor: 'accessing'!
  prob ^prob!
  prob: x prob := x!
  in ^in!
  in: x in := x!
!

!CoinGate methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #CoinGate.
    paramNames := #(#prob #in).
    mceNames := #().
    ugenRate := #(1).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    prob := 0.0. in := 0.0!
!


!CoinGate class methodsFor: 'instance creation'!
  prob: prob in: in
    |u|
    u := self new.
    u prob: prob.
    u in: in.
    ^u!
  prob: prob in: in mul: mul ^(CoinGate prob: prob in: in) * mul!
  prob: prob in: in mul: mul add: add ^(CoinGate prob: prob in: in) * mul + add!
  primaryFactoryMethod ^#prob:in:!
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
    ugenRate := #(0).
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
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul ^(CombC in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add ^(CombC in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul + add!
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
    ugenRate := #(0).
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
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul ^(CombL in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add ^(CombL in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul + add!
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
    ugenRate := #(0).
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
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul ^(CombN in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add ^(CombN in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul + add!
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
  mul: mul ^(ControlDur new) * mul!
  mul: mul add: add ^(ControlDur new) * mul + add!
  primaryFactoryMethod ^#new!
!

UGen subclass: #ControlRate
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

ControlRate comment: 'Server control rate.'!


!ControlRate methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #ControlRate.
    paramNames := #().
    mceNames := #().
    ugenRate := #ir.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    !
!


!ControlRate class methodsFor: 'instance creation'!
  mul: mul ^(ControlRate new) * mul!
  mul: mul add: add ^(ControlRate new) * mul + add!
  primaryFactoryMethod ^#new!
!

UGen subclass: #Convolution
  instanceVariableNames: 'in kernel framesize'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Convolution comment: 'Real-time convolver.'!

!Convolution methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  kernel ^kernel!
  kernel: x kernel := x!
  framesize ^framesize!
  framesize: x framesize := x!
!

!Convolution methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Convolution.
    paramNames := #(#in #kernel #framesize).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. kernel := 0.0. framesize := 512.0!
!


!Convolution class methodsFor: 'instance creation'!
  in: in kernel: kernel framesize: framesize
    |u|
    u := self new.
    u in: in.
    u kernel: kernel.
    u framesize: framesize.
    ^u!
  in: in kernel: kernel framesize: framesize mul: mul ^(Convolution in: in kernel: kernel framesize: framesize) * mul!
  in: in kernel: kernel framesize: framesize mul: mul add: add ^(Convolution in: in kernel: kernel framesize: framesize) * mul + add!
  primaryFactoryMethod ^#in:kernel:framesize:!
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
  chaosParam: chaosParam mul: mul ^(Crackle chaosParam: chaosParam) * mul!
  chaosParam: chaosParam mul: mul add: add ^(Crackle chaosParam: chaosParam) * mul + add!
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
  freq: freq a: a b: b xi: xi mul: mul ^(CuspL freq: freq a: a b: b xi: xi) * mul!
  freq: freq a: a b: b xi: xi mul: mul add: add ^(CuspL freq: freq a: a b: b xi: xi) * mul + add!
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
  bufnum: bufnum phase: phase loop: loop mul: mul ^(Dbufrd bufnum: bufnum phase: phase loop: loop) * mul!
  bufnum: bufnum phase: phase loop: loop mul: mul add: add ^(Dbufrd bufnum: bufnum phase: phase loop: loop) * mul + add!
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
  bufnum: bufnum phase: phase input: input loop: loop mul: mul ^(Dbufwr bufnum: bufnum phase: phase input: input loop: loop) * mul!
  bufnum: bufnum phase: phase input: input loop: loop mul: mul add: add ^(Dbufwr bufnum: bufnum phase: phase input: input loop: loop) * mul + add!
  primaryFactoryMethod ^#bufnum:phase:input:loop:!
!

UGen subclass: #DC
  instanceVariableNames: 'in'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

DC comment: 'Create a constant amplitude signal'!

!DC methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
!

!DC methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #DC.
    paramNames := #(#in).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0!
!


!DC class methodsFor: 'instance creation'!
  in: in
    |u|
    u := self new.
    u in: in.
    ^u!
  in: in mul: mul ^(DC in: in) * mul!
  in: in mul: mul add: add ^(DC in: in) * mul + add!
  primaryFactoryMethod ^#in:!
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
    ugenRate := #(0).
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
  in: in decayTime: decayTime mul: mul ^(Decay in: in decayTime: decayTime) * mul!
  in: in decayTime: decayTime mul: mul add: add ^(Decay in: in decayTime: decayTime) * mul + add!
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
    ugenRate := #(0).
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
  in: in attackTime: attackTime decayTime: decayTime mul: mul ^(Decay2 in: in attackTime: attackTime decayTime: decayTime) * mul!
  in: in attackTime: attackTime decayTime: decayTime mul: mul add: add ^(Decay2 in: in attackTime: attackTime decayTime: decayTime) * mul + add!
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
    ugenRate := #(1).
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
  bufnum: bufnum in: in octave: octave mul: mul ^(DegreeToKey bufnum: bufnum in: in octave: octave) * mul!
  bufnum: bufnum in: in octave: octave mul: mul add: add ^(DegreeToKey bufnum: bufnum in: in octave: octave) * mul + add!
  primaryFactoryMethod ^#bufnum:in:octave:!
!

UGen subclass: #DelayC
  instanceVariableNames: 'in maxdelaytime delaytime'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

DelayC comment: 'Simple delay line with cubic interpolation.'!

!DelayC methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  maxdelaytime ^maxdelaytime!
  maxdelaytime: x maxdelaytime := x!
  delaytime ^delaytime!
  delaytime: x delaytime := x!
!

!DelayC methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #DelayC.
    paramNames := #(#in #maxdelaytime #delaytime).
    mceNames := #().
    ugenRate := #(0).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. maxdelaytime := 0.2. delaytime := 0.2!
!


!DelayC class methodsFor: 'instance creation'!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime
    |u|
    u := self new.
    u in: in.
    u maxdelaytime: maxdelaytime.
    u delaytime: delaytime.
    ^u!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime mul: mul ^(DelayC in: in maxdelaytime: maxdelaytime delaytime: delaytime) * mul!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime mul: mul add: add ^(DelayC in: in maxdelaytime: maxdelaytime delaytime: delaytime) * mul + add!
  primaryFactoryMethod ^#in:maxdelaytime:delaytime:!
!

UGen subclass: #DelayL
  instanceVariableNames: 'in maxdelaytime delaytime'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

DelayL comment: 'Simple delay line with linear interpolation.'!

!DelayL methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  maxdelaytime ^maxdelaytime!
  maxdelaytime: x maxdelaytime := x!
  delaytime ^delaytime!
  delaytime: x delaytime := x!
!

!DelayL methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #DelayL.
    paramNames := #(#in #maxdelaytime #delaytime).
    mceNames := #().
    ugenRate := #(0).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. maxdelaytime := 0.2. delaytime := 0.2!
!


!DelayL class methodsFor: 'instance creation'!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime
    |u|
    u := self new.
    u in: in.
    u maxdelaytime: maxdelaytime.
    u delaytime: delaytime.
    ^u!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime mul: mul ^(DelayL in: in maxdelaytime: maxdelaytime delaytime: delaytime) * mul!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime mul: mul add: add ^(DelayL in: in maxdelaytime: maxdelaytime delaytime: delaytime) * mul + add!
  primaryFactoryMethod ^#in:maxdelaytime:delaytime:!
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
    ugenRate := #(0).
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
  in: in maxdelaytime: maxdelaytime delaytime: delaytime mul: mul ^(DelayN in: in maxdelaytime: maxdelaytime delaytime: delaytime) * mul!
  in: in maxdelaytime: maxdelaytime delaytime: delaytime mul: mul add: add ^(DelayN in: in maxdelaytime: maxdelaytime delaytime: delaytime) * mul + add!
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
    ugenRate := #(0).
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
  trig: trig reset: reset demandUGens: demandUGens mul: mul ^(Demand trig: trig reset: reset demandUGens: demandUGens) * mul!
  trig: trig reset: reset demandUGens: demandUGens mul: mul add: add ^(Demand trig: trig reset: reset demandUGens: demandUGens) * mul + add!
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
    ugenRate := #(0).
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
  in: in amp: amp time: time doneAction: doneAction mul: mul ^(DetectSilence in: in amp: amp time: time doneAction: doneAction) * mul!
  in: in amp: amp time: time doneAction: doneAction mul: mul add: add ^(DetectSilence in: in amp: amp time: time doneAction: doneAction) * mul + add!
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
  length: length lo: lo hi: hi mul: mul ^(Diwhite length: length lo: lo hi: hi) * mul!
  length: length lo: lo hi: hi mul: mul add: add ^(Diwhite length: length lo: lo hi: hi) * mul + add!
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
  repeats: repeats list: list mul: mul ^(Drand repeats: repeats list: list) * mul!
  repeats: repeats list: list mul: mul add: add ^(Drand repeats: repeats list: list) * mul + add!
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
  repeats: repeats list: list mul: mul ^(Dseq repeats: repeats list: list) * mul!
  repeats: repeats list: list mul: mul add: add ^(Dseq repeats: repeats list: list) * mul + add!
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
  length: length start: start step: step mul: mul ^(Dseries length: length start: start step: step) * mul!
  length: length start: start step: step mul: mul add: add ^(Dseries length: length start: start step: step) * mul + add!
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
  repeats: repeats list: list mul: mul ^(Dshuf repeats: repeats list: list) * mul!
  repeats: repeats list: list mul: mul add: add ^(Dshuf repeats: repeats list: list) * mul + add!
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
  density: density mul: mul ^(Dust density: density) * mul!
  density: density mul: mul add: add ^(Dust density: density) * mul + add!
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
  density: density mul: mul ^(Dust2 density: density) * mul!
  density: density mul: mul add: add ^(Dust2 density: density) * mul + add!
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
  dur: dur reset: reset doneAction: doneAction level: level mul: mul ^(Duty dur: dur reset: reset doneAction: doneAction level: level) * mul!
  dur: dur reset: reset doneAction: doneAction level: level mul: mul add: add ^(Duty dur: dur reset: reset doneAction: doneAction level: level) * mul + add!
  primaryFactoryMethod ^#dur:reset:doneAction:level:!
!

UGen subclass: #DWGPluckedStiff
  instanceVariableNames: 'freq amp gate pos c1 c3 inp release fB'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

DWGPluckedStiff comment: 'Plucked physical model.'!

!DWGPluckedStiff methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
  amp ^amp!
  amp: x amp := x!
  gate ^gate!
  gate: x gate := x!
  pos ^pos!
  pos: x pos := x!
  c1 ^c1!
  c1: x c1 := x!
  c3 ^c3!
  c3: x c3 := x!
  inp ^inp!
  inp: x inp := x!
  release ^release!
  release: x release := x!
  fB ^fB!
  fB: x fB := x!
!

!DWGPluckedStiff methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #DWGPluckedStiff.
    paramNames := #(#freq #amp #gate #pos #c1 #c3 #inp #release #fB).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 440.0. amp := 0.5. gate := 1.0. pos := 0.14. c1 := 1.0. c3 := 30.0. inp := 0.0. release := 0.1. fB := 2.0!
!


!DWGPluckedStiff class methodsFor: 'instance creation'!
  freq: freq amp: amp gate: gate pos: pos c1: c1 c3: c3 inp: inp release: release fB: fB
    |u|
    u := self new.
    u freq: freq.
    u amp: amp.
    u gate: gate.
    u pos: pos.
    u c1: c1.
    u c3: c3.
    u inp: inp.
    u release: release.
    u fB: fB.
    ^u!
  freq: freq amp: amp gate: gate pos: pos c1: c1 c3: c3 inp: inp release: release fB: fB mul: mul ^(DWGPluckedStiff freq: freq amp: amp gate: gate pos: pos c1: c1 c3: c3 inp: inp release: release fB: fB) * mul!
  freq: freq amp: amp gate: gate pos: pos c1: c1 c3: c3 inp: inp release: release fB: fB mul: mul add: add ^(DWGPluckedStiff freq: freq amp: amp gate: gate pos: pos c1: c1 c3: c3 inp: inp release: release fB: fB) * mul + add!
  primaryFactoryMethod ^#freq:amp:gate:pos:c1:c3:inp:release:fB:!
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
  gate: gate levelScale: levelScale levelBias: levelBias timeScale: timeScale doneAction: doneAction envelope: envelope mul: mul ^(EnvGen gate: gate levelScale: levelScale levelBias: levelBias timeScale: timeScale doneAction: doneAction envelope: envelope) * mul!
  gate: gate levelScale: levelScale levelBias: levelBias timeScale: timeScale doneAction: doneAction envelope: envelope mul: mul add: add ^(EnvGen gate: gate levelScale: levelScale levelBias: levelBias timeScale: timeScale doneAction: doneAction envelope: envelope) * mul + add!
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
  lo: lo hi: hi mul: mul ^(ExpRand lo: lo hi: hi) * mul!
  lo: lo hi: hi mul: mul add: add ^(ExpRand lo: lo hi: hi) * mul + add!
  primaryFactoryMethod ^#lo:hi:!
!

UGen subclass: #FBSineL
  instanceVariableNames: 'freq im fb a c xi yi'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

FBSineL comment: 'Feedback sine with chaotic phase indexing'!

!FBSineL methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
  im ^im!
  im: x im := x!
  fb ^fb!
  fb: x fb := x!
  a ^a!
  a: x a := x!
  c ^c!
  c: x c := x!
  xi ^xi!
  xi: x xi := x!
  yi ^yi!
  yi: x yi := x!
!

!FBSineL methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #FBSineL.
    paramNames := #(#freq #im #fb #a #c #xi #yi).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 22050.0. im := 1.0. fb := 0.1. a := 1.1. c := 0.5. xi := 0.1. yi := 0.1!
!


!FBSineL class methodsFor: 'instance creation'!
  freq: freq im: im fb: fb a: a c: c xi: xi yi: yi
    |u|
    u := self new.
    u freq: freq.
    u im: im.
    u fb: fb.
    u a: a.
    u c: c.
    u xi: xi.
    u yi: yi.
    ^u!
  freq: freq im: im fb: fb a: a c: c xi: xi yi: yi mul: mul ^(FBSineL freq: freq im: im fb: fb a: a c: c xi: xi yi: yi) * mul!
  freq: freq im: im fb: fb a: a c: c xi: xi yi: yi mul: mul add: add ^(FBSineL freq: freq im: im fb: fb a: a c: c xi: xi yi: yi) * mul + add!
  primaryFactoryMethod ^#freq:im:fb:a:c:xi:yi:!
!

UGen subclass: #FBSineC
  instanceVariableNames: 'freq im fb a c xi yi'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

FBSineC comment: 'Feedback sine with chaotic phase indexing'!

!FBSineC methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
  im ^im!
  im: x im := x!
  fb ^fb!
  fb: x fb := x!
  a ^a!
  a: x a := x!
  c ^c!
  c: x c := x!
  xi ^xi!
  xi: x xi := x!
  yi ^yi!
  yi: x yi := x!
!

!FBSineC methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #FBSineC.
    paramNames := #(#freq #im #fb #a #c #xi #yi).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 22050.0. im := 1.0. fb := 0.1. a := 1.1. c := 0.5. xi := 0.1. yi := 0.1!
!


!FBSineC class methodsFor: 'instance creation'!
  freq: freq im: im fb: fb a: a c: c xi: xi yi: yi
    |u|
    u := self new.
    u freq: freq.
    u im: im.
    u fb: fb.
    u a: a.
    u c: c.
    u xi: xi.
    u yi: yi.
    ^u!
  freq: freq im: im fb: fb a: a c: c xi: xi yi: yi mul: mul ^(FBSineC freq: freq im: im fb: fb a: a c: c xi: xi yi: yi) * mul!
  freq: freq im: im fb: fb a: a c: c xi: xi yi: yi mul: mul add: add ^(FBSineC freq: freq im: im fb: fb a: a c: c xi: xi yi: yi) * mul + add!
  primaryFactoryMethod ^#freq:im:fb:a:c:xi:yi:!
!

UGen subclass: #FFT
  instanceVariableNames: 'buffer in hop wintype active winsize'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

FFT comment: 'Fast Fourier Transform'!

!FFT methodsFor: 'accessing'!
  buffer ^buffer!
  buffer: x buffer := x!
  in ^in!
  in: x in := x!
  hop ^hop!
  hop: x hop := x!
  wintype ^wintype!
  wintype: x wintype := x!
  active ^active!
  active: x active := x!
  winsize ^winsize!
  winsize: x winsize := x!
!

!FFT methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #FFT.
    paramNames := #(#buffer #in #hop #wintype #active #winsize).
    mceNames := #().
    ugenRate := #kr.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    buffer := 0.0. in := 0.0. hop := 0.5. wintype := 0.0. active := 1.0. winsize := 0.0!
!


!FFT class methodsFor: 'instance creation'!
  buffer: buffer in: in hop: hop wintype: wintype active: active winsize: winsize
    |u|
    u := self new.
    u buffer: buffer.
    u in: in.
    u hop: hop.
    u wintype: wintype.
    u active: active.
    u winsize: winsize.
    ^u!
  buffer: buffer in: in hop: hop wintype: wintype active: active winsize: winsize mul: mul ^(FFT buffer: buffer in: in hop: hop wintype: wintype active: active winsize: winsize) * mul!
  buffer: buffer in: in hop: hop wintype: wintype active: active winsize: winsize mul: mul add: add ^(FFT buffer: buffer in: in hop: hop wintype: wintype active: active winsize: winsize) * mul + add!
  primaryFactoryMethod ^#buffer:in:hop:wintype:active:winsize:!
!

UGen subclass: #Fold
  instanceVariableNames: 'in lo hi'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Fold comment: 'Fold a signal outside given thresholds.'!

!Fold methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  lo ^lo!
  lo: x lo := x!
  hi ^hi!
  hi: x hi := x!
!

!Fold methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Fold.
    paramNames := #(#in #lo #hi).
    mceNames := #().
    ugenRate := #(0).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. lo := 0.0. hi := 1.0!
!


!Fold class methodsFor: 'instance creation'!
  in: in lo: lo hi: hi
    |u|
    u := self new.
    u in: in.
    u lo: lo.
    u hi: hi.
    ^u!
  in: in lo: lo hi: hi mul: mul ^(Fold in: in lo: lo hi: hi) * mul!
  in: in lo: lo hi: hi mul: mul add: add ^(Fold in: in lo: lo hi: hi) * mul + add!
  primaryFactoryMethod ^#in:lo:hi:!
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
  fundfreq: fundfreq formfreq: formfreq bwfreq: bwfreq mul: mul ^(Formant fundfreq: fundfreq formfreq: formfreq bwfreq: bwfreq) * mul!
  fundfreq: fundfreq formfreq: formfreq bwfreq: bwfreq mul: mul add: add ^(Formant fundfreq: fundfreq formfreq: formfreq bwfreq: bwfreq) * mul + add!
  primaryFactoryMethod ^#fundfreq:formfreq:bwfreq:!
!

UGen subclass: #Formlet
  instanceVariableNames: 'in freq attacktime decaytime'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Formlet comment: 'FOF-like filter.'!

!Formlet methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  freq ^freq!
  freq: x freq := x!
  attacktime ^attacktime!
  attacktime: x attacktime := x!
  decaytime ^decaytime!
  decaytime: x decaytime := x!
!

!Formlet methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Formlet.
    paramNames := #(#in #freq #attacktime #decaytime).
    mceNames := #().
    ugenRate := #(0).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. freq := 440.0. attacktime := 1.0. decaytime := 1.0!
!


!Formlet class methodsFor: 'instance creation'!
  in: in freq: freq attacktime: attacktime decaytime: decaytime
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u attacktime: attacktime.
    u decaytime: decaytime.
    ^u!
  in: in freq: freq attacktime: attacktime decaytime: decaytime mul: mul ^(Formlet in: in freq: freq attacktime: attacktime decaytime: decaytime) * mul!
  in: in freq: freq attacktime: attacktime decaytime: decaytime mul: mul add: add ^(Formlet in: in freq: freq attacktime: attacktime decaytime: decaytime) * mul + add!
  primaryFactoryMethod ^#in:freq:attacktime:decaytime:!
!

UGen subclass: #FreqShift
  instanceVariableNames: 'in freq phase'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

FreqShift comment: 'Frequency Shifter.'!

!FreqShift methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  freq ^freq!
  freq: x freq := x!
  phase ^phase!
  phase: x phase := x!
!

!FreqShift methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #FreqShift.
    paramNames := #(#in #freq #phase).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. freq := 0.0. phase := 0.0!
!


!FreqShift class methodsFor: 'instance creation'!
  in: in freq: freq phase: phase
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u phase: phase.
    ^u!
  in: in freq: freq phase: phase mul: mul ^(FreqShift in: in freq: freq phase: phase) * mul!
  in: in freq: freq phase: phase mul: mul add: add ^(FreqShift in: in freq: freq phase: phase) * mul + add!
  primaryFactoryMethod ^#in:freq:phase:!
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
  freq: freq iphase: iphase mul: mul ^(FSinOsc freq: freq iphase: iphase) * mul!
  freq: freq iphase: iphase mul: mul add: add ^(FSinOsc freq: freq iphase: iphase) * mul + add!
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
    ugenRate := #(0).
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
  in: in mix: mix room: room damp: damp mul: mul ^(FreeVerb in: in mix: mix room: room damp: damp) * mul!
  in: in mix: mix room: room damp: damp mul: mul add: add ^(FreeVerb in: in mix: mix room: room damp: damp) * mul + add!
  primaryFactoryMethod ^#in:mix:room:damp:!
!

UGen subclass: #FreeVerb2
  instanceVariableNames: 'in in2 mix room damp'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

FreeVerb2 comment: 'A two-channel reverb'!

!FreeVerb2 methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  in2 ^in2!
  in2: x in2 := x!
  mix ^mix!
  mix: x mix := x!
  room ^room!
  room: x room := x!
  damp ^damp!
  damp: x damp := x!
!

!FreeVerb2 methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #FreeVerb2.
    paramNames := #(#in #in2 #mix #room #damp).
    mceNames := #().
    ugenRate := #(0).
    numChan := 2.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. in2 := 0.0. mix := 0.33. room := 0.5. damp := 0.5!
!


!FreeVerb2 class methodsFor: 'instance creation'!
  in: in in2: in2 mix: mix room: room damp: damp
    |u|
    u := self new.
    u in: in.
    u in2: in2.
    u mix: mix.
    u room: room.
    u damp: damp.
    ^u!
  in: in in2: in2 mix: mix room: room damp: damp mul: mul ^(FreeVerb2 in: in in2: in2 mix: mix room: room damp: damp) * mul!
  in: in in2: in2 mix: mix room: room damp: damp mul: mul add: add ^(FreeVerb2 in: in in2: in2 mix: mix room: room damp: damp) * mul + add!
  primaryFactoryMethod ^#in:in2:mix:room:damp:!
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
  ampdist: ampdist durdist: durdist adparam: adparam ddparam: ddparam minfreq: minfreq maxfreq: maxfreq ampscale: ampscale durscale: durscale initCPs: initCPs knum: knum mul: mul ^(Gendy1 ampdist: ampdist durdist: durdist adparam: adparam ddparam: ddparam minfreq: minfreq maxfreq: maxfreq ampscale: ampscale durscale: durscale initCPs: initCPs knum: knum) * mul!
  ampdist: ampdist durdist: durdist adparam: adparam ddparam: ddparam minfreq: minfreq maxfreq: maxfreq ampscale: ampscale durscale: durscale initCPs: initCPs knum: knum mul: mul add: add ^(Gendy1 ampdist: ampdist durdist: durdist adparam: adparam ddparam: ddparam minfreq: minfreq maxfreq: maxfreq ampscale: ampscale durscale: durscale initCPs: initCPs knum: knum) * mul + add!
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
  numChan: numChan trigger: trigger dur: dur carfreq: carfreq modfreq: modfreq index: index pan: pan envbufnum: envbufnum maxGrains: maxGrains mul: mul ^(GrainFM numChan: numChan trigger: trigger dur: dur carfreq: carfreq modfreq: modfreq index: index pan: pan envbufnum: envbufnum maxGrains: maxGrains) * mul!
  numChan: numChan trigger: trigger dur: dur carfreq: carfreq modfreq: modfreq index: index pan: pan envbufnum: envbufnum maxGrains: maxGrains mul: mul add: add ^(GrainFM numChan: numChan trigger: trigger dur: dur carfreq: carfreq modfreq: modfreq index: index pan: pan envbufnum: envbufnum maxGrains: maxGrains) * mul + add!
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
  numChan: numChan trigger: trigger dur: dur freq: freq pan: pan envbufnum: envbufnum maxGrains: maxGrains mul: mul ^(GrainSin numChan: numChan trigger: trigger dur: dur freq: freq pan: pan envbufnum: envbufnum maxGrains: maxGrains) * mul!
  numChan: numChan trigger: trigger dur: dur freq: freq pan: pan envbufnum: envbufnum maxGrains: maxGrains mul: mul add: add ^(GrainSin numChan: numChan trigger: trigger dur: dur freq: freq pan: pan envbufnum: envbufnum maxGrains: maxGrains) * mul + add!
  primaryFactoryMethod ^#numChan:trigger:dur:freq:pan:envbufnum:maxGrains:!
!

UGen subclass: #GrayNoise
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

GrayNoise comment: 'Gray Noise.'!


!GrayNoise methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #GrayNoise.
    paramNames := #().
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    !
!


!GrayNoise class methodsFor: 'instance creation'!
  mul: mul ^(GrayNoise new) * mul!
  mul: mul add: add ^(GrayNoise new) * mul + add!
  primaryFactoryMethod ^#new!
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
    ugenRate := #(0).
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
  in: in roomsize: roomsize revtime: revtime damping: damping inputbw: inputbw spread: spread drylevel: drylevel earlyreflevel: earlyreflevel taillevel: taillevel maxroomsize: maxroomsize mul: mul ^(GVerb in: in roomsize: roomsize revtime: revtime damping: damping inputbw: inputbw spread: spread drylevel: drylevel earlyreflevel: earlyreflevel taillevel: taillevel maxroomsize: maxroomsize) * mul!
  in: in roomsize: roomsize revtime: revtime damping: damping inputbw: inputbw spread: spread drylevel: drylevel earlyreflevel: earlyreflevel taillevel: taillevel maxroomsize: maxroomsize mul: mul add: add ^(GVerb in: in roomsize: roomsize revtime: revtime damping: damping inputbw: inputbw spread: spread drylevel: drylevel earlyreflevel: earlyreflevel taillevel: taillevel maxroomsize: maxroomsize) * mul + add!
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
    ugenRate := #(0).
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
  in: in mul: mul ^(Hasher in: in) * mul!
  in: in mul: mul add: add ^(Hasher in: in) * mul + add!
  primaryFactoryMethod ^#in:!
!

UGen subclass: #HenonL
  instanceVariableNames: 'freq a b x0 x1'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

HenonL comment: 'Henon map chaotic generator'!

!HenonL methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
  a ^a!
  a: x a := x!
  b ^b!
  b: x b := x!
  x0 ^x0!
  x0: x x0 := x!
  x1 ^x1!
  x1: x x1 := x!
!

!HenonL methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #HenonL.
    paramNames := #(#freq #a #b #x0 #x1).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 22050.0. a := 1.4. b := 0.3. x0 := 0.0. x1 := 0.0!
!


!HenonL class methodsFor: 'instance creation'!
  freq: freq a: a b: b x0: x0 x1: x1
    |u|
    u := self new.
    u freq: freq.
    u a: a.
    u b: b.
    u x0: x0.
    u x1: x1.
    ^u!
  freq: freq a: a b: b x0: x0 x1: x1 mul: mul ^(HenonL freq: freq a: a b: b x0: x0 x1: x1) * mul!
  freq: freq a: a b: b x0: x0 x1: x1 mul: mul add: add ^(HenonL freq: freq a: a b: b x0: x0 x1: x1) * mul + add!
  primaryFactoryMethod ^#freq:a:b:x0:x1:!
!

UGen subclass: #HenonC
  instanceVariableNames: 'freq a b x0 x1'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

HenonC comment: 'Henon map chaotic generator'!

!HenonC methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
  a ^a!
  a: x a := x!
  b ^b!
  b: x b := x!
  x0 ^x0!
  x0: x x0 := x!
  x1 ^x1!
  x1: x x1 := x!
!

!HenonC methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #HenonC.
    paramNames := #(#freq #a #b #x0 #x1).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 22050.0. a := 1.4. b := 0.3. x0 := 0.0. x1 := 0.0!
!


!HenonC class methodsFor: 'instance creation'!
  freq: freq a: a b: b x0: x0 x1: x1
    |u|
    u := self new.
    u freq: freq.
    u a: a.
    u b: b.
    u x0: x0.
    u x1: x1.
    ^u!
  freq: freq a: a b: b x0: x0 x1: x1 mul: mul ^(HenonC freq: freq a: a b: b x0: x0 x1: x1) * mul!
  freq: freq a: a b: b x0: x0 x1: x1 mul: mul add: add ^(HenonC freq: freq a: a b: b x0: x0 x1: x1) * mul + add!
  primaryFactoryMethod ^#freq:a:b:x0:x1:!
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
    ugenRate := #(0).
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
  in: in freq: freq mul: mul ^(HPF in: in freq: freq) * mul!
  in: in freq: freq mul: mul add: add ^(HPF in: in freq: freq) * mul + add!
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
    ugenRate := #(0).
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
  in: in mul: mul ^(HPZ1 in: in) * mul!
  in: in mul: mul add: add ^(HPZ1 in: in) * mul + add!
  primaryFactoryMethod ^#in:!
!

UGen subclass: #IFFT
  instanceVariableNames: 'buffer wintype winsize'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

IFFT comment: 'Inverse Fast Fourier Transform'!

!IFFT methodsFor: 'accessing'!
  buffer ^buffer!
  buffer: x buffer := x!
  wintype ^wintype!
  wintype: x wintype := x!
  winsize ^winsize!
  winsize: x winsize := x!
!

!IFFT methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #IFFT.
    paramNames := #(#buffer #wintype #winsize).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    buffer := 0.0. wintype := 0.0. winsize := 0.0!
!


!IFFT class methodsFor: 'instance creation'!
  buffer: buffer wintype: wintype winsize: winsize
    |u|
    u := self new.
    u buffer: buffer.
    u wintype: wintype.
    u winsize: winsize.
    ^u!
  buffer: buffer wintype: wintype winsize: winsize mul: mul ^(IFFT buffer: buffer wintype: wintype winsize: winsize) * mul!
  buffer: buffer wintype: wintype winsize: winsize mul: mul add: add ^(IFFT buffer: buffer wintype: wintype winsize: winsize) * mul + add!
  primaryFactoryMethod ^#buffer:wintype:winsize:!
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
  freq: freq phase: phase mul: mul ^(Impulse freq: freq phase: phase) * mul!
  freq: freq phase: phase mul: mul add: add ^(Impulse freq: freq phase: phase) * mul + add!
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
  numChan: numChan bus: bus mul: mul ^(In numChan: numChan bus: bus) * mul!
  numChan: numChan bus: bus mul: mul add: add ^(In numChan: numChan bus: bus) * mul + add!
  primaryFactoryMethod ^#numChan:bus:!
!

UGen subclass: #Index
  instanceVariableNames: 'bufnum in'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Index comment: 'Index into a table with a signal'!

!Index methodsFor: 'accessing'!
  bufnum ^bufnum!
  bufnum: x bufnum := x!
  in ^in!
  in: x in := x!
!

!Index methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Index.
    paramNames := #(#bufnum #in).
    mceNames := #().
    ugenRate := #(1).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    bufnum := 0.0. in := 0.0!
!


!Index class methodsFor: 'instance creation'!
  bufnum: bufnum in: in
    |u|
    u := self new.
    u bufnum: bufnum.
    u in: in.
    ^u!
  bufnum: bufnum in: in mul: mul ^(Index bufnum: bufnum in: in) * mul!
  bufnum: bufnum in: in mul: mul add: add ^(Index bufnum: bufnum in: in) * mul + add!
  primaryFactoryMethod ^#bufnum:in:!
!

UGen subclass: #IndexInBetween
  instanceVariableNames: 'bufnum in'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

IndexInBetween comment: 'Finds the (lowest) point in the Buffer at which the input signal lies in-between the two values'!

!IndexInBetween methodsFor: 'accessing'!
  bufnum ^bufnum!
  bufnum: x bufnum := x!
  in ^in!
  in: x in := x!
!

!IndexInBetween methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #IndexInBetween.
    paramNames := #(#bufnum #in).
    mceNames := #().
    ugenRate := #(1).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    bufnum := 0.0. in := 0.0!
!


!IndexInBetween class methodsFor: 'instance creation'!
  bufnum: bufnum in: in
    |u|
    u := self new.
    u bufnum: bufnum.
    u in: in.
    ^u!
  bufnum: bufnum in: in mul: mul ^(IndexInBetween bufnum: bufnum in: in) * mul!
  bufnum: bufnum in: in mul: mul add: add ^(IndexInBetween bufnum: bufnum in: in) * mul + add!
  primaryFactoryMethod ^#bufnum:in:!
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
  numChan: numChan bus: bus mul: mul ^(InFeedback numChan: numChan bus: bus) * mul!
  numChan: numChan bus: bus mul: mul add: add ^(InFeedback numChan: numChan bus: bus) * mul + add!
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
    ugenRate := #(0).
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
  in: in lo: lo hi: hi mul: mul ^(InRange in: in lo: lo hi: hi) * mul!
  in: in lo: lo hi: hi mul: mul add: add ^(InRange in: in lo: lo hi: hi) * mul + add!
  primaryFactoryMethod ^#in:lo:hi:!
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
  lo: lo hi: hi mul: mul ^(IRand lo: lo hi: hi) * mul!
  lo: lo hi: hi mul: mul add: add ^(IRand lo: lo hi: hi) * mul + add!
  primaryFactoryMethod ^#lo:hi:!
!

UGen subclass: #Integrator
  instanceVariableNames: 'in coef'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Integrator comment: 'A leaky integrator.'!

!Integrator methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  coef ^coef!
  coef: x coef := x!
!

!Integrator methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Integrator.
    paramNames := #(#in #coef).
    mceNames := #().
    ugenRate := #(0).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. coef := 1.0!
!


!Integrator class methodsFor: 'instance creation'!
  in: in coef: coef
    |u|
    u := self new.
    u in: in.
    u coef: coef.
    ^u!
  in: in coef: coef mul: mul ^(Integrator in: in coef: coef) * mul!
  in: in coef: coef mul: mul add: add ^(Integrator in: in coef: coef) * mul + add!
  primaryFactoryMethod ^#in:coef:!
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
  in: in mul: mul ^(K2A in: in) * mul!
  in: in mul: mul add: add ^(K2A in: in) * mul + add!
  primaryFactoryMethod ^#in:!
!

UGen subclass: #KeyState
  instanceVariableNames: 'keycode minval maxval lag'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

KeyState comment: 'Respond to the state of a key'!

!KeyState methodsFor: 'accessing'!
  keycode ^keycode!
  keycode: x keycode := x!
  minval ^minval!
  minval: x minval := x!
  maxval ^maxval!
  maxval: x maxval := x!
  lag ^lag!
  lag: x lag := x!
!

!KeyState methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #KeyState.
    paramNames := #(#keycode #minval #maxval #lag).
    mceNames := #().
    ugenRate := #kr.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    keycode := 0.0. minval := 0.0. maxval := 1.0. lag := 0.2!
!


!KeyState class methodsFor: 'instance creation'!
  keycode: keycode minval: minval maxval: maxval lag: lag
    |u|
    u := self new.
    u keycode: keycode.
    u minval: minval.
    u maxval: maxval.
    u lag: lag.
    ^u!
  keycode: keycode minval: minval maxval: maxval lag: lag mul: mul ^(KeyState keycode: keycode minval: minval maxval: maxval lag: lag) * mul!
  keycode: keycode minval: minval maxval: maxval lag: lag mul: mul add: add ^(KeyState keycode: keycode minval: minval maxval: maxval lag: lag) * mul + add!
  primaryFactoryMethod ^#keycode:minval:maxval:lag:!
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
  freqscale: freqscale freqoffset: freqoffset specificationsArrayRef: specificationsArrayRef mul: mul ^(Klang freqscale: freqscale freqoffset: freqoffset specificationsArrayRef: specificationsArrayRef) * mul!
  freqscale: freqscale freqoffset: freqoffset specificationsArrayRef: specificationsArrayRef mul: mul add: add ^(Klang freqscale: freqscale freqoffset: freqoffset specificationsArrayRef: specificationsArrayRef) * mul + add!
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
    ugenRate := #(0).
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
  input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef mul: mul ^(Klank input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef) * mul!
  input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef mul: mul add: add ^(Klank input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef) * mul + add!
  primaryFactoryMethod ^#input:freqscale:freqoffset:decayscale:specificationsArrayRef:!
!

UGen subclass: #LFBrownNoise1
  instanceVariableNames: 'freq dev dist'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

LFBrownNoise1 comment: 'random walk linear interp'!

!LFBrownNoise1 methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
  dev ^dev!
  dev: x dev := x!
  dist ^dist!
  dist: x dist := x!
!

!LFBrownNoise1 methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #LFBrownNoise1.
    paramNames := #(#freq #dev #dist).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 20.0. dev := 1.0. dist := 0.0!
!


!LFBrownNoise1 class methodsFor: 'instance creation'!
  freq: freq dev: dev dist: dist
    |u|
    u := self new.
    u freq: freq.
    u dev: dev.
    u dist: dist.
    ^u!
  freq: freq dev: dev dist: dist mul: mul ^(LFBrownNoise1 freq: freq dev: dev dist: dist) * mul!
  freq: freq dev: dev dist: dist mul: mul add: add ^(LFBrownNoise1 freq: freq dev: dev dist: dist) * mul + add!
  primaryFactoryMethod ^#freq:dev:dist:!
!

UGen subclass: #LFClipNoise
  instanceVariableNames: 'freq'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

LFClipNoise comment: 'Clipped noise'!

!LFClipNoise methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
!

!LFClipNoise methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #LFClipNoise.
    paramNames := #(#freq).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 500.0!
!


!LFClipNoise class methodsFor: 'instance creation'!
  freq: freq
    |u|
    u := self new.
    u freq: freq.
    ^u!
  freq: freq mul: mul ^(LFClipNoise freq: freq) * mul!
  freq: freq mul: mul add: add ^(LFClipNoise freq: freq) * mul + add!
  primaryFactoryMethod ^#freq:!
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
  freq: freq iphase: iphase mul: mul ^(LFCub freq: freq iphase: iphase) * mul!
  freq: freq iphase: iphase mul: mul add: add ^(LFCub freq: freq iphase: iphase) * mul + add!
  primaryFactoryMethod ^#freq:iphase:!
!

UGen subclass: #LFDNoise1
  instanceVariableNames: 'freq'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

LFDNoise1 comment: 'Dynamic ramp noise'!

!LFDNoise1 methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
!

!LFDNoise1 methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #LFDNoise1.
    paramNames := #(#freq).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 500.0!
!


!LFDNoise1 class methodsFor: 'instance creation'!
  freq: freq
    |u|
    u := self new.
    u freq: freq.
    ^u!
  freq: freq mul: mul ^(LFDNoise1 freq: freq) * mul!
  freq: freq mul: mul add: add ^(LFDNoise1 freq: freq) * mul + add!
  primaryFactoryMethod ^#freq:!
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
  freq: freq mul: mul ^(LFDNoise3 freq: freq) * mul!
  freq: freq mul: mul add: add ^(LFDNoise3 freq: freq) * mul + add!
  primaryFactoryMethod ^#freq:!
!

UGen subclass: #LFGauss
  instanceVariableNames: 'duration width iphase loop doneAction'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

LFGauss comment: 'Gaussian function oscillator'!

!LFGauss methodsFor: 'accessing'!
  duration ^duration!
  duration: x duration := x!
  width ^width!
  width: x width := x!
  iphase ^iphase!
  iphase: x iphase := x!
  loop ^loop!
  loop: x loop := x!
  doneAction ^doneAction!
  doneAction: x doneAction := x!
!

!LFGauss methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #LFGauss.
    paramNames := #(#duration #width #iphase #loop #doneAction).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    duration := 1.0. width := 0.1. iphase := 0.0. loop := 1.0. doneAction := 0.0!
!


!LFGauss class methodsFor: 'instance creation'!
  duration: duration width: width iphase: iphase loop: loop doneAction: doneAction
    |u|
    u := self new.
    u duration: duration.
    u width: width.
    u iphase: iphase.
    u loop: loop.
    u doneAction: doneAction.
    ^u!
  duration: duration width: width iphase: iphase loop: loop doneAction: doneAction mul: mul ^(LFGauss duration: duration width: width iphase: iphase loop: loop doneAction: doneAction) * mul!
  duration: duration width: width iphase: iphase loop: loop doneAction: doneAction mul: mul add: add ^(LFGauss duration: duration width: width iphase: iphase loop: loop doneAction: doneAction) * mul + add!
  primaryFactoryMethod ^#duration:width:iphase:loop:doneAction:!
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
  freq: freq mul: mul ^(LFNoise0 freq: freq) * mul!
  freq: freq mul: mul add: add ^(LFNoise0 freq: freq) * mul + add!
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
  freq: freq mul: mul ^(LFNoise1 freq: freq) * mul!
  freq: freq mul: mul add: add ^(LFNoise1 freq: freq) * mul + add!
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
  freq: freq mul: mul ^(LFNoise2 freq: freq) * mul!
  freq: freq mul: mul add: add ^(LFNoise2 freq: freq) * mul + add!
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
  freq: freq iphase: iphase mul: mul ^(LFPar freq: freq iphase: iphase) * mul!
  freq: freq iphase: iphase mul: mul add: add ^(LFPar freq: freq iphase: iphase) * mul + add!
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
  freq: freq iphase: iphase width: width mul: mul ^(LFPulse freq: freq iphase: iphase width: width) * mul!
  freq: freq iphase: iphase width: width mul: mul add: add ^(LFPulse freq: freq iphase: iphase width: width) * mul + add!
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
  freq: freq iphase: iphase mul: mul ^(LFSaw freq: freq iphase: iphase) * mul!
  freq: freq iphase: iphase mul: mul add: add ^(LFSaw freq: freq iphase: iphase) * mul + add!
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
  freq: freq iphase: iphase mul: mul ^(LFTri freq: freq iphase: iphase) * mul!
  freq: freq iphase: iphase mul: mul add: add ^(LFTri freq: freq iphase: iphase) * mul + add!
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
    ugenRate := #(0).
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
  in: in freq: freq mul: mul ^(LPF in: in freq: freq) * mul!
  in: in freq: freq mul: mul add: add ^(LPF in: in freq: freq) * mul + add!
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
    ugenRate := #(0).
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
  in: in lagTime: lagTime mul: mul ^(Lag in: in lagTime: lagTime) * mul!
  in: in lagTime: lagTime mul: mul add: add ^(Lag in: in lagTime: lagTime) * mul + add!
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
    ugenRate := #(0).
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
  in: in lagTimeU: lagTimeU lagTimeD: lagTimeD mul: mul ^(LagUD in: in lagTimeU: lagTimeU lagTimeD: lagTimeD) * mul!
  in: in lagTimeU: lagTimeU lagTimeD: lagTimeD mul: mul add: add ^(LagUD in: in lagTimeU: lagTimeU lagTimeD: lagTimeD) * mul + add!
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
    ugenRate := #(0).
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
  in: in lagTime: lagTime mul: mul ^(Lag2 in: in lagTime: lagTime) * mul!
  in: in lagTime: lagTime mul: mul add: add ^(Lag2 in: in lagTime: lagTime) * mul + add!
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
    ugenRate := #(0).
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
  in: in lagTime: lagTime mul: mul ^(Lag3 in: in lagTime: lagTime) * mul!
  in: in lagTime: lagTime mul: mul add: add ^(Lag3 in: in lagTime: lagTime) * mul + add!
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
    ugenRate := #(0).
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
  in: in lagTimeU: lagTimeU lagTimeD: lagTimeD mul: mul ^(Lag3UD in: in lagTimeU: lagTimeU lagTimeD: lagTimeD) * mul!
  in: in lagTimeU: lagTimeU lagTimeD: lagTimeD mul: mul add: add ^(Lag3UD in: in lagTimeU: lagTimeU lagTimeD: lagTimeD) * mul + add!
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
    ugenRate := #(0).
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
  in: in trig: trig mul: mul ^(Latch in: in trig: trig) * mul!
  in: in trig: trig mul: mul add: add ^(Latch in: in trig: trig) * mul + add!
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
    ugenRate := #(0).
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
  in: in coef: coef mul: mul ^(LeakDC in: in coef: coef) * mul!
  in: in coef: coef mul: mul add: add ^(LeakDC in: in coef: coef) * mul + add!
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
    ugenRate := #(0).
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
  in: in level: level dur: dur mul: mul ^(Limiter in: in level: level dur: dur) * mul!
  in: in level: level dur: dur mul: mul add: add ^(Limiter in: in level: level dur: dur) * mul + add!
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
  start: start end: end dur: dur doneAction: doneAction mul: mul ^(Line start: start end: end dur: dur doneAction: doneAction) * mul!
  start: start end: end dur: dur doneAction: doneAction mul: mul add: add ^(Line start: start end: end dur: dur doneAction: doneAction) * mul + add!
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
    ugenRate := #(0).
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
  in: in srclo: srclo srchi: srchi dstlo: dstlo dsthi: dsthi mul: mul ^(LinExp in: in srclo: srclo srchi: srchi dstlo: dstlo dsthi: dsthi) * mul!
  in: in srclo: srclo srchi: srchi dstlo: dstlo dsthi: dsthi mul: mul add: add ^(LinExp in: in srclo: srclo srchi: srchi dstlo: dstlo dsthi: dsthi) * mul + add!
  primaryFactoryMethod ^#in:srclo:srchi:dstlo:dsthi:!
!

UGen subclass: #LinPan2
  instanceVariableNames: 'in pos level'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

LinPan2 comment: 'Two channel linear pan.'!

!LinPan2 methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  pos ^pos!
  pos: x pos := x!
  level ^level!
  level: x level := x!
!

!LinPan2 methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #LinPan2.
    paramNames := #(#in #pos #level).
    mceNames := #().
    ugenRate := #(0).
    numChan := 2.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. pos := 0.0. level := 1.0!
!


!LinPan2 class methodsFor: 'instance creation'!
  in: in pos: pos level: level
    |u|
    u := self new.
    u in: in.
    u pos: pos.
    u level: level.
    ^u!
  in: in pos: pos level: level mul: mul ^(LinPan2 in: in pos: pos level: level) * mul!
  in: in pos: pos level: level mul: mul add: add ^(LinPan2 in: in pos: pos level: level) * mul + add!
  primaryFactoryMethod ^#in:pos:level:!
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
  lo: lo hi: hi minmax: minmax mul: mul ^(LinRand lo: lo hi: hi minmax: minmax) * mul!
  lo: lo hi: hi minmax: minmax mul: mul add: add ^(LinRand lo: lo hi: hi minmax: minmax) * mul + add!
  primaryFactoryMethod ^#lo:hi:minmax:!
!

UGen subclass: #LinXFade2
  instanceVariableNames: 'inA inB pan'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

LinXFade2 comment: 'Two channel linear crossfade.'!

!LinXFade2 methodsFor: 'accessing'!
  inA ^inA!
  inA: x inA := x!
  inB ^inB!
  inB: x inB := x!
  pan ^pan!
  pan: x pan := x!
!

!LinXFade2 methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #LinXFade2.
    paramNames := #(#inA #inB #pan).
    mceNames := #().
    ugenRate := #(0 1).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    inA := 0.0. inB := 0.0. pan := 0.0!
!


!LinXFade2 class methodsFor: 'instance creation'!
  inA: inA inB: inB pan: pan
    |u|
    u := self new.
    u inA: inA.
    u inB: inB.
    u pan: pan.
    ^u!
  inA: inA inB: inB pan: pan mul: mul ^(LinXFade2 inA: inA inB: inB pan: pan) * mul!
  inA: inA inB: inB pan: pan mul: mul add: add ^(LinXFade2 inA: inA inB: inB pan: pan) * mul + add!
  primaryFactoryMethod ^#inA:inB:pan:!
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
  numChannels: numChannels numFrames: numFrames mul: mul ^(LocalBuf numChannels: numChannels numFrames: numFrames) * mul!
  numChannels: numChannels numFrames: numFrames mul: mul add: add ^(LocalBuf numChannels: numChannels numFrames: numFrames) * mul + add!
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
  numChan: numChan default: default mul: mul ^(LocalIn numChan: numChan default: default) * mul!
  numChan: numChan default: default mul: mul add: add ^(LocalIn numChan: numChan default: default) * mul + add!
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
    ugenRate := #(0).
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
  channelsArray: channelsArray mul: mul ^(LocalOut channelsArray: channelsArray) * mul!
  channelsArray: channelsArray mul: mul add: add ^(LocalOut channelsArray: channelsArray) * mul + add!
  primaryFactoryMethod ^#channelsArray:!
!

UGen subclass: #LPZ1
  instanceVariableNames: 'in'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

LPZ1 comment: 'Two point average filter'!

!LPZ1 methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
!

!LPZ1 methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #LPZ1.
    paramNames := #(#in).
    mceNames := #().
    ugenRate := #(0).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0!
!


!LPZ1 class methodsFor: 'instance creation'!
  in: in
    |u|
    u := self new.
    u in: in.
    ^u!
  in: in mul: mul ^(LPZ1 in: in) * mul!
  in: in mul: mul add: add ^(LPZ1 in: in) * mul + add!
  primaryFactoryMethod ^#in:!
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
    ugenRate := #(0).
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
  in: in bits: bits mul: mul ^(MantissaMask in: in bits: bits) * mul!
  in: in bits: bits mul: mul add: add ^(MantissaMask in: in bits: bits) * mul + add!
  primaryFactoryMethod ^#in:bits:!
!

UGen subclass: #MaxLocalBufs
  instanceVariableNames: 'count'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

MaxLocalBufs comment: 'LocalBuf count'!

!MaxLocalBufs methodsFor: 'accessing'!
  count ^count!
  count: x count := x!
!

!MaxLocalBufs methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #MaxLocalBufs.
    paramNames := #(#count).
    mceNames := #().
    ugenRate := #ir.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    count := 0.0!
!


!MaxLocalBufs class methodsFor: 'instance creation'!
  count: count
    |u|
    u := self new.
    u count: count.
    ^u!
  count: count mul: mul ^(MaxLocalBufs count: count) * mul!
  count: count mul: mul add: add ^(MaxLocalBufs count: count) * mul + add!
  primaryFactoryMethod ^#count:!
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
    ugenRate := #(0).
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
  x: x y: y mod: mod mul: mul ^(ModDif x: x y: y mod: mod) * mul!
  x: x y: y mod: mod mul: mul add: add ^(ModDif x: x y: y mod: mod) * mul + add!
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
    ugenRate := #(0).
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
  in: in freq: freq gain: gain reset: reset mul: mul ^(MoogFF in: in freq: freq gain: gain reset: reset) * mul!
  in: in freq: freq gain: gain reset: reset mul: mul add: add ^(MoogFF in: in freq: freq gain: gain reset: reset) * mul + add!
  primaryFactoryMethod ^#in:freq:gain:reset:!
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
  minval: minval maxval: maxval lag: lag mul: mul ^(MouseButton minval: minval maxval: maxval lag: lag) * mul!
  minval: minval maxval: maxval lag: lag mul: mul add: add ^(MouseButton minval: minval maxval: maxval lag: lag) * mul + add!
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
  minval: minval maxval: maxval warp: warp lag: lag mul: mul ^(MouseX minval: minval maxval: maxval warp: warp lag: lag) * mul!
  minval: minval maxval: maxval warp: warp lag: lag mul: mul add: add ^(MouseX minval: minval maxval: maxval warp: warp lag: lag) * mul + add!
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
  minval: minval maxval: maxval warp: warp lag: lag mul: mul ^(MouseY minval: minval maxval: maxval warp: warp lag: lag) * mul!
  minval: minval maxval: maxval warp: warp lag: lag mul: mul add: add ^(MouseY minval: minval maxval: maxval warp: warp lag: lag) * mul + add!
  primaryFactoryMethod ^#minval:maxval:warp:lag:!
!

UGen subclass: #MulAdd
  instanceVariableNames: 'in mul add'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

MulAdd comment: 'Multiply add'!

!MulAdd methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  mul ^mul!
  mul: x mul := x!
  add ^add!
  add: x add := x!
!

!MulAdd methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #MulAdd.
    paramNames := #(#in #mul #add).
    mceNames := #().
    ugenRate := #(0 1 2).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. mul := 0.0. add := 0.0!
!


!MulAdd class methodsFor: 'instance creation'!
  in: in mul: mul add: add
    |u|
    ((mul = 1) and: (add = 0)) ifTrue: [ ^in ].
    u := self new.
    u in: in.
    u mul: mul.
    u add: add.
    ^u!
  primaryFactoryMethod ^#in:mul:add:!
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
    ugenRate := #(0).
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
  in: in level: level dur: dur mul: mul ^(Normalizer in: in level: level dur: dur) * mul!
  in: in level: level dur: dur mul: mul add: add ^(Normalizer in: in level: level dur: dur) * mul + add!
  primaryFactoryMethod ^#in:level:dur:!
!

UGen subclass: #NRand
  instanceVariableNames: 'lo hi n'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

NRand comment: 'Sum of uniform distributions.'!

!NRand methodsFor: 'accessing'!
  lo ^lo!
  lo: x lo := x!
  hi ^hi!
  hi: x hi := x!
  n ^n!
  n: x n := x!
!

!NRand methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #NRand.
    paramNames := #(#lo #hi #n).
    mceNames := #().
    ugenRate := #ir.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    lo := 0.0. hi := 1.0. n := 0.0!
!


!NRand class methodsFor: 'instance creation'!
  lo: lo hi: hi n: n
    |u|
    u := self new.
    u lo: lo.
    u hi: hi.
    u n: n.
    ^u!
  lo: lo hi: hi n: n mul: mul ^(NRand lo: lo hi: hi n: n) * mul!
  lo: lo hi: hi n: n mul: mul add: add ^(NRand lo: lo hi: hi n: n) * mul + add!
  primaryFactoryMethod ^#lo:hi:n:!
!

UGen subclass: #NumOutputBuses
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

NumOutputBuses comment: 'Number of output busses.'!


!NumOutputBuses methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #NumOutputBuses.
    paramNames := #().
    mceNames := #().
    ugenRate := #ir.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    !
!


!NumOutputBuses class methodsFor: 'instance creation'!
  mul: mul ^(NumOutputBuses new) * mul!
  mul: mul add: add ^(NumOutputBuses new) * mul + add!
  primaryFactoryMethod ^#new!
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
    ugenRate := #(0).
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
  in: in coef: coef mul: mul ^(OnePole in: in coef: coef) * mul!
  in: in coef: coef mul: mul add: add ^(OnePole in: in coef: coef) * mul + add!
  primaryFactoryMethod ^#in:coef:!
!

UGen subclass: #Osc
  instanceVariableNames: 'bufnum freq phase'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Osc comment: 'Interpolating wavetable oscillator.'!

!Osc methodsFor: 'accessing'!
  bufnum ^bufnum!
  bufnum: x bufnum := x!
  freq ^freq!
  freq: x freq := x!
  phase ^phase!
  phase: x phase := x!
!

!Osc methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Osc.
    paramNames := #(#bufnum #freq #phase).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    bufnum := 0.0. freq := 440.0. phase := 0.0!
!


!Osc class methodsFor: 'instance creation'!
  bufnum: bufnum freq: freq phase: phase
    |u|
    u := self new.
    u bufnum: bufnum.
    u freq: freq.
    u phase: phase.
    ^u!
  bufnum: bufnum freq: freq phase: phase mul: mul ^(Osc bufnum: bufnum freq: freq phase: phase) * mul!
  bufnum: bufnum freq: freq phase: phase mul: mul add: add ^(Osc bufnum: bufnum freq: freq phase: phase) * mul + add!
  primaryFactoryMethod ^#bufnum:freq:phase:!
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
    ugenRate := #(1).
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
  bus: bus channelsArray: channelsArray mul: mul ^(Out bus: bus channelsArray: channelsArray) * mul!
  bus: bus channelsArray: channelsArray mul: mul add: add ^(Out bus: bus channelsArray: channelsArray) * mul + add!
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
    ugenRate := #(0).
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
  in: in pos: pos level: level mul: mul ^(Pan2 in: in pos: pos level: level) * mul!
  in: in pos: pos level: level mul: mul add: add ^(Pan2 in: in pos: pos level: level) * mul + add!
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
  trig: trig rate: rate start: start end: end resetPos: resetPos mul: mul ^(Phasor trig: trig rate: rate start: start end: end resetPos: resetPos) * mul!
  trig: trig rate: rate start: start end: end resetPos: resetPos mul: mul add: add ^(Phasor trig: trig rate: rate start: start end: end resetPos: resetPos) * mul + add!
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
  mul: mul ^(PinkNoise new) * mul!
  mul: mul add: add ^(PinkNoise new) * mul + add!
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
  in: in initFreq: initFreq minFreq: minFreq maxFreq: maxFreq execFreq: execFreq maxBinsPerOctave: maxBinsPerOctave median: median ampThreshold: ampThreshold peakThreshold: peakThreshold downSample: downSample clar: clar mul: mul ^(Pitch in: in initFreq: initFreq minFreq: minFreq maxFreq: maxFreq execFreq: execFreq maxBinsPerOctave: maxBinsPerOctave median: median ampThreshold: ampThreshold peakThreshold: peakThreshold downSample: downSample clar: clar) * mul!
  in: in initFreq: initFreq minFreq: minFreq maxFreq: maxFreq execFreq: execFreq maxBinsPerOctave: maxBinsPerOctave median: median ampThreshold: ampThreshold peakThreshold: peakThreshold downSample: downSample clar: clar mul: mul add: add ^(Pitch in: in initFreq: initFreq minFreq: minFreq maxFreq: maxFreq execFreq: execFreq maxBinsPerOctave: maxBinsPerOctave median: median ampThreshold: ampThreshold peakThreshold: peakThreshold downSample: downSample clar: clar) * mul + add!
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
    ugenRate := #(0).
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
  in: in windowSize: windowSize pitchRatio: pitchRatio pitchDispersion: pitchDispersion timeDispersion: timeDispersion mul: mul ^(PitchShift in: in windowSize: windowSize pitchRatio: pitchRatio pitchDispersion: pitchDispersion timeDispersion: timeDispersion) * mul!
  in: in windowSize: windowSize pitchRatio: pitchRatio pitchDispersion: pitchDispersion timeDispersion: timeDispersion mul: mul add: add ^(PitchShift in: in windowSize: windowSize pitchRatio: pitchRatio pitchDispersion: pitchDispersion timeDispersion: timeDispersion) * mul + add!
  primaryFactoryMethod ^#in:windowSize:pitchRatio:pitchDispersion:timeDispersion:!
!

UGen subclass: #PlayBuf
  instanceVariableNames: 'bufnum rate trigger startPos loop doneAction'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

PlayBuf comment: 'Sample playback oscillator.'!

!PlayBuf methodsFor: 'accessing'!
  bufnum ^bufnum!
  bufnum: x bufnum := x!
  rate ^rate!
  rate: x rate := x!
  trigger ^trigger!
  trigger: x trigger := x!
  startPos ^startPos!
  startPos: x startPos := x!
  loop ^loop!
  loop: x loop := x!
  doneAction ^doneAction!
  doneAction: x doneAction := x!
!

!PlayBuf methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #PlayBuf.
    paramNames := #(#bufnum #rate #trigger #startPos #loop #doneAction).
    mceNames := #().
    ugenRate := #ar.
    numChan := nil.
    specialIndex := 0.
    ugenId := nil.
    bufnum := 0.0. rate := 1.0. trigger := 1.0. startPos := 0.0. loop := 0.0. doneAction := 0.0!
!


!PlayBuf class methodsFor: 'instance creation'!
  numChan: numChan bufnum: bufnum rate: rate trigger: trigger startPos: startPos loop: loop doneAction: doneAction
    |u|
    u := self new.
    u numChan: numChan.
    u bufnum: bufnum.
    u rate: rate.
    u trigger: trigger.
    u startPos: startPos.
    u loop: loop.
    u doneAction: doneAction.
    ^u!
  numChan: numChan bufnum: bufnum rate: rate trigger: trigger startPos: startPos loop: loop doneAction: doneAction mul: mul ^(PlayBuf numChan: numChan bufnum: bufnum rate: rate trigger: trigger startPos: startPos loop: loop doneAction: doneAction) * mul!
  numChan: numChan bufnum: bufnum rate: rate trigger: trigger startPos: startPos loop: loop doneAction: doneAction mul: mul add: add ^(PlayBuf numChan: numChan bufnum: bufnum rate: rate trigger: trigger startPos: startPos loop: loop doneAction: doneAction) * mul + add!
  primaryFactoryMethod ^#numChan:bufnum:rate:trigger:startPos:loop:doneAction:!
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
    ugenRate := #(0).
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
  in: in trig: trig maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime coef: coef mul: mul ^(Pluck in: in trig: trig maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime coef: coef) * mul!
  in: in trig: trig maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime coef: coef mul: mul add: add ^(Pluck in: in trig: trig maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime coef: coef) * mul + add!
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
  freq: freq width: width mul: mul ^(Pulse freq: freq width: width) * mul!
  freq: freq width: width mul: mul add: add ^(Pulse freq: freq width: width) * mul + add!
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
    ugenRate := #(0).
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
  trig: trig reset: reset mul: mul ^(PulseCount trig: trig reset: reset) * mul!
  trig: trig reset: reset mul: mul add: add ^(PulseCount trig: trig reset: reset) * mul + add!
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
    ugenRate := #(0).
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
  trig: trig div: div start: start mul: mul ^(PulseDivider trig: trig div: div start: start) * mul!
  trig: trig div: div start: start mul: mul add: add ^(PulseDivider trig: trig div: div start: start) * mul + add!
  primaryFactoryMethod ^#trig:div:start:!
!

UGen subclass: #PV_RandComb
  instanceVariableNames: 'buffer wipe trig'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

PV_RandComb comment: 'Pass random bins.'!

!PV_RandComb methodsFor: 'accessing'!
  buffer ^buffer!
  buffer: x buffer := x!
  wipe ^wipe!
  wipe: x wipe := x!
  trig ^trig!
  trig: x trig := x!
!

!PV_RandComb methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #PV_RandComb.
    paramNames := #(#buffer #wipe #trig).
    mceNames := #().
    ugenRate := #kr.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    buffer := 0.0. wipe := 0.0. trig := 0.0!
!


!PV_RandComb class methodsFor: 'instance creation'!
  buffer: buffer wipe: wipe trig: trig
    |u|
    u := self new.
    u buffer: buffer.
    u wipe: wipe.
    u trig: trig.
    ^u!
  buffer: buffer wipe: wipe trig: trig mul: mul ^(PV_RandComb buffer: buffer wipe: wipe trig: trig) * mul!
  buffer: buffer wipe: wipe trig: trig mul: mul add: add ^(PV_RandComb buffer: buffer wipe: wipe trig: trig) * mul + add!
  primaryFactoryMethod ^#buffer:wipe:trig:!
!

UGen subclass: #QuadL
  instanceVariableNames: 'freq a b c xi'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

QuadL comment: 'General quadratic map chaotic generator'!

!QuadL methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
  a ^a!
  a: x a := x!
  b ^b!
  b: x b := x!
  c ^c!
  c: x c := x!
  xi ^xi!
  xi: x xi := x!
!

!QuadL methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #QuadL.
    paramNames := #(#freq #a #b #c #xi).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 22050.0. a := 1.0. b := -1.0. c := -0.75. xi := 0.0!
!


!QuadL class methodsFor: 'instance creation'!
  freq: freq a: a b: b c: c xi: xi
    |u|
    u := self new.
    u freq: freq.
    u a: a.
    u b: b.
    u c: c.
    u xi: xi.
    ^u!
  freq: freq a: a b: b c: c xi: xi mul: mul ^(QuadL freq: freq a: a b: b c: c xi: xi) * mul!
  freq: freq a: a b: b c: c xi: xi mul: mul add: add ^(QuadL freq: freq a: a b: b c: c xi: xi) * mul + add!
  primaryFactoryMethod ^#freq:a:b:c:xi:!
!

UGen subclass: #QuadC
  instanceVariableNames: 'freq a b c xi'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

QuadC comment: 'General quadratic map chaotic generator'!

!QuadC methodsFor: 'accessing'!
  freq ^freq!
  freq: x freq := x!
  a ^a!
  a: x a := x!
  b ^b!
  b: x b := x!
  c ^c!
  c: x c := x!
  xi ^xi!
  xi: x xi := x!
!

!QuadC methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #QuadC.
    paramNames := #(#freq #a #b #c #xi).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    freq := 22050.0. a := 1.0. b := -1.0. c := -0.75. xi := 0.0!
!


!QuadC class methodsFor: 'instance creation'!
  freq: freq a: a b: b c: c xi: xi
    |u|
    u := self new.
    u freq: freq.
    u a: a.
    u b: b.
    u c: c.
    u xi: xi.
    ^u!
  freq: freq a: a b: b c: c xi: xi mul: mul ^(QuadC freq: freq a: a b: b c: c xi: xi) * mul!
  freq: freq a: a b: b c: c xi: xi mul: mul add: add ^(QuadC freq: freq a: a b: b c: c xi: xi) * mul + add!
  primaryFactoryMethod ^#freq:a:b:c:xi:!
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
    ugenRate := #(0).
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
  in: in freq: freq rq: rq mul: mul ^(RHPF in: in freq: freq rq: rq) * mul!
  in: in freq: freq rq: rq mul: mul add: add ^(RHPF in: in freq: freq rq: rq) * mul + add!
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
    ugenRate := #(0).
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
  in: in freq: freq rq: rq mul: mul ^(RLPF in: in freq: freq rq: rq) * mul!
  in: in freq: freq rq: rq mul: mul add: add ^(RLPF in: in freq: freq rq: rq) * mul + add!
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
  lo: lo hi: hi mul: mul ^(Rand lo: lo hi: hi) * mul!
  lo: lo hi: hi mul: mul add: add ^(Rand lo: lo hi: hi) * mul + add!
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
  bufnum: bufnum offset: offset recLevel: recLevel preLevel: preLevel run: run loop: loop trigger: trigger doneAction: doneAction inputArray: inputArray mul: mul ^(RecordBuf bufnum: bufnum offset: offset recLevel: recLevel preLevel: preLevel run: run loop: loop trigger: trigger doneAction: doneAction inputArray: inputArray) * mul!
  bufnum: bufnum offset: offset recLevel: recLevel preLevel: preLevel run: run loop: loop trigger: trigger doneAction: doneAction inputArray: inputArray mul: mul add: add ^(RecordBuf bufnum: bufnum offset: offset recLevel: recLevel preLevel: preLevel run: run loop: loop trigger: trigger doneAction: doneAction inputArray: inputArray) * mul + add!
  primaryFactoryMethod ^#bufnum:offset:recLevel:preLevel:run:loop:trigger:doneAction:inputArray:!
!

UGen subclass: #ReplaceOut
  instanceVariableNames: 'bus channelsArray'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

ReplaceOut comment: 'Send signal to a bus, overwriting previous contents.'!

!ReplaceOut methodsFor: 'accessing'!
  bus ^bus!
  bus: x bus := x!
  channelsArray ^channelsArray!
  channelsArray: x channelsArray := x!
!

!ReplaceOut methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #ReplaceOut.
    paramNames := #(#bus).
    mceNames := #(#channelsArray).
    ugenRate := #(1).
    numChan := 0.
    specialIndex := 0.
    ugenId := nil.
    bus := 0.0!
!


!ReplaceOut class methodsFor: 'instance creation'!
  bus: bus channelsArray: channelsArray
    |u|
    u := self new.
    u bus: bus.
    u channelsArray: channelsArray.
    ^u!
  bus: bus channelsArray: channelsArray mul: mul ^(ReplaceOut bus: bus channelsArray: channelsArray) * mul!
  bus: bus channelsArray: channelsArray mul: mul add: add ^(ReplaceOut bus: bus channelsArray: channelsArray) * mul + add!
  primaryFactoryMethod ^#bus:channelsArray:!
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
    ugenRate := #(0).
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
  in: in freq: freq bwr: bwr mul: mul ^(Resonz in: in freq: freq bwr: bwr) * mul!
  in: in freq: freq bwr: bwr mul: mul add: add ^(Resonz in: in freq: freq bwr: bwr) * mul + add!
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
    ugenRate := #(0).
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
  in: in freq: freq decaytime: decaytime mul: mul ^(Ringz in: in freq: freq decaytime: decaytime) * mul!
  in: in freq: freq decaytime: decaytime mul: mul add: add ^(Ringz in: in freq: freq decaytime: decaytime) * mul + add!
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
    ugenRate := #(0).
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
  in: in trig: trig mul: mul ^(RunningMax in: in trig: trig) * mul!
  in: in trig: trig mul: mul add: add ^(RunningMax in: in trig: trig) * mul + add!
  primaryFactoryMethod ^#in:trig:!
!

UGen subclass: #Rotate2
  instanceVariableNames: 'x y pos'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Rotate2 comment: 'Rotate a sound field.'!

!Rotate2 methodsFor: 'accessing'!
  x ^x!
  x: xValue x := xValue!
  y ^y!
  y: x y := x!
  pos ^pos!
  pos: x pos := x!
!

!Rotate2 methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Rotate2.
    paramNames := #(#x #y #pos).
    mceNames := #().
    ugenRate := #(0 1).
    numChan := 2.
    specialIndex := 0.
    ugenId := nil.
    x := 0.0. y := 0.0. pos := 0.0!
!


!Rotate2 class methodsFor: 'instance creation'!
  x: x y: y pos: pos
    |u|
    u := self new.
    u x: x.
    u y: y.
    u pos: pos.
    ^u!
  x: x y: y pos: pos mul: mul ^(Rotate2 x: x y: y pos: pos) * mul!
  x: x y: y pos: pos mul: mul add: add ^(Rotate2 x: x y: y pos: pos) * mul + add!
  primaryFactoryMethod ^#x:y:pos:!
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
  mul: mul ^(SampleDur new) * mul!
  mul: mul add: add ^(SampleDur new) * mul + add!
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
  mul: mul ^(SampleRate new) * mul!
  mul: mul add: add ^(SampleRate new) * mul + add!
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
  freq: freq mul: mul ^(Saw freq: freq) * mul!
  freq: freq mul: mul add: add ^(Saw freq: freq) * mul + add!
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
    ugenRate := #(0 1).
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
  which: which array: array mul: mul ^(Select which: which array: array) * mul!
  which: which array: array mul: mul add: add ^(Select which: which array: array) * mul + add!
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
  buf: buf offset: offset length: length array: array mul: mul ^(SetBuf buf: buf offset: offset length: length array: array) * mul!
  buf: buf offset: offset length: length array: array mul: mul add: add ^(SetBuf buf: buf offset: offset length: length array: array) * mul + add!
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
    ugenRate := #(0 1).
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
  trig: trig reset: reset mul: mul ^(SetResetFF trig: trig reset: reset) * mul!
  trig: trig reset: reset mul: mul add: add ^(SetResetFF trig: trig reset: reset) * mul + add!
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
  freq: freq phase: phase mul: mul ^(SinOsc freq: freq phase: phase) * mul!
  freq: freq phase: phase mul: mul add: add ^(SinOsc freq: freq phase: phase) * mul + add!
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
  freq: freq feedback: feedback mul: mul ^(SinOscFB freq: freq feedback: feedback) * mul!
  freq: freq feedback: feedback mul: mul add: add ^(SinOscFB freq: freq feedback: feedback) * mul + add!
  primaryFactoryMethod ^#freq:feedback:!
!

UGen subclass: #Slew
  instanceVariableNames: 'in up dn'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Slew comment: 'Slew rate limiter.'!

!Slew methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  up ^up!
  up: x up := x!
  dn ^dn!
  dn: x dn := x!
!

!Slew methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Slew.
    paramNames := #(#in #up #dn).
    mceNames := #().
    ugenRate := #(0).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. up := 1.0. dn := 1.0!
!


!Slew class methodsFor: 'instance creation'!
  in: in up: up dn: dn
    |u|
    u := self new.
    u in: in.
    u up: up.
    u dn: dn.
    ^u!
  in: in up: up dn: dn mul: mul ^(Slew in: in up: up dn: dn) * mul!
  in: in up: up dn: dn mul: mul add: add ^(Slew in: in up: up dn: dn) * mul + add!
  primaryFactoryMethod ^#in:up:dn:!
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
    ugenRate := #(0).
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
  in: in mul: mul ^(Slope in: in) * mul!
  in: in mul: mul add: add ^(Slope in: in) * mul + add!
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
    ugenRate := #(0).
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
  trig: trig reset: reset min: min max: max step: step resetval: resetval mul: mul ^(Stepper trig: trig reset: reset min: min max: max step: step resetval: resetval) * mul!
  trig: trig reset: reset min: min max: max step: step resetval: resetval mul: mul add: add ^(Stepper trig: trig reset: reset min: min max: max step: step resetval: resetval) * mul + add!
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
    ugenRate := #(0).
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
  trig: trig rate: rate mul: mul ^(Sweep trig: trig rate: rate) * mul!
  trig: trig rate: rate mul: mul add: add ^(Sweep trig: trig rate: rate) * mul + add!
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
  syncFreq: syncFreq sawFreq: sawFreq mul: mul ^(SyncSaw syncFreq: syncFreq sawFreq: sawFreq) * mul!
  syncFreq: syncFreq sawFreq: sawFreq mul: mul add: add ^(SyncSaw syncFreq: syncFreq sawFreq: sawFreq) * mul + add!
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
  dur: dur reset: reset doneAction: doneAction level: level gapFirst: gapFirst mul: mul ^(TDuty dur: dur reset: reset doneAction: doneAction level: level gapFirst: gapFirst) * mul!
  dur: dur reset: reset doneAction: doneAction level: level gapFirst: gapFirst mul: mul add: add ^(TDuty dur: dur reset: reset doneAction: doneAction level: level gapFirst: gapFirst) * mul + add!
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
    ugenRate := #(2).
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
  lo: lo hi: hi trig: trig mul: mul ^(TExpRand lo: lo hi: hi trig: trig) * mul!
  lo: lo hi: hi trig: trig mul: mul add: add ^(TExpRand lo: lo hi: hi trig: trig) * mul + add!
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
  numChan: numChan trigger: trigger bufnum: bufnum rate: rate centerPos: centerPos dur: dur pan: pan amp: amp interp: interp mul: mul ^(TGrains numChan: numChan trigger: trigger bufnum: bufnum rate: rate centerPos: centerPos dur: dur pan: pan amp: amp interp: interp) * mul!
  numChan: numChan trigger: trigger bufnum: bufnum rate: rate centerPos: centerPos dur: dur pan: pan amp: amp interp: interp mul: mul add: add ^(TGrains numChan: numChan trigger: trigger bufnum: bufnum rate: rate centerPos: centerPos dur: dur pan: pan amp: amp interp: interp) * mul + add!
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
    ugenRate := #(0).
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
  trig: trig mul: mul ^(Timer trig: trig) * mul!
  trig: trig mul: mul add: add ^(Timer trig: trig) * mul + add!
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
    ugenRate := #(2).
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
  lo: lo hi: hi trig: trig mul: mul ^(TIRand lo: lo hi: hi trig: trig) * mul!
  lo: lo hi: hi trig: trig mul: mul add: add ^(TIRand lo: lo hi: hi trig: trig) * mul + add!
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
    ugenRate := #(0).
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
  trig: trig mul: mul ^(ToggleFF trig: trig) * mul!
  trig: trig mul: mul add: add ^(ToggleFF trig: trig) * mul + add!
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
    ugenRate := #(2).
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
  lo: lo hi: hi trig: trig mul: mul ^(TRand lo: lo hi: hi trig: trig) * mul!
  lo: lo hi: hi trig: trig mul: mul add: add ^(TRand lo: lo hi: hi trig: trig) * mul + add!
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
    ugenRate := #(0).
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
  in: in dur: dur mul: mul ^(Trig in: in dur: dur) * mul!
  in: in dur: dur mul: mul add: add ^(Trig in: in dur: dur) * mul + add!
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
    ugenRate := #(0).
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
  in: in dur: dur mul: mul ^(Trig1 in: in dur: dur) * mul!
  in: in dur: dur mul: mul add: add ^(Trig1 in: in dur: dur) * mul + add!
  primaryFactoryMethod ^#in:dur:!
!

UGen subclass: #TwoPole
  instanceVariableNames: 'in freq radius'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

TwoPole comment: 'Two pole filter.'!

!TwoPole methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  freq ^freq!
  freq: x freq := x!
  radius ^radius!
  radius: x radius := x!
!

!TwoPole methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #TwoPole.
    paramNames := #(#in #freq #radius).
    mceNames := #().
    ugenRate := #(0).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. freq := 440.0. radius := 0.8!
!


!TwoPole class methodsFor: 'instance creation'!
  in: in freq: freq radius: radius
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u radius: radius.
    ^u!
  in: in freq: freq radius: radius mul: mul ^(TwoPole in: in freq: freq radius: radius) * mul!
  in: in freq: freq radius: radius mul: mul add: add ^(TwoPole in: in freq: freq radius: radius) * mul + add!
  primaryFactoryMethod ^#in:freq:radius:!
!

UGen subclass: #TwoZero
  instanceVariableNames: 'in freq radius'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

TwoZero comment: 'Two zero filter.'!

!TwoZero methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  freq ^freq!
  freq: x freq := x!
  radius ^radius!
  radius: x radius := x!
!

!TwoZero methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #TwoZero.
    paramNames := #(#in #freq #radius).
    mceNames := #().
    ugenRate := #(0).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. freq := 440.0. radius := 0.8!
!


!TwoZero class methodsFor: 'instance creation'!
  in: in freq: freq radius: radius
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u radius: radius.
    ^u!
  in: in freq: freq radius: radius mul: mul ^(TwoZero in: in freq: freq radius: radius) * mul!
  in: in freq: freq radius: radius mul: mul add: add ^(TwoZero in: in freq: freq radius: radius) * mul + add!
  primaryFactoryMethod ^#in:freq:radius:!
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
  freq: freq iphase: iphase width: width mul: mul ^(VarSaw freq: freq iphase: iphase width: width) * mul!
  freq: freq iphase: iphase width: width mul: mul add: add ^(VarSaw freq: freq iphase: iphase width: width) * mul + add!
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
  freq: freq rate: rate depth: depth delay: delay onset: onset rateVariation: rateVariation depthVariation: depthVariation iphase: iphase trig: trig mul: mul ^(Vibrato freq: freq rate: rate depth: depth delay: delay onset: onset rateVariation: rateVariation depthVariation: depthVariation iphase: iphase trig: trig) * mul!
  freq: freq rate: rate depth: depth delay: delay onset: onset rateVariation: rateVariation depthVariation: depthVariation iphase: iphase trig: trig mul: mul add: add ^(Vibrato freq: freq rate: rate depth: depth delay: delay onset: onset rateVariation: rateVariation depthVariation: depthVariation iphase: iphase trig: trig) * mul + add!
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
  mul: mul ^(WhiteNoise new) * mul!
  mul: mul add: add ^(WhiteNoise new) * mul + add!
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
    ugenRate := #(0).
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
  in: in lo: lo hi: hi mul: mul ^(Wrap in: in lo: lo hi: hi) * mul!
  in: in lo: lo hi: hi mul: mul add: add ^(Wrap in: in lo: lo hi: hi) * mul + add!
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
    ugenRate := #(0 1).
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
  inA: inA inB: inB pan: pan level: level mul: mul ^(XFade2 inA: inA inB: inB pan: pan level: level) * mul!
  inA: inA inB: inB pan: pan level: level mul: mul add: add ^(XFade2 inA: inA inB: inB pan: pan level: level) * mul + add!
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
  start: start end: end dur: dur doneAction: doneAction mul: mul ^(XLine start: start end: end dur: dur doneAction: doneAction) * mul!
  start: start end: end dur: dur doneAction: doneAction mul: mul add: add ^(XLine start: start end: end dur: dur doneAction: doneAction) * mul + add!
  primaryFactoryMethod ^#start:end:dur:doneAction:!
!

UGen subclass: #ZeroCrossing
  instanceVariableNames: 'in'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

ZeroCrossing comment: 'Zero crossing frequency follower'!

!ZeroCrossing methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
!

!ZeroCrossing methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #ZeroCrossing.
    paramNames := #(#in).
    mceNames := #().
    ugenRate := #(0).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0!
!


!ZeroCrossing class methodsFor: 'instance creation'!
  in: in
    |u|
    u := self new.
    u in: in.
    ^u!
  in: in mul: mul ^(ZeroCrossing in: in) * mul!
  in: in mul: mul add: add ^(ZeroCrossing in: in) * mul + add!
  primaryFactoryMethod ^#in:!
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
    ugenRate := #(0).
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
  in: in ffreq: ffreq res: res mul: mul ^(MoogLadder in: in ffreq: ffreq res: res) * mul!
  in: in ffreq: ffreq res: res mul: mul add: add ^(MoogLadder in: in ffreq: ffreq res: res) * mul + add!
  primaryFactoryMethod ^#in:ffreq:res:!
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
    ugenRate := #(0 1).
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
  in1: in1 in2: in2 damping: damping delaytime: delaytime diffusion: diffusion feedback: feedback moddepth: moddepth modfreq: modfreq size: size mul: mul ^(GreyholeRaw in1: in1 in2: in2 damping: damping delaytime: delaytime diffusion: diffusion feedback: feedback moddepth: moddepth modfreq: modfreq size: size) * mul!
  in1: in1 in2: in2 damping: damping delaytime: delaytime diffusion: diffusion feedback: feedback moddepth: moddepth modfreq: modfreq size: size mul: mul add: add ^(GreyholeRaw in1: in1 in2: in2 damping: damping delaytime: delaytime diffusion: diffusion feedback: feedback moddepth: moddepth modfreq: modfreq size: size) * mul + add!
  primaryFactoryMethod ^#in1:in2:damping:delaytime:diffusion:feedback:moddepth:modfreq:size:!
!

UGen subclass: #CrossoverDistortion
  instanceVariableNames: 'in amp smooth'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

CrossoverDistortion comment: 'class B/AB power amp distortion simulation'!

!CrossoverDistortion methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  amp ^amp!
  amp: x amp := x!
  smooth ^smooth!
  smooth: x smooth := x!
!

!CrossoverDistortion methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #CrossoverDistortion.
    paramNames := #(#in #amp #smooth).
    mceNames := #().
    ugenRate := #(0).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. amp := 0.5. smooth := 0.5!
!


!CrossoverDistortion class methodsFor: 'instance creation'!
  in: in amp: amp smooth: smooth
    |u|
    u := self new.
    u in: in.
    u amp: amp.
    u smooth: smooth.
    ^u!
  in: in amp: amp smooth: smooth mul: mul ^(CrossoverDistortion in: in amp: amp smooth: smooth) * mul!
  in: in amp: amp smooth: smooth mul: mul add: add ^(CrossoverDistortion in: in amp: amp smooth: smooth) * mul + add!
  primaryFactoryMethod ^#in:amp:smooth:!
!

UGen subclass: #Friction
  instanceVariableNames: 'in friction spring damp mass beltmass'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Friction comment: 'A physical model of a system with dry-friction. A chaotic filter.'!

!Friction methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  friction ^friction!
  friction: x friction := x!
  spring ^spring!
  spring: x spring := x!
  damp ^damp!
  damp: x damp := x!
  mass ^mass!
  mass: x mass := x!
  beltmass ^beltmass!
  beltmass: x beltmass := x!
!

!Friction methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Friction.
    paramNames := #(#in #friction #spring #damp #mass #beltmass).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. friction := 0.5. spring := 0.414. damp := 0.313. mass := 0.1. beltmass := 1.0!
!


!Friction class methodsFor: 'instance creation'!
  in: in friction: friction spring: spring damp: damp mass: mass beltmass: beltmass
    |u|
    u := self new.
    u in: in.
    u friction: friction.
    u spring: spring.
    u damp: damp.
    u mass: mass.
    u beltmass: beltmass.
    ^u!
  in: in friction: friction spring: spring damp: damp mass: mass beltmass: beltmass mul: mul ^(Friction in: in friction: friction spring: spring damp: damp mass: mass beltmass: beltmass) * mul!
  in: in friction: friction spring: spring damp: damp mass: mass beltmass: beltmass mul: mul add: add ^(Friction in: in friction: friction spring: spring damp: damp mass: mass beltmass: beltmass) * mul + add!
  primaryFactoryMethod ^#in:friction:spring:damp:mass:beltmass:!
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
  excitation: excitation tension: tension loss: loss mul: mul ^(MembraneCircle excitation: excitation tension: tension loss: loss) * mul!
  excitation: excitation tension: tension loss: loss mul: mul add: add ^(MembraneCircle excitation: excitation tension: tension loss: loss) * mul + add!
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
  in: in trig: trig pit: pit struct: struct bright: bright damp: damp pos: pos model: model poly: poly intern_exciter: intern_exciter easteregg: easteregg bypass: bypass mul: mul ^(MiRings in: in trig: trig pit: pit struct: struct bright: bright damp: damp pos: pos model: model poly: poly intern_exciter: intern_exciter easteregg: easteregg bypass: bypass) * mul!
  in: in trig: trig pit: pit struct: struct bright: bright damp: damp pos: pos model: model poly: poly intern_exciter: intern_exciter easteregg: easteregg bypass: bypass mul: mul add: add ^(MiRings in: in trig: trig pit: pit struct: struct bright: bright damp: damp pos: pos model: model poly: poly intern_exciter: intern_exciter easteregg: easteregg bypass: bypass) * mul + add!
  primaryFactoryMethod ^#in:trig:pit:struct:bright:damp:pos:model:poly:intern_exciter:easteregg:bypass:!
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

UGen subclass: #RCD
  instanceVariableNames: 'clock rotate reset div spread auto len down gates'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

RCD comment: 'rotating clock divider'!

!RCD methodsFor: 'accessing'!
  clock ^clock!
  clock: x clock := x!
  rotate ^rotate!
  rotate: x rotate := x!
  reset ^reset!
  reset: x reset := x!
  div ^div!
  div: x div := x!
  spread ^spread!
  spread: x spread := x!
  auto ^auto!
  auto: x auto := x!
  len ^len!
  len: x len := x!
  down ^down!
  down: x down := x!
  gates ^gates!
  gates: x gates := x!
!

!RCD methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #RCD.
    paramNames := #(#clock #rotate #reset #div #spread #auto #len #down #gates).
    mceNames := #().
    ugenRate := #(0).
    numChan := 8.
    specialIndex := 0.
    ugenId := nil.
    clock := 0.0. rotate := 0.0. reset := 0.0. div := 0.0. spread := 0.0. auto := 0.0. len := 0.0. down := 0.0. gates := 0.0!
!


!RCD class methodsFor: 'instance creation'!
  clock: clock rotate: rotate reset: reset div: div spread: spread auto: auto len: len down: down gates: gates
    |u|
    u := self new.
    u clock: clock.
    u rotate: rotate.
    u reset: reset.
    u div: div.
    u spread: spread.
    u auto: auto.
    u len: len.
    u down: down.
    u gates: gates.
    ^u!
  clock: clock rotate: rotate reset: reset div: div spread: spread auto: auto len: len down: down gates: gates mul: mul ^(RCD clock: clock rotate: rotate reset: reset div: div spread: spread auto: auto len: len down: down gates: gates) * mul!
  clock: clock rotate: rotate reset: reset div: div spread: spread auto: auto len: len down: down gates: gates mul: mul add: add ^(RCD clock: clock rotate: rotate reset: reset div: div spread: spread auto: auto len: len down: down gates: gates) * mul + add!
  primaryFactoryMethod ^#clock:rotate:reset:div:spread:auto:len:down:gates:!
!

UGen subclass: #SCM
  instanceVariableNames: 'clock bpm rotate slip shuffle skip pw'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

SCM comment: 'shuffling clock multiplier'!

!SCM methodsFor: 'accessing'!
  clock ^clock!
  clock: x clock := x!
  bpm ^bpm!
  bpm: x bpm := x!
  rotate ^rotate!
  rotate: x rotate := x!
  slip ^slip!
  slip: x slip := x!
  shuffle ^shuffle!
  shuffle: x shuffle := x!
  skip ^skip!
  skip: x skip := x!
  pw ^pw!
  pw: x pw := x!
!

!SCM methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #SCM.
    paramNames := #(#clock #bpm #rotate #slip #shuffle #skip #pw).
    mceNames := #().
    ugenRate := #ar.
    numChan := 8.
    specialIndex := 0.
    ugenId := nil.
    clock := 0.0. bpm := 120.0. rotate := 0.0. slip := 0.0. shuffle := 0.0. skip := 0.0. pw := 0.0!
!


!SCM class methodsFor: 'instance creation'!
  clock: clock bpm: bpm rotate: rotate slip: slip shuffle: shuffle skip: skip pw: pw
    |u|
    u := self new.
    u clock: clock.
    u bpm: bpm.
    u rotate: rotate.
    u slip: slip.
    u shuffle: shuffle.
    u skip: skip.
    u pw: pw.
    ^u!
  clock: clock bpm: bpm rotate: rotate slip: slip shuffle: shuffle skip: skip pw: pw mul: mul ^(SCM clock: clock bpm: bpm rotate: rotate slip: slip shuffle: shuffle skip: skip pw: pw) * mul!
  clock: clock bpm: bpm rotate: rotate slip: slip shuffle: shuffle skip: skip pw: pw mul: mul add: add ^(SCM clock: clock bpm: bpm rotate: rotate slip: slip shuffle: shuffle skip: skip pw: pw) * mul + add!
  primaryFactoryMethod ^#clock:bpm:rotate:slip:shuffle:skip:pw:!
!

UGen subclass: #DustRange
  instanceVariableNames: 'iotMin iotMax'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

DustRange comment: '(Undocumented class)'!

!DustRange methodsFor: 'accessing'!
  iotMin ^iotMin!
  iotMin: x iotMin := x!
  iotMax ^iotMax!
  iotMax: x iotMax := x!
!

!DustRange methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #DustRange.
    paramNames := #(#iotMin #iotMax).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    iotMin := 0.1. iotMax := 1.0!
!


!DustRange class methodsFor: 'instance creation'!
  iotMin: iotMin iotMax: iotMax
    |u|
    u := self new.
    u iotMin: iotMin.
    u iotMax: iotMax.
    ^u!
  iotMin: iotMin iotMax: iotMax mul: mul ^(DustRange iotMin: iotMin iotMax: iotMax) * mul!
  iotMin: iotMin iotMax: iotMax mul: mul add: add ^(DustRange iotMin: iotMin iotMax: iotMax) * mul + add!
  primaryFactoryMethod ^#iotMin:iotMax:!
!

UGen subclass: #ExpRandN
  instanceVariableNames: 'lo hi'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

ExpRandN comment: '(Undocumented class)'!

!ExpRandN methodsFor: 'accessing'!
  lo ^lo!
  lo: x lo := x!
  hi ^hi!
  hi: x hi := x!
!

!ExpRandN methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #ExpRandN.
    paramNames := #(#lo #hi).
    mceNames := #().
    ugenRate := #ir.
    numChan := nil.
    specialIndex := 0.
    ugenId := nil.
    lo := 0.0. hi := 1.0!
!


!ExpRandN class methodsFor: 'instance creation'!
  numChan: numChan lo: lo hi: hi
    |u|
    u := self new.
    u numChan: numChan.
    u lo: lo.
    u hi: hi.
    ^u!
  numChan: numChan lo: lo hi: hi mul: mul ^(ExpRandN numChan: numChan lo: lo hi: hi) * mul!
  numChan: numChan lo: lo hi: hi mul: mul add: add ^(ExpRandN numChan: numChan lo: lo hi: hi) * mul + add!
  primaryFactoryMethod ^#numChan:lo:hi:!
!

UGen subclass: #LinRandN
  instanceVariableNames: 'lo hi minmax'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

LinRandN comment: '(Undocumented class)'!

!LinRandN methodsFor: 'accessing'!
  lo ^lo!
  lo: x lo := x!
  hi ^hi!
  hi: x hi := x!
  minmax ^minmax!
  minmax: x minmax := x!
!

!LinRandN methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #LinRandN.
    paramNames := #(#lo #hi #minmax).
    mceNames := #().
    ugenRate := #ir.
    numChan := nil.
    specialIndex := 0.
    ugenId := nil.
    lo := 0.0. hi := 1.0. minmax := 0.0!
!


!LinRandN class methodsFor: 'instance creation'!
  numChan: numChan lo: lo hi: hi minmax: minmax
    |u|
    u := self new.
    u numChan: numChan.
    u lo: lo.
    u hi: hi.
    u minmax: minmax.
    ^u!
  numChan: numChan lo: lo hi: hi minmax: minmax mul: mul ^(LinRandN numChan: numChan lo: lo hi: hi minmax: minmax) * mul!
  numChan: numChan lo: lo hi: hi minmax: minmax mul: mul add: add ^(LinRandN numChan: numChan lo: lo hi: hi minmax: minmax) * mul + add!
  primaryFactoryMethod ^#numChan:lo:hi:minmax:!
!

UGen subclass: #RandN
  instanceVariableNames: 'lo hi'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

RandN comment: '(Undocumented class)'!

!RandN methodsFor: 'accessing'!
  lo ^lo!
  lo: x lo := x!
  hi ^hi!
  hi: x hi := x!
!

!RandN methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #RandN.
    paramNames := #(#lo #hi).
    mceNames := #().
    ugenRate := #ir.
    numChan := nil.
    specialIndex := 0.
    ugenId := nil.
    lo := 0.0. hi := 1.0!
!


!RandN class methodsFor: 'instance creation'!
  numChan: numChan lo: lo hi: hi
    |u|
    u := self new.
    u numChan: numChan.
    u lo: lo.
    u hi: hi.
    ^u!
  numChan: numChan lo: lo hi: hi mul: mul ^(RandN numChan: numChan lo: lo hi: hi) * mul!
  numChan: numChan lo: lo hi: hi mul: mul add: add ^(RandN numChan: numChan lo: lo hi: hi) * mul + add!
  primaryFactoryMethod ^#numChan:lo:hi:!
!

UGen subclass: #TScramble
  instanceVariableNames: 'trigger inputs'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

TScramble comment: '(Undocumented class)'!

!TScramble methodsFor: 'accessing'!
  trigger ^trigger!
  trigger: x trigger := x!
  inputs ^inputs!
  inputs: x inputs := x!
!

!TScramble methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #TScramble.
    paramNames := #(#trigger).
    mceNames := #(#inputs).
    ugenRate := #(0).
    numChan := #mceInput.
    specialIndex := 0.
    ugenId := nil.
    trigger := 0.0!
!


!TScramble class methodsFor: 'instance creation'!
  trigger: trigger inputs: inputs
    |u|
    u := self new.
    u trigger: trigger.
    u inputs: inputs.
    ^u!
  trigger: trigger inputs: inputs mul: mul ^(TScramble trigger: trigger inputs: inputs) * mul!
  trigger: trigger inputs: inputs mul: mul add: add ^(TScramble trigger: trigger inputs: inputs) * mul + add!
  primaryFactoryMethod ^#trigger:inputs:!
!

UGen subclass: #DX7
  instanceVariableNames: 'bufnum on off data vc mnn vel pw mw bc fc'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

DX7 comment: '(Undocumented class)'!

!DX7 methodsFor: 'accessing'!
  bufnum ^bufnum!
  bufnum: x bufnum := x!
  on ^on!
  on: x on := x!
  off ^off!
  off: x off := x!
  data ^data!
  data: x data := x!
  vc ^vc!
  vc: x vc := x!
  mnn ^mnn!
  mnn: x mnn := x!
  vel ^vel!
  vel: x vel := x!
  pw ^pw!
  pw: x pw := x!
  mw ^mw!
  mw: x mw := x!
  bc ^bc!
  bc: x bc := x!
  fc ^fc!
  fc: x fc := x!
!

!DX7 methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #DX7.
    paramNames := #(#bufnum #on #off #data #vc #mnn #vel #pw #mw #bc #fc).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    bufnum := 0.0. on := 0.0. off := 0.0. data := 0.0. vc := 0.0. mnn := 60.0. vel := 99.0. pw := 0.0. mw := 0.0. bc := 0.0. fc := 0.0!
!


!DX7 class methodsFor: 'instance creation'!
  bufnum: bufnum on: on off: off data: data vc: vc mnn: mnn vel: vel pw: pw mw: mw bc: bc fc: fc
    |u|
    u := self new.
    u bufnum: bufnum.
    u on: on.
    u off: off.
    u data: data.
    u vc: vc.
    u mnn: mnn.
    u vel: vel.
    u pw: pw.
    u mw: mw.
    u bc: bc.
    u fc: fc.
    ^u!
  bufnum: bufnum on: on off: off data: data vc: vc mnn: mnn vel: vel pw: pw mw: mw bc: bc fc: fc mul: mul ^(DX7 bufnum: bufnum on: on off: off data: data vc: vc mnn: mnn vel: vel pw: pw mw: mw bc: bc fc: fc) * mul!
  bufnum: bufnum on: on off: off data: data vc: vc mnn: mnn vel: vel pw: pw mw: mw bc: bc fc: fc mul: mul add: add ^(DX7 bufnum: bufnum on: on off: off data: data vc: vc mnn: mnn vel: vel pw: pw mw: mw bc: bc fc: fc) * mul + add!
  primaryFactoryMethod ^#bufnum:on:off:data:vc:mnn:vel:pw:mw:bc:fc:!
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
  gate: gate data: data r1: r1 r2: r2 r3: r3 r4: r4 l1: l1 l2: l2 l3: l3 l4: l4 ol: ol mul: mul ^(RDX7Env gate: gate data: data r1: r1 r2: r2 r3: r3 r4: r4 l1: l1 l2: l2 l3: l3 l4: l4 ol: ol) * mul!
  gate: gate data: data r1: r1 r2: r2 r3: r3 r4: r4 l1: l1 l2: l2 l3: l3 l4: l4 ol: ol mul: mul add: add ^(RDX7Env gate: gate data: data r1: r1 r2: r2 r3: r3 r4: r4 l1: l1 l2: l2 l3: l3 l4: l4 ol: ol) * mul + add!
  primaryFactoryMethod ^#gate:data:r1:r2:r3:r4:l1:l2:l3:l4:ol:!
!

UGen subclass: #ObxdFilter
  instanceVariableNames: 'in cutoff resonance multimode bandpass fourpole'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

ObxdFilter comment: '(Undocumented class)'!

!ObxdFilter methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  cutoff ^cutoff!
  cutoff: x cutoff := x!
  resonance ^resonance!
  resonance: x resonance := x!
  multimode ^multimode!
  multimode: x multimode := x!
  bandpass ^bandpass!
  bandpass: x bandpass := x!
  fourpole ^fourpole!
  fourpole: x fourpole := x!
!

!ObxdFilter methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #ObxdFilter.
    paramNames := #(#in #cutoff #resonance #multimode #bandpass #fourpole).
    mceNames := #().
    ugenRate := #(0).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. cutoff := 440.0. resonance := 0.0. multimode := 0.5. bandpass := 0.0. fourpole := 0.0!
!


!ObxdFilter class methodsFor: 'instance creation'!
  in: in cutoff: cutoff resonance: resonance multimode: multimode bandpass: bandpass fourpole: fourpole
    |u|
    u := self new.
    u in: in.
    u cutoff: cutoff.
    u resonance: resonance.
    u multimode: multimode.
    u bandpass: bandpass.
    u fourpole: fourpole.
    ^u!
  in: in cutoff: cutoff resonance: resonance multimode: multimode bandpass: bandpass fourpole: fourpole mul: mul ^(ObxdFilter in: in cutoff: cutoff resonance: resonance multimode: multimode bandpass: bandpass fourpole: fourpole) * mul!
  in: in cutoff: cutoff resonance: resonance multimode: multimode bandpass: bandpass fourpole: fourpole mul: mul add: add ^(ObxdFilter in: in cutoff: cutoff resonance: resonance multimode: multimode bandpass: bandpass fourpole: fourpole) * mul + add!
  primaryFactoryMethod ^#in:cutoff:resonance:multimode:bandpass:fourpole:!
!

UGen subclass: #SvfBp
  instanceVariableNames: 'in freq q'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

SvfBp comment: '(Undocumented class)'!

!SvfBp methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  freq ^freq!
  freq: x freq := x!
  q ^q!
  q: x q := x!
!

!SvfBp methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #SvfBp.
    paramNames := #(#in #freq #q).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. freq := 440.0. q := 0.0!
!


!SvfBp class methodsFor: 'instance creation'!
  in: in freq: freq q: q
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u q: q.
    ^u!
  in: in freq: freq q: q mul: mul ^(SvfBp in: in freq: freq q: q) * mul!
  in: in freq: freq q: q mul: mul add: add ^(SvfBp in: in freq: freq q: q) * mul + add!
  primaryFactoryMethod ^#in:freq:q:!
!

UGen subclass: #SvfHp
  instanceVariableNames: 'in freq q'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

SvfHp comment: '(Undocumented class)'!

!SvfHp methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  freq ^freq!
  freq: x freq := x!
  q ^q!
  q: x q := x!
!

!SvfHp methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #SvfHp.
    paramNames := #(#in #freq #q).
    mceNames := #().
    ugenRate := #(0).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. freq := 440.0. q := 0.0!
!


!SvfHp class methodsFor: 'instance creation'!
  in: in freq: freq q: q
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u q: q.
    ^u!
  in: in freq: freq q: q mul: mul ^(SvfHp in: in freq: freq q: q) * mul!
  in: in freq: freq q: q mul: mul add: add ^(SvfHp in: in freq: freq q: q) * mul + add!
  primaryFactoryMethod ^#in:freq:q:!
!

UGen subclass: #SvfLp
  instanceVariableNames: 'in freq q'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

SvfLp comment: '(Undocumented class)'!

!SvfLp methodsFor: 'accessing'!
  in ^in!
  in: x in := x!
  freq ^freq!
  freq: x freq := x!
  q ^q!
  q: x q := x!
!

!SvfLp methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #SvfLp.
    paramNames := #(#in #freq #q).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    in := 0.0. freq := 440.0. q := 0.0!
!


!SvfLp class methodsFor: 'instance creation'!
  in: in freq: freq q: q
    |u|
    u := self new.
    u in: in.
    u freq: freq.
    u q: q.
    ^u!
  in: in freq: freq q: q mul: mul ^(SvfLp in: in freq: freq q: q) * mul!
  in: in freq: freq q: q mul: mul add: add ^(SvfLp in: in freq: freq q: q) * mul + add!
  primaryFactoryMethod ^#in:freq:q:!
!

UGen subclass: #Bezier
  instanceVariableNames: 'haltAfter dx freq phase param'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Bezier comment: '(Undocumented class)'!

!Bezier methodsFor: 'accessing'!
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

!Bezier methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Bezier.
    paramNames := #(#haltAfter #dx #freq #phase).
    mceNames := #(#param).
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    haltAfter := 100.0. dx := 0.0001. freq := 440.0. phase := 0.0!
!


!Bezier class methodsFor: 'instance creation'!
  haltAfter: haltAfter dx: dx freq: freq phase: phase param: param
    |u|
    u := self new.
    u haltAfter: haltAfter.
    u dx: dx.
    u freq: freq.
    u phase: phase.
    u param: param.
    ^u!
  haltAfter: haltAfter dx: dx freq: freq phase: phase param: param mul: mul ^(Bezier haltAfter: haltAfter dx: dx freq: freq phase: phase param: param) * mul!
  haltAfter: haltAfter dx: dx freq: freq phase: phase param: param mul: mul add: add ^(Bezier haltAfter: haltAfter dx: dx freq: freq phase: phase param: param) * mul + add!
  primaryFactoryMethod ^#haltAfter:dx:freq:phase:param:!
!

UGen subclass: #Freezer
  instanceVariableNames: 'bufnum left right gain increment incrementOffset incrementRandom rightRandom syncPhaseTrigger randomizePhaseTrigger numberOfLoops'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Freezer comment: '(Undocumented class)'!

!Freezer methodsFor: 'accessing'!
  bufnum ^bufnum!
  bufnum: x bufnum := x!
  left ^left!
  left: x left := x!
  right ^right!
  right: x right := x!
  gain ^gain!
  gain: x gain := x!
  increment ^increment!
  increment: x increment := x!
  incrementOffset ^incrementOffset!
  incrementOffset: x incrementOffset := x!
  incrementRandom ^incrementRandom!
  incrementRandom: x incrementRandom := x!
  rightRandom ^rightRandom!
  rightRandom: x rightRandom := x!
  syncPhaseTrigger ^syncPhaseTrigger!
  syncPhaseTrigger: x syncPhaseTrigger := x!
  randomizePhaseTrigger ^randomizePhaseTrigger!
  randomizePhaseTrigger: x randomizePhaseTrigger := x!
  numberOfLoops ^numberOfLoops!
  numberOfLoops: x numberOfLoops := x!
!

!Freezer methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Freezer.
    paramNames := #(#bufnum #left #right #gain #increment #incrementOffset #incrementRandom #rightRandom #syncPhaseTrigger #randomizePhaseTrigger #numberOfLoops).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    bufnum := 0.0. left := 0.0. right := 1.0. gain := 1.0. increment := 1.0. incrementOffset := 0.0. incrementRandom := 0.0. rightRandom := 0.0. syncPhaseTrigger := 0.0. randomizePhaseTrigger := 0.0. numberOfLoops := 4.0!
!


!Freezer class methodsFor: 'instance creation'!
  bufnum: bufnum left: left right: right gain: gain increment: increment incrementOffset: incrementOffset incrementRandom: incrementRandom rightRandom: rightRandom syncPhaseTrigger: syncPhaseTrigger randomizePhaseTrigger: randomizePhaseTrigger numberOfLoops: numberOfLoops
    |u|
    u := self new.
    u bufnum: bufnum.
    u left: left.
    u right: right.
    u gain: gain.
    u increment: increment.
    u incrementOffset: incrementOffset.
    u incrementRandom: incrementRandom.
    u rightRandom: rightRandom.
    u syncPhaseTrigger: syncPhaseTrigger.
    u randomizePhaseTrigger: randomizePhaseTrigger.
    u numberOfLoops: numberOfLoops.
    ^u!
  bufnum: bufnum left: left right: right gain: gain increment: increment incrementOffset: incrementOffset incrementRandom: incrementRandom rightRandom: rightRandom syncPhaseTrigger: syncPhaseTrigger randomizePhaseTrigger: randomizePhaseTrigger numberOfLoops: numberOfLoops mul: mul ^(Freezer bufnum: bufnum left: left right: right gain: gain increment: increment incrementOffset: incrementOffset incrementRandom: incrementRandom rightRandom: rightRandom syncPhaseTrigger: syncPhaseTrigger randomizePhaseTrigger: randomizePhaseTrigger numberOfLoops: numberOfLoops) * mul!
  bufnum: bufnum left: left right: right gain: gain increment: increment incrementOffset: incrementOffset incrementRandom: incrementRandom rightRandom: rightRandom syncPhaseTrigger: syncPhaseTrigger randomizePhaseTrigger: randomizePhaseTrigger numberOfLoops: numberOfLoops mul: mul add: add ^(Freezer bufnum: bufnum left: left right: right gain: gain increment: increment incrementOffset: incrementOffset incrementRandom: incrementRandom rightRandom: rightRandom syncPhaseTrigger: syncPhaseTrigger randomizePhaseTrigger: randomizePhaseTrigger numberOfLoops: numberOfLoops) * mul + add!
  primaryFactoryMethod ^#bufnum:left:right:gain:increment:incrementOffset:incrementRandom:rightRandom:syncPhaseTrigger:randomizePhaseTrigger:numberOfLoops:!
!

UGen subclass: #ShufflerB
  instanceVariableNames: 'bufnum readLocationMinima readLocationMaxima readIncrementMinima readIncrementMaxima durationMinima durationMaxima envelopeAmplitudeMinima envelopeAmplitudeMaxima envelopeShapeMinima envelopeShapeMaxima envelopeSkewMinima envelopeSkewMaxima stereoLocationMinima stereoLocationMaxima interOffsetTimeMinima interOffsetTimeMaxima ftableReadLocationIncrement readIncrementQuanta interOffsetTimeQuanta'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

ShufflerB comment: '(Undocumented class)'!

!ShufflerB methodsFor: 'accessing'!
  bufnum ^bufnum!
  bufnum: x bufnum := x!
  readLocationMinima ^readLocationMinima!
  readLocationMinima: x readLocationMinima := x!
  readLocationMaxima ^readLocationMaxima!
  readLocationMaxima: x readLocationMaxima := x!
  readIncrementMinima ^readIncrementMinima!
  readIncrementMinima: x readIncrementMinima := x!
  readIncrementMaxima ^readIncrementMaxima!
  readIncrementMaxima: x readIncrementMaxima := x!
  durationMinima ^durationMinima!
  durationMinima: x durationMinima := x!
  durationMaxima ^durationMaxima!
  durationMaxima: x durationMaxima := x!
  envelopeAmplitudeMinima ^envelopeAmplitudeMinima!
  envelopeAmplitudeMinima: x envelopeAmplitudeMinima := x!
  envelopeAmplitudeMaxima ^envelopeAmplitudeMaxima!
  envelopeAmplitudeMaxima: x envelopeAmplitudeMaxima := x!
  envelopeShapeMinima ^envelopeShapeMinima!
  envelopeShapeMinima: x envelopeShapeMinima := x!
  envelopeShapeMaxima ^envelopeShapeMaxima!
  envelopeShapeMaxima: x envelopeShapeMaxima := x!
  envelopeSkewMinima ^envelopeSkewMinima!
  envelopeSkewMinima: x envelopeSkewMinima := x!
  envelopeSkewMaxima ^envelopeSkewMaxima!
  envelopeSkewMaxima: x envelopeSkewMaxima := x!
  stereoLocationMinima ^stereoLocationMinima!
  stereoLocationMinima: x stereoLocationMinima := x!
  stereoLocationMaxima ^stereoLocationMaxima!
  stereoLocationMaxima: x stereoLocationMaxima := x!
  interOffsetTimeMinima ^interOffsetTimeMinima!
  interOffsetTimeMinima: x interOffsetTimeMinima := x!
  interOffsetTimeMaxima ^interOffsetTimeMaxima!
  interOffsetTimeMaxima: x interOffsetTimeMaxima := x!
  ftableReadLocationIncrement ^ftableReadLocationIncrement!
  ftableReadLocationIncrement: x ftableReadLocationIncrement := x!
  readIncrementQuanta ^readIncrementQuanta!
  readIncrementQuanta: x readIncrementQuanta := x!
  interOffsetTimeQuanta ^interOffsetTimeQuanta!
  interOffsetTimeQuanta: x interOffsetTimeQuanta := x!
!

!ShufflerB methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #ShufflerB.
    paramNames := #(#bufnum #readLocationMinima #readLocationMaxima #readIncrementMinima #readIncrementMaxima #durationMinima #durationMaxima #envelopeAmplitudeMinima #envelopeAmplitudeMaxima #envelopeShapeMinima #envelopeShapeMaxima #envelopeSkewMinima #envelopeSkewMaxima #stereoLocationMinima #stereoLocationMaxima #interOffsetTimeMinima #interOffsetTimeMaxima #ftableReadLocationIncrement #readIncrementQuanta #interOffsetTimeQuanta).
    mceNames := #().
    ugenRate := #ar.
    numChan := 2.
    specialIndex := 0.
    ugenId := nil.
    bufnum := 0.0. readLocationMinima := 0.01. readLocationMaxima := 0.02. readIncrementMinima := 1.0. readIncrementMaxima := 1.0. durationMinima := 0.2. durationMaxima := 0.2. envelopeAmplitudeMinima := 0.5. envelopeAmplitudeMaxima := 0.5. envelopeShapeMinima := 0.5. envelopeShapeMaxima := 0.5. envelopeSkewMinima := 0.5. envelopeSkewMaxima := 0.5. stereoLocationMinima := 0.5. stereoLocationMaxima := 0.5. interOffsetTimeMinima := 0.05. interOffsetTimeMaxima := 0.01. ftableReadLocationIncrement := 1.0. readIncrementQuanta := 0.0. interOffsetTimeQuanta := 0.0!
!


!ShufflerB class methodsFor: 'instance creation'!
  bufnum: bufnum readLocationMinima: readLocationMinima readLocationMaxima: readLocationMaxima readIncrementMinima: readIncrementMinima readIncrementMaxima: readIncrementMaxima durationMinima: durationMinima durationMaxima: durationMaxima envelopeAmplitudeMinima: envelopeAmplitudeMinima envelopeAmplitudeMaxima: envelopeAmplitudeMaxima envelopeShapeMinima: envelopeShapeMinima envelopeShapeMaxima: envelopeShapeMaxima envelopeSkewMinima: envelopeSkewMinima envelopeSkewMaxima: envelopeSkewMaxima stereoLocationMinima: stereoLocationMinima stereoLocationMaxima: stereoLocationMaxima interOffsetTimeMinima: interOffsetTimeMinima interOffsetTimeMaxima: interOffsetTimeMaxima ftableReadLocationIncrement: ftableReadLocationIncrement readIncrementQuanta: readIncrementQuanta interOffsetTimeQuanta: interOffsetTimeQuanta
    |u|
    u := self new.
    u bufnum: bufnum.
    u readLocationMinima: readLocationMinima.
    u readLocationMaxima: readLocationMaxima.
    u readIncrementMinima: readIncrementMinima.
    u readIncrementMaxima: readIncrementMaxima.
    u durationMinima: durationMinima.
    u durationMaxima: durationMaxima.
    u envelopeAmplitudeMinima: envelopeAmplitudeMinima.
    u envelopeAmplitudeMaxima: envelopeAmplitudeMaxima.
    u envelopeShapeMinima: envelopeShapeMinima.
    u envelopeShapeMaxima: envelopeShapeMaxima.
    u envelopeSkewMinima: envelopeSkewMinima.
    u envelopeSkewMaxima: envelopeSkewMaxima.
    u stereoLocationMinima: stereoLocationMinima.
    u stereoLocationMaxima: stereoLocationMaxima.
    u interOffsetTimeMinima: interOffsetTimeMinima.
    u interOffsetTimeMaxima: interOffsetTimeMaxima.
    u ftableReadLocationIncrement: ftableReadLocationIncrement.
    u readIncrementQuanta: readIncrementQuanta.
    u interOffsetTimeQuanta: interOffsetTimeQuanta.
    ^u!
  bufnum: bufnum readLocationMinima: readLocationMinima readLocationMaxima: readLocationMaxima readIncrementMinima: readIncrementMinima readIncrementMaxima: readIncrementMaxima durationMinima: durationMinima durationMaxima: durationMaxima envelopeAmplitudeMinima: envelopeAmplitudeMinima envelopeAmplitudeMaxima: envelopeAmplitudeMaxima envelopeShapeMinima: envelopeShapeMinima envelopeShapeMaxima: envelopeShapeMaxima envelopeSkewMinima: envelopeSkewMinima envelopeSkewMaxima: envelopeSkewMaxima stereoLocationMinima: stereoLocationMinima stereoLocationMaxima: stereoLocationMaxima interOffsetTimeMinima: interOffsetTimeMinima interOffsetTimeMaxima: interOffsetTimeMaxima ftableReadLocationIncrement: ftableReadLocationIncrement readIncrementQuanta: readIncrementQuanta interOffsetTimeQuanta: interOffsetTimeQuanta mul: mul ^(ShufflerB bufnum: bufnum readLocationMinima: readLocationMinima readLocationMaxima: readLocationMaxima readIncrementMinima: readIncrementMinima readIncrementMaxima: readIncrementMaxima durationMinima: durationMinima durationMaxima: durationMaxima envelopeAmplitudeMinima: envelopeAmplitudeMinima envelopeAmplitudeMaxima: envelopeAmplitudeMaxima envelopeShapeMinima: envelopeShapeMinima envelopeShapeMaxima: envelopeShapeMaxima envelopeSkewMinima: envelopeSkewMinima envelopeSkewMaxima: envelopeSkewMaxima stereoLocationMinima: stereoLocationMinima stereoLocationMaxima: stereoLocationMaxima interOffsetTimeMinima: interOffsetTimeMinima interOffsetTimeMaxima: interOffsetTimeMaxima ftableReadLocationIncrement: ftableReadLocationIncrement readIncrementQuanta: readIncrementQuanta interOffsetTimeQuanta: interOffsetTimeQuanta) * mul!
  bufnum: bufnum readLocationMinima: readLocationMinima readLocationMaxima: readLocationMaxima readIncrementMinima: readIncrementMinima readIncrementMaxima: readIncrementMaxima durationMinima: durationMinima durationMaxima: durationMaxima envelopeAmplitudeMinima: envelopeAmplitudeMinima envelopeAmplitudeMaxima: envelopeAmplitudeMaxima envelopeShapeMinima: envelopeShapeMinima envelopeShapeMaxima: envelopeShapeMaxima envelopeSkewMinima: envelopeSkewMinima envelopeSkewMaxima: envelopeSkewMaxima stereoLocationMinima: stereoLocationMinima stereoLocationMaxima: stereoLocationMaxima interOffsetTimeMinima: interOffsetTimeMinima interOffsetTimeMaxima: interOffsetTimeMaxima ftableReadLocationIncrement: ftableReadLocationIncrement readIncrementQuanta: readIncrementQuanta interOffsetTimeQuanta: interOffsetTimeQuanta mul: mul add: add ^(ShufflerB bufnum: bufnum readLocationMinima: readLocationMinima readLocationMaxima: readLocationMaxima readIncrementMinima: readIncrementMinima readIncrementMaxima: readIncrementMaxima durationMinima: durationMinima durationMaxima: durationMaxima envelopeAmplitudeMinima: envelopeAmplitudeMinima envelopeAmplitudeMaxima: envelopeAmplitudeMaxima envelopeShapeMinima: envelopeShapeMinima envelopeShapeMaxima: envelopeShapeMaxima envelopeSkewMinima: envelopeSkewMinima envelopeSkewMaxima: envelopeSkewMaxima stereoLocationMinima: stereoLocationMinima stereoLocationMaxima: stereoLocationMaxima interOffsetTimeMinima: interOffsetTimeMinima interOffsetTimeMaxima: interOffsetTimeMaxima ftableReadLocationIncrement: ftableReadLocationIncrement readIncrementQuanta: readIncrementQuanta interOffsetTimeQuanta: interOffsetTimeQuanta) * mul + add!
  primaryFactoryMethod ^#bufnum:readLocationMinima:readLocationMaxima:readIncrementMinima:readIncrementMaxima:durationMinima:durationMaxima:envelopeAmplitudeMinima:envelopeAmplitudeMaxima:envelopeShapeMinima:envelopeShapeMaxima:envelopeSkewMinima:envelopeSkewMaxima:stereoLocationMinima:stereoLocationMaxima:interOffsetTimeMinima:interOffsetTimeMaxima:ftableReadLocationIncrement:readIncrementQuanta:interOffsetTimeQuanta:!
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
  roundTo: b ^ BinaryOpUGen specialIndex: 19 a: self b: b!
  truncateTo: b ^ BinaryOpUGen specialIndex: 21 a: self b: b!
  raisedTo: b ^ BinaryOpUGen specialIndex: 25 a: self b: b!
  bitShiftLeft: b ^ BinaryOpUGen specialIndex: 26 a: self b: b!
  bitShiftRight: b ^ BinaryOpUGen specialIndex: 27 a: self b: b!
  amClip: b ^ BinaryOpUGen specialIndex: 40 a: self b: b!
  clip2: b ^ BinaryOpUGen specialIndex: 42 a: self b: b!
  fold2: b ^ BinaryOpUGen specialIndex: 44 a: self b: b!
!

!AbstractUGen methodsFor: 'arithmetic'!
  negated ^ UnaryOpUGen specialIndex: 0 a: self!
  abs ^ UnaryOpUGen specialIndex: 5 a: self!
  ceil ^ UnaryOpUGen specialIndex: 8 a: self!
  floor ^ UnaryOpUGen specialIndex: 9 a: self!
  frac ^ UnaryOpUGen specialIndex: 10 a: self!
  sign ^ UnaryOpUGen specialIndex: 11 a: self!
  squared ^ UnaryOpUGen specialIndex: 12 a: self!
  cubed ^ UnaryOpUGen specialIndex: 13 a: self!
  sqrt ^ UnaryOpUGen specialIndex: 14 a: self!
  exp ^ UnaryOpUGen specialIndex: 15 a: self!
  reciprocal ^ UnaryOpUGen specialIndex: 16 a: self!
  midiCps ^ UnaryOpUGen specialIndex: 17 a: self!
  cpsMidi ^ UnaryOpUGen specialIndex: 18 a: self!
  midiRatio ^ UnaryOpUGen specialIndex: 19 a: self!
  ratioMidi ^ UnaryOpUGen specialIndex: 20 a: self!
  dbAmp ^ UnaryOpUGen specialIndex: 21 a: self!
  ampDb ^ UnaryOpUGen specialIndex: 22 a: self!
  ln ^ UnaryOpUGen specialIndex: 25 a: self!
  log2 ^ UnaryOpUGen specialIndex: 26 a: self!
  log ^ UnaryOpUGen specialIndex: 27 a: self!
  sin ^ UnaryOpUGen specialIndex: 28 a: self!
  cos ^ UnaryOpUGen specialIndex: 29 a: self!
  tanh ^ UnaryOpUGen specialIndex: 36 a: self!
  distort ^ UnaryOpUGen specialIndex: 42 a: self!
  softClip ^ UnaryOpUGen specialIndex: 43 a: self!
!

