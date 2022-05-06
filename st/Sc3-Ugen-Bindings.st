PrimitiveUgen subclass: #AllpassC
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

AllpassC comment: 'Schroeder allpass delay line with cubic interpolation.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!AllpassC class methodsFor: 'instance creation'!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
    | anAllpassC |
    anAllpassC := self new.
    anAllpassC paramValues: {in. maxdelaytime. delaytime. decaytime}.
    ^anAllpassC
!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul
    ^(AllpassC in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul
!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add
    ^(AllpassC in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul + add
!
primaryFactoryMethod
    ^#in:maxdelaytime:delaytime:decaytime:
!
!

PrimitiveUgen subclass: #AllpassL
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

AllpassL comment: 'Schroeder allpass delay line with linear interpolation.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!AllpassL class methodsFor: 'instance creation'!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
    | anAllpassL |
    anAllpassL := self new.
    anAllpassL paramValues: {in. maxdelaytime. delaytime. decaytime}.
    ^anAllpassL
!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul
    ^(AllpassL in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul
!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add
    ^(AllpassL in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul + add
!
primaryFactoryMethod
    ^#in:maxdelaytime:delaytime:decaytime:
!
!

PrimitiveUgen subclass: #AllpassN
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

AllpassN comment: 'Schroeder allpass delay line with no interpolation.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!AllpassN class methodsFor: 'instance creation'!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
    | anAllpassN |
    anAllpassN := self new.
    anAllpassN paramValues: {in. maxdelaytime. delaytime. decaytime}.
    ^anAllpassN
!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul
    ^(AllpassN in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul
!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add
    ^(AllpassN in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul + add
!
primaryFactoryMethod
    ^#in:maxdelaytime:delaytime:decaytime:
!
!

PrimitiveUgen subclass: #AmpComp
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

AmpComp comment: 'Basic psychoacoustic amplitude compensation.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!AmpComp class methodsFor: 'instance creation'!
freq: freq root: root exp: exp
    | anAmpComp |
    anAmpComp := self new.
    anAmpComp paramValues: {freq. root. exp}.
    ^anAmpComp
!
freq: freq root: root exp: exp mul: mul
    ^(AmpComp freq: freq root: root exp: exp) * mul
!
freq: freq root: root exp: exp mul: mul add: add
    ^(AmpComp freq: freq root: root exp: exp) * mul + add
!
primaryFactoryMethod
    ^#freq:root:exp:
!
!

PrimitiveUgen subclass: #AmpCompA
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

AmpCompA comment: 'Basic psychoacoustic amplitude compensation (ANSI A-weighting curve).'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!AmpCompA class methodsFor: 'instance creation'!
freq: freq root: root minAmp: minAmp rootAmp: rootAmp
    | anAmpCompA |
    anAmpCompA := self new.
    anAmpCompA paramValues: {freq. root. minAmp. rootAmp}.
    ^anAmpCompA
!
freq: freq root: root minAmp: minAmp rootAmp: rootAmp mul: mul
    ^(AmpCompA freq: freq root: root minAmp: minAmp rootAmp: rootAmp) * mul
!
freq: freq root: root minAmp: minAmp rootAmp: rootAmp mul: mul add: add
    ^(AmpCompA freq: freq root: root minAmp: minAmp rootAmp: rootAmp) * mul + add
!
primaryFactoryMethod
    ^#freq:root:minAmp:rootAmp:
!
!

PrimitiveUgen subclass: #Amplitude
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Amplitude comment: 'Amplitude follower'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Amplitude class methodsFor: 'instance creation'!
in: in attackTime: attackTime releaseTime: releaseTime
    | anAmplitude |
    anAmplitude := self new.
    anAmplitude paramValues: {in. attackTime. releaseTime}.
    ^anAmplitude
!
in: in attackTime: attackTime releaseTime: releaseTime mul: mul
    ^(Amplitude in: in attackTime: attackTime releaseTime: releaseTime) * mul
!
in: in attackTime: attackTime releaseTime: releaseTime mul: mul add: add
    ^(Amplitude in: in attackTime: attackTime releaseTime: releaseTime) * mul + add
!
primaryFactoryMethod
    ^#in:attackTime:releaseTime:
!
!

PrimitiveUgen subclass: #Balance2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Balance2 comment: 'Stereo signal balancer'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Balance2 class methodsFor: 'instance creation'!
left: left right: right pos: pos level: level
    | aBalance2 |
    aBalance2 := self new.
    aBalance2 paramValues: {left. right. pos. level}.
    ^aBalance2
!
left: left right: right pos: pos level: level mul: mul
    ^(Balance2 left: left right: right pos: pos level: level) * mul
!
left: left right: right pos: pos level: level mul: mul add: add
    ^(Balance2 left: left right: right pos: pos level: level) * mul + add
!
primaryFactoryMethod
    ^#left:right:pos:level:
!
!

PrimitiveUgen subclass: #BBandPass
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BBandPass comment: 'Band Pass Filter'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!BBandPass class methodsFor: 'instance creation'!
in: in freq: freq bw: bw
    | aBBandPass |
    aBBandPass := self new.
    aBBandPass paramValues: {in. freq. bw}.
    ^aBBandPass
!
in: in freq: freq bw: bw mul: mul
    ^(BBandPass in: in freq: freq bw: bw) * mul
!
in: in freq: freq bw: bw mul: mul add: add
    ^(BBandPass in: in freq: freq bw: bw) * mul + add
!
primaryFactoryMethod
    ^#in:freq:bw:
!
!

PrimitiveUgen subclass: #BBandStop
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BBandStop comment: 'Band reject filter'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!BBandStop class methodsFor: 'instance creation'!
in: in freq: freq bw: bw
    | aBBandStop |
    aBBandStop := self new.
    aBBandStop paramValues: {in. freq. bw}.
    ^aBBandStop
!
in: in freq: freq bw: bw mul: mul
    ^(BBandStop in: in freq: freq bw: bw) * mul
!
in: in freq: freq bw: bw mul: mul add: add
    ^(BBandStop in: in freq: freq bw: bw) * mul + add
!
primaryFactoryMethod
    ^#in:freq:bw:
!
!

PrimitiveUgen subclass: #Blip
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Blip comment: 'Band limited impulse oscillator.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Blip class methodsFor: 'instance creation'!
freq: freq numharm: numharm
    | aBlip |
    aBlip := self new.
    aBlip paramValues: {freq. numharm}.
    ^aBlip
!
freq: freq numharm: numharm mul: mul
    ^(Blip freq: freq numharm: numharm) * mul
!
freq: freq numharm: numharm mul: mul add: add
    ^(Blip freq: freq numharm: numharm) * mul + add
!
primaryFactoryMethod
    ^#freq:numharm:
!
!

PrimitiveUgen subclass: #BlockSize
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

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
    paramValues := {}.
    mceValues := {}.
!
!


!BlockSize class methodsFor: 'instance creation'!
mul: mul
    ^(BlockSize new) * mul
!
mul: mul add: add
    ^(BlockSize new) * mul + add
!
primaryFactoryMethod
    ^#new
!
!

PrimitiveUgen subclass: #BLowPass
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BLowPass comment: '12db/oct rolloff - 2nd order resonant Low Pass Filter'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!BLowPass class methodsFor: 'instance creation'!
in: in freq: freq rq: rq
    | aBLowPass |
    aBLowPass := self new.
    aBLowPass paramValues: {in. freq. rq}.
    ^aBLowPass
!
in: in freq: freq rq: rq mul: mul
    ^(BLowPass in: in freq: freq rq: rq) * mul
!
in: in freq: freq rq: rq mul: mul add: add
    ^(BLowPass in: in freq: freq rq: rq) * mul + add
!
primaryFactoryMethod
    ^#in:freq:rq:
!
!

PrimitiveUgen subclass: #BPeakEQ
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BPeakEQ comment: 'Parametric equalizer'!


!BPeakEQ methodsFor: 'initialization'!
initialize
    super initialize.
    ugenName := #BPeakEQ.
    paramNames := #(#in #freq #rq #db).
    mceNames := #().
    ugenRate := #(0).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    paramValues := {}.
    mceValues := {}.
!
!


!BPeakEQ class methodsFor: 'instance creation'!
in: in freq: freq rq: rq db: db
    | aBPeakEQ |
    aBPeakEQ := self new.
    aBPeakEQ paramValues: {in. freq. rq. db}.
    ^aBPeakEQ
!
in: in freq: freq rq: rq db: db mul: mul
    ^(BPeakEQ in: in freq: freq rq: rq db: db) * mul
!
in: in freq: freq rq: rq db: db mul: mul add: add
    ^(BPeakEQ in: in freq: freq rq: rq db: db) * mul + add
!
primaryFactoryMethod
    ^#in:freq:rq:db:
!
!

PrimitiveUgen subclass: #BPF
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BPF comment: '2nd order Butterworth bandpass filter.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!BPF class methodsFor: 'instance creation'!
in: in freq: freq rq: rq
    | aBPF |
    aBPF := self new.
    aBPF paramValues: {in. freq. rq}.
    ^aBPF
!
in: in freq: freq rq: rq mul: mul
    ^(BPF in: in freq: freq rq: rq) * mul
!
in: in freq: freq rq: rq mul: mul add: add
    ^(BPF in: in freq: freq rq: rq) * mul + add
!
primaryFactoryMethod
    ^#in:freq:rq:
!
!

PrimitiveUgen subclass: #BPZ2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BPZ2 comment: 'Two zero fixed midpass.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!BPZ2 class methodsFor: 'instance creation'!
in: in
    | aBPZ2 |
    aBPZ2 := self new.
    aBPZ2 paramValues: {in}.
    ^aBPZ2
!
in: in mul: mul
    ^(BPZ2 in: in) * mul
!
in: in mul: mul add: add
    ^(BPZ2 in: in) * mul + add
!
primaryFactoryMethod
    ^#in:
!
!

PrimitiveUgen subclass: #BRF
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BRF comment: '2nd order Butterworth band reject filter.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!BRF class methodsFor: 'instance creation'!
in: in freq: freq rq: rq
    | aBRF |
    aBRF := self new.
    aBRF paramValues: {in. freq. rq}.
    ^aBRF
!
in: in freq: freq rq: rq mul: mul
    ^(BRF in: in freq: freq rq: rq) * mul
!
in: in freq: freq rq: rq mul: mul add: add
    ^(BRF in: in freq: freq rq: rq) * mul + add
!
primaryFactoryMethod
    ^#in:freq:rq:
!
!

PrimitiveUgen subclass: #BRZ2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BRZ2 comment: 'Two zero fixed midcut.'!


!BRZ2 methodsFor: 'initialization'!
initialize
    super initialize.
    ugenName := #BRZ2.
    paramNames := #(#in).
    mceNames := #().
    ugenRate := #(0).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    paramValues := {}.
    mceValues := {}.
!
!


!BRZ2 class methodsFor: 'instance creation'!
in: in
    | aBRZ2 |
    aBRZ2 := self new.
    aBRZ2 paramValues: {in}.
    ^aBRZ2
!
in: in mul: mul
    ^(BRZ2 in: in) * mul
!
in: in mul: mul add: add
    ^(BRZ2 in: in) * mul + add
!
primaryFactoryMethod
    ^#in:
!
!

PrimitiveUgen subclass: #BrownNoise
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

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
    paramValues := {}.
    mceValues := {}.
!
!


!BrownNoise class methodsFor: 'instance creation'!
mul: mul
    ^(BrownNoise new) * mul
!
mul: mul add: add
    ^(BrownNoise new) * mul + add
!
primaryFactoryMethod
    ^#new
!
!

PrimitiveUgen subclass: #BufDur
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BufDur comment: 'Current duration of soundfile in buffer.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!BufDur class methodsFor: 'instance creation'!
bufnum: bufnum
    | aBufDur |
    aBufDur := self new.
    aBufDur paramValues: {bufnum}.
    ^aBufDur
!
bufnum: bufnum mul: mul
    ^(BufDur bufnum: bufnum) * mul
!
bufnum: bufnum mul: mul add: add
    ^(BufDur bufnum: bufnum) * mul + add
!
primaryFactoryMethod
    ^#bufnum:
!
!

PrimitiveUgen subclass: #BufFrames
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BufFrames comment: 'Current number of frames allocated in the buffer.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!BufFrames class methodsFor: 'instance creation'!
bufnum: bufnum
    | aBufFrames |
    aBufFrames := self new.
    aBufFrames paramValues: {bufnum}.
    ^aBufFrames
!
bufnum: bufnum mul: mul
    ^(BufFrames bufnum: bufnum) * mul
!
bufnum: bufnum mul: mul add: add
    ^(BufFrames bufnum: bufnum) * mul + add
!
primaryFactoryMethod
    ^#bufnum:
!
!

PrimitiveUgen subclass: #BufRateScale
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BufRateScale comment: 'Buffer rate scaling in respect to server samplerate.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!BufRateScale class methodsFor: 'instance creation'!
bufnum: bufnum
    | aBufRateScale |
    aBufRateScale := self new.
    aBufRateScale paramValues: {bufnum}.
    ^aBufRateScale
!
bufnum: bufnum mul: mul
    ^(BufRateScale bufnum: bufnum) * mul
!
bufnum: bufnum mul: mul add: add
    ^(BufRateScale bufnum: bufnum) * mul + add
!
primaryFactoryMethod
    ^#bufnum:
!
!

PrimitiveUgen subclass: #BufRd
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BufRd comment: 'Buffer reading oscillator.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!BufRd class methodsFor: 'instance creation'!
numChan: numChan bufnum: bufnum phase: phase loop: loop interpolation: interpolation
    | aBufRd |
    aBufRd := self new.
    aBufRd numChan: numChan.
    aBufRd paramValues: {bufnum. phase. loop. interpolation}.
    ^aBufRd
!
numChan: numChan bufnum: bufnum phase: phase loop: loop interpolation: interpolation mul: mul
    ^(BufRd numChan: numChan bufnum: bufnum phase: phase loop: loop interpolation: interpolation) * mul
!
numChan: numChan bufnum: bufnum phase: phase loop: loop interpolation: interpolation mul: mul add: add
    ^(BufRd numChan: numChan bufnum: bufnum phase: phase loop: loop interpolation: interpolation) * mul + add
!
primaryFactoryMethod
    ^#numChan:bufnum:phase:loop:interpolation:
!
!

PrimitiveUgen subclass: #BufSampleRate
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BufSampleRate comment: 'Buffer sample rate.'!


!BufSampleRate methodsFor: 'initialization'!
initialize
    super initialize.
    ugenName := #BufSampleRate.
    paramNames := #(#bufnum).
    mceNames := #().
    ugenRate := #kr.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    paramValues := {}.
    mceValues := {}.
!
!


!BufSampleRate class methodsFor: 'instance creation'!
bufnum: bufnum
    | aBufSampleRate |
    aBufSampleRate := self new.
    aBufSampleRate paramValues: {bufnum}.
    ^aBufSampleRate
!
bufnum: bufnum mul: mul
    ^(BufSampleRate bufnum: bufnum) * mul
!
bufnum: bufnum mul: mul add: add
    ^(BufSampleRate bufnum: bufnum) * mul + add
!
primaryFactoryMethod
    ^#bufnum:
!
!

PrimitiveUgen subclass: #BufWr
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BufWr comment: 'Buffer writing oscillator.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!BufWr class methodsFor: 'instance creation'!
bufnum: bufnum phase: phase loop: loop inputArray: inputArray
    | aBufWr |
    aBufWr := self new.
    aBufWr paramValues: {bufnum. phase. loop}.
    aBufWr mceValues: {inputArray}.
    ^aBufWr
!
bufnum: bufnum phase: phase loop: loop inputArray: inputArray mul: mul
    ^(BufWr bufnum: bufnum phase: phase loop: loop inputArray: inputArray) * mul
!
bufnum: bufnum phase: phase loop: loop inputArray: inputArray mul: mul add: add
    ^(BufWr bufnum: bufnum phase: phase loop: loop inputArray: inputArray) * mul + add
!
primaryFactoryMethod
    ^#bufnum:phase:loop:inputArray:
!
!

PrimitiveUgen subclass: #ClearBuf
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

ClearBuf comment: '(Undocumented class)'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!ClearBuf class methodsFor: 'instance creation'!
buf: buf
    | aClearBuf |
    aClearBuf := self new.
    aClearBuf paramValues: {buf}.
    ^aClearBuf
!
buf: buf mul: mul
    ^(ClearBuf buf: buf) * mul
!
buf: buf mul: mul add: add
    ^(ClearBuf buf: buf) * mul + add
!
primaryFactoryMethod
    ^#buf:
!
!

PrimitiveUgen subclass: #Clip
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Clip comment: 'Clip a signal outside given thresholds.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Clip class methodsFor: 'instance creation'!
in: in lo: lo hi: hi
    | aClip |
    aClip := self new.
    aClip paramValues: {in. lo. hi}.
    ^aClip
!
in: in lo: lo hi: hi mul: mul
    ^(Clip in: in lo: lo hi: hi) * mul
!
in: in lo: lo hi: hi mul: mul add: add
    ^(Clip in: in lo: lo hi: hi) * mul + add
!
primaryFactoryMethod
    ^#in:lo:hi:
!
!

PrimitiveUgen subclass: #ClipNoise
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

ClipNoise comment: 'Clip Noise.'!


!ClipNoise methodsFor: 'initialization'!
initialize
    super initialize.
    ugenName := #ClipNoise.
    paramNames := #().
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    paramValues := {}.
    mceValues := {}.
!
!


!ClipNoise class methodsFor: 'instance creation'!
mul: mul
    ^(ClipNoise new) * mul
!
mul: mul add: add
    ^(ClipNoise new) * mul + add
!
primaryFactoryMethod
    ^#new
!
!

PrimitiveUgen subclass: #CoinGate
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

CoinGate comment: 'Statistical gate.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!CoinGate class methodsFor: 'instance creation'!
prob: prob in: in
    | aCoinGate |
    aCoinGate := self new.
    aCoinGate paramValues: {prob. in}.
    ^aCoinGate
!
prob: prob in: in mul: mul
    ^(CoinGate prob: prob in: in) * mul
!
prob: prob in: in mul: mul add: add
    ^(CoinGate prob: prob in: in) * mul + add
!
primaryFactoryMethod
    ^#prob:in:
!
!

PrimitiveUgen subclass: #CombC
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

CombC comment: 'Comb delay line with cubic interpolation.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!CombC class methodsFor: 'instance creation'!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
    | aCombC |
    aCombC := self new.
    aCombC paramValues: {in. maxdelaytime. delaytime. decaytime}.
    ^aCombC
!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul
    ^(CombC in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul
!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add
    ^(CombC in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul + add
!
primaryFactoryMethod
    ^#in:maxdelaytime:delaytime:decaytime:
!
!

PrimitiveUgen subclass: #CombL
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

CombL comment: 'Comb delay line with linear interpolation.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!CombL class methodsFor: 'instance creation'!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
    | aCombL |
    aCombL := self new.
    aCombL paramValues: {in. maxdelaytime. delaytime. decaytime}.
    ^aCombL
!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul
    ^(CombL in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul
!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add
    ^(CombL in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul + add
!
primaryFactoryMethod
    ^#in:maxdelaytime:delaytime:decaytime:
!
!

PrimitiveUgen subclass: #CombN
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

CombN comment: 'Comb delay line with no interpolation.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!CombN class methodsFor: 'instance creation'!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
    | aCombN |
    aCombN := self new.
    aCombN paramValues: {in. maxdelaytime. delaytime. decaytime}.
    ^aCombN
!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul
    ^(CombN in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul
!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add
    ^(CombN in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul + add
!
primaryFactoryMethod
    ^#in:maxdelaytime:delaytime:decaytime:
!
!

PrimitiveUgen subclass: #ControlDur
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

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
    paramValues := {}.
    mceValues := {}.
!
!


!ControlDur class methodsFor: 'instance creation'!
mul: mul
    ^(ControlDur new) * mul
!
mul: mul add: add
    ^(ControlDur new) * mul + add
!
primaryFactoryMethod
    ^#new
!
!

PrimitiveUgen subclass: #ControlRate
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

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
    paramValues := {}.
    mceValues := {}.
!
!


!ControlRate class methodsFor: 'instance creation'!
mul: mul
    ^(ControlRate new) * mul
!
mul: mul add: add
    ^(ControlRate new) * mul + add
!
primaryFactoryMethod
    ^#new
!
!

PrimitiveUgen subclass: #Convolution
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Convolution comment: 'Real-time convolver.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Convolution class methodsFor: 'instance creation'!
in: in kernel: kernel framesize: framesize
    | aConvolution |
    aConvolution := self new.
    aConvolution paramValues: {in. kernel. framesize}.
    ^aConvolution
!
in: in kernel: kernel framesize: framesize mul: mul
    ^(Convolution in: in kernel: kernel framesize: framesize) * mul
!
in: in kernel: kernel framesize: framesize mul: mul add: add
    ^(Convolution in: in kernel: kernel framesize: framesize) * mul + add
!
primaryFactoryMethod
    ^#in:kernel:framesize:
!
!

PrimitiveUgen subclass: #Crackle
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Crackle comment: 'Chaotic noise function.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Crackle class methodsFor: 'instance creation'!
chaosParam: chaosParam
    | aCrackle |
    aCrackle := self new.
    aCrackle paramValues: {chaosParam}.
    ^aCrackle
!
chaosParam: chaosParam mul: mul
    ^(Crackle chaosParam: chaosParam) * mul
!
chaosParam: chaosParam mul: mul add: add
    ^(Crackle chaosParam: chaosParam) * mul + add
!
primaryFactoryMethod
    ^#chaosParam:
!
!

PrimitiveUgen subclass: #CuspL
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

CuspL comment: 'Cusp map chaotic generator'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!CuspL class methodsFor: 'instance creation'!
freq: freq a: a b: b xi: xi
    | aCuspL |
    aCuspL := self new.
    aCuspL paramValues: {freq. a. b. xi}.
    ^aCuspL
!
freq: freq a: a b: b xi: xi mul: mul
    ^(CuspL freq: freq a: a b: b xi: xi) * mul
!
freq: freq a: a b: b xi: xi mul: mul add: add
    ^(CuspL freq: freq a: a b: b xi: xi) * mul + add
!
primaryFactoryMethod
    ^#freq:a:b:xi:
!
!

PrimitiveUgen subclass: #Dbufrd
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Dbufrd comment: 'Buffer read demand ugen'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Dbufrd class methodsFor: 'instance creation'!
bufnum: bufnum phase: phase loop: loop
    | aDbufrd |
    aDbufrd := self new.
    aDbufrd paramValues: {bufnum. phase. loop}.
    ^aDbufrd
!
bufnum: bufnum phase: phase loop: loop mul: mul
    ^(Dbufrd bufnum: bufnum phase: phase loop: loop) * mul
!
bufnum: bufnum phase: phase loop: loop mul: mul add: add
    ^(Dbufrd bufnum: bufnum phase: phase loop: loop) * mul + add
!
primaryFactoryMethod
    ^#bufnum:phase:loop:
!
!

PrimitiveUgen subclass: #Dbufwr
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Dbufwr comment: 'Buffer write demand ugen'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Dbufwr class methodsFor: 'instance creation'!
bufnum: bufnum phase: phase input: input loop: loop
    | aDbufwr |
    aDbufwr := self new.
    aDbufwr paramValues: {bufnum. phase. input. loop}.
    ^aDbufwr
!
bufnum: bufnum phase: phase input: input loop: loop mul: mul
    ^(Dbufwr bufnum: bufnum phase: phase input: input loop: loop) * mul
!
bufnum: bufnum phase: phase input: input loop: loop mul: mul add: add
    ^(Dbufwr bufnum: bufnum phase: phase input: input loop: loop) * mul + add
!
primaryFactoryMethod
    ^#bufnum:phase:input:loop:
!
!

PrimitiveUgen subclass: #DC
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

DC comment: 'Create a constant amplitude signal'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!DC class methodsFor: 'instance creation'!
in: in
    | aDC |
    aDC := self new.
    aDC paramValues: {in}.
    ^aDC
!
in: in mul: mul
    ^(DC in: in) * mul
!
in: in mul: mul add: add
    ^(DC in: in) * mul + add
!
primaryFactoryMethod
    ^#in:
!
!

PrimitiveUgen subclass: #Decay
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Decay comment: 'Exponential decay'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Decay class methodsFor: 'instance creation'!
in: in decayTime: decayTime
    | aDecay |
    aDecay := self new.
    aDecay paramValues: {in. decayTime}.
    ^aDecay
!
in: in decayTime: decayTime mul: mul
    ^(Decay in: in decayTime: decayTime) * mul
!
in: in decayTime: decayTime mul: mul add: add
    ^(Decay in: in decayTime: decayTime) * mul + add
!
primaryFactoryMethod
    ^#in:decayTime:
!
!

PrimitiveUgen subclass: #Decay2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Decay2 comment: 'Exponential decay'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Decay2 class methodsFor: 'instance creation'!
in: in attackTime: attackTime decayTime: decayTime
    | aDecay2 |
    aDecay2 := self new.
    aDecay2 paramValues: {in. attackTime. decayTime}.
    ^aDecay2
!
in: in attackTime: attackTime decayTime: decayTime mul: mul
    ^(Decay2 in: in attackTime: attackTime decayTime: decayTime) * mul
!
in: in attackTime: attackTime decayTime: decayTime mul: mul add: add
    ^(Decay2 in: in attackTime: attackTime decayTime: decayTime) * mul + add
!
primaryFactoryMethod
    ^#in:attackTime:decayTime:
!
!

PrimitiveUgen subclass: #DegreeToKey
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

DegreeToKey comment: 'Convert signal to modal pitch.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!DegreeToKey class methodsFor: 'instance creation'!
bufnum: bufnum in: in octave: octave
    | aDegreeToKey |
    aDegreeToKey := self new.
    aDegreeToKey paramValues: {bufnum. in. octave}.
    ^aDegreeToKey
!
bufnum: bufnum in: in octave: octave mul: mul
    ^(DegreeToKey bufnum: bufnum in: in octave: octave) * mul
!
bufnum: bufnum in: in octave: octave mul: mul add: add
    ^(DegreeToKey bufnum: bufnum in: in octave: octave) * mul + add
!
primaryFactoryMethod
    ^#bufnum:in:octave:
!
!

PrimitiveUgen subclass: #Delay1
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Delay1 comment: 'Single sample delay.'!


!Delay1 methodsFor: 'initialization'!
initialize
    super initialize.
    ugenName := #Delay1.
    paramNames := #(#in).
    mceNames := #().
    ugenRate := #(0).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    paramValues := {}.
    mceValues := {}.
!
!


!Delay1 class methodsFor: 'instance creation'!
in: in
    | aDelay1 |
    aDelay1 := self new.
    aDelay1 paramValues: {in}.
    ^aDelay1
!
in: in mul: mul
    ^(Delay1 in: in) * mul
!
in: in mul: mul add: add
    ^(Delay1 in: in) * mul + add
!
primaryFactoryMethod
    ^#in:
!
!

PrimitiveUgen subclass: #Delay2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Delay2 comment: 'Two sample delay.'!


!Delay2 methodsFor: 'initialization'!
initialize
    super initialize.
    ugenName := #Delay2.
    paramNames := #(#in).
    mceNames := #().
    ugenRate := #(0).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    paramValues := {}.
    mceValues := {}.
!
!


!Delay2 class methodsFor: 'instance creation'!
in: in
    | aDelay2 |
    aDelay2 := self new.
    aDelay2 paramValues: {in}.
    ^aDelay2
!
in: in mul: mul
    ^(Delay2 in: in) * mul
!
in: in mul: mul add: add
    ^(Delay2 in: in) * mul + add
!
primaryFactoryMethod
    ^#in:
!
!

PrimitiveUgen subclass: #DelayC
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

DelayC comment: 'Simple delay line with cubic interpolation.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!DelayC class methodsFor: 'instance creation'!
in: in maxdelaytime: maxdelaytime delaytime: delaytime
    | aDelayC |
    aDelayC := self new.
    aDelayC paramValues: {in. maxdelaytime. delaytime}.
    ^aDelayC
!
in: in maxdelaytime: maxdelaytime delaytime: delaytime mul: mul
    ^(DelayC in: in maxdelaytime: maxdelaytime delaytime: delaytime) * mul
!
in: in maxdelaytime: maxdelaytime delaytime: delaytime mul: mul add: add
    ^(DelayC in: in maxdelaytime: maxdelaytime delaytime: delaytime) * mul + add
!
primaryFactoryMethod
    ^#in:maxdelaytime:delaytime:
!
!

PrimitiveUgen subclass: #DelayL
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

DelayL comment: 'Simple delay line with linear interpolation.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!DelayL class methodsFor: 'instance creation'!
in: in maxdelaytime: maxdelaytime delaytime: delaytime
    | aDelayL |
    aDelayL := self new.
    aDelayL paramValues: {in. maxdelaytime. delaytime}.
    ^aDelayL
!
in: in maxdelaytime: maxdelaytime delaytime: delaytime mul: mul
    ^(DelayL in: in maxdelaytime: maxdelaytime delaytime: delaytime) * mul
!
in: in maxdelaytime: maxdelaytime delaytime: delaytime mul: mul add: add
    ^(DelayL in: in maxdelaytime: maxdelaytime delaytime: delaytime) * mul + add
!
primaryFactoryMethod
    ^#in:maxdelaytime:delaytime:
!
!

PrimitiveUgen subclass: #DelayN
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

DelayN comment: 'Simple delay line with no interpolation.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!DelayN class methodsFor: 'instance creation'!
in: in maxdelaytime: maxdelaytime delaytime: delaytime
    | aDelayN |
    aDelayN := self new.
    aDelayN paramValues: {in. maxdelaytime. delaytime}.
    ^aDelayN
!
in: in maxdelaytime: maxdelaytime delaytime: delaytime mul: mul
    ^(DelayN in: in maxdelaytime: maxdelaytime delaytime: delaytime) * mul
!
in: in maxdelaytime: maxdelaytime delaytime: delaytime mul: mul add: add
    ^(DelayN in: in maxdelaytime: maxdelaytime delaytime: delaytime) * mul + add
!
primaryFactoryMethod
    ^#in:maxdelaytime:delaytime:
!
!

PrimitiveUgen subclass: #Demand
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Demand comment: 'Demand results from demand rate UGens.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Demand class methodsFor: 'instance creation'!
trig: trig reset: reset demandUGens: demandUGens
    | aDemand |
    aDemand := self new.
    aDemand paramValues: {trig. reset}.
    aDemand mceValues: {demandUGens}.
    ^aDemand
!
trig: trig reset: reset demandUGens: demandUGens mul: mul
    ^(Demand trig: trig reset: reset demandUGens: demandUGens) * mul
!
trig: trig reset: reset demandUGens: demandUGens mul: mul add: add
    ^(Demand trig: trig reset: reset demandUGens: demandUGens) * mul + add
!
primaryFactoryMethod
    ^#trig:reset:demandUGens:
!
!

PrimitiveUgen subclass: #DetectSilence
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

DetectSilence comment: 'Detect when input falls below an amplitude threshold'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!DetectSilence class methodsFor: 'instance creation'!
in: in amp: amp time: time doneAction: doneAction
    | aDetectSilence |
    aDetectSilence := self new.
    aDetectSilence paramValues: {in. amp. time. doneAction}.
    ^aDetectSilence
!
in: in amp: amp time: time doneAction: doneAction mul: mul
    ^(DetectSilence in: in amp: amp time: time doneAction: doneAction) * mul
!
in: in amp: amp time: time doneAction: doneAction mul: mul add: add
    ^(DetectSilence in: in amp: amp time: time doneAction: doneAction) * mul + add
!
primaryFactoryMethod
    ^#in:amp:time:doneAction:
!
!

PrimitiveUgen subclass: #Diwhite
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Diwhite comment: 'Demand rate white noise random generator.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Diwhite class methodsFor: 'instance creation'!
length: length lo: lo hi: hi
    | aDiwhite |
    aDiwhite := self new.
    aDiwhite paramValues: {length. lo. hi}.
    ^aDiwhite
!
length: length lo: lo hi: hi mul: mul
    ^(Diwhite length: length lo: lo hi: hi) * mul
!
length: length lo: lo hi: hi mul: mul add: add
    ^(Diwhite length: length lo: lo hi: hi) * mul + add
!
primaryFactoryMethod
    ^#length:lo:hi:
!
!

PrimitiveUgen subclass: #Drand
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Drand comment: 'Demand rate random sequence generator.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Drand class methodsFor: 'instance creation'!
repeats: repeats list: list
    | aDrand |
    aDrand := self new.
    aDrand paramValues: {repeats}.
    aDrand mceValues: {list}.
    ^aDrand
!
repeats: repeats list: list mul: mul
    ^(Drand repeats: repeats list: list) * mul
!
repeats: repeats list: list mul: mul add: add
    ^(Drand repeats: repeats list: list) * mul + add
!
primaryFactoryMethod
    ^#repeats:list:
!
!

PrimitiveUgen subclass: #Dseq
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Dseq comment: 'Demand rate sequence generator.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Dseq class methodsFor: 'instance creation'!
repeats: repeats list: list
    | aDseq |
    aDseq := self new.
    aDseq paramValues: {repeats}.
    aDseq mceValues: {list}.
    ^aDseq
!
repeats: repeats list: list mul: mul
    ^(Dseq repeats: repeats list: list) * mul
!
repeats: repeats list: list mul: mul add: add
    ^(Dseq repeats: repeats list: list) * mul + add
!
primaryFactoryMethod
    ^#repeats:list:
!
!

PrimitiveUgen subclass: #Dseries
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Dseries comment: 'Demand rate arithmetic series UGen.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Dseries class methodsFor: 'instance creation'!
length: length start: start step: step
    | aDseries |
    aDseries := self new.
    aDseries paramValues: {length. start. step}.
    ^aDseries
!
length: length start: start step: step mul: mul
    ^(Dseries length: length start: start step: step) * mul
!
length: length start: start step: step mul: mul add: add
    ^(Dseries length: length start: start step: step) * mul + add
!
primaryFactoryMethod
    ^#length:start:step:
!
!

PrimitiveUgen subclass: #Dshuf
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Dshuf comment: 'Demand rate random sequence generator'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Dshuf class methodsFor: 'instance creation'!
repeats: repeats list: list
    | aDshuf |
    aDshuf := self new.
    aDshuf paramValues: {repeats}.
    aDshuf mceValues: {list}.
    ^aDshuf
!
repeats: repeats list: list mul: mul
    ^(Dshuf repeats: repeats list: list) * mul
!
repeats: repeats list: list mul: mul add: add
    ^(Dshuf repeats: repeats list: list) * mul + add
!
primaryFactoryMethod
    ^#repeats:list:
!
!

PrimitiveUgen subclass: #Dust
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Dust comment: 'Random impulses.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Dust class methodsFor: 'instance creation'!
density: density
    | aDust |
    aDust := self new.
    aDust paramValues: {density}.
    ^aDust
!
density: density mul: mul
    ^(Dust density: density) * mul
!
density: density mul: mul add: add
    ^(Dust density: density) * mul + add
!
primaryFactoryMethod
    ^#density:
!
!

PrimitiveUgen subclass: #Dust2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Dust2 comment: 'Random impulses.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Dust2 class methodsFor: 'instance creation'!
density: density
    | aDust2 |
    aDust2 := self new.
    aDust2 paramValues: {density}.
    ^aDust2
!
density: density mul: mul
    ^(Dust2 density: density) * mul
!
density: density mul: mul add: add
    ^(Dust2 density: density) * mul + add
!
primaryFactoryMethod
    ^#density:
!
!

PrimitiveUgen subclass: #Duty
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Duty comment: 'Demand results from demand rate UGens.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Duty class methodsFor: 'instance creation'!
dur: dur reset: reset doneAction: doneAction level: level
    | aDuty |
    aDuty := self new.
    aDuty paramValues: {dur. reset. doneAction. level}.
    ^aDuty
!
dur: dur reset: reset doneAction: doneAction level: level mul: mul
    ^(Duty dur: dur reset: reset doneAction: doneAction level: level) * mul
!
dur: dur reset: reset doneAction: doneAction level: level mul: mul add: add
    ^(Duty dur: dur reset: reset doneAction: doneAction level: level) * mul + add
!
primaryFactoryMethod
    ^#dur:reset:doneAction:level:
!
!

PrimitiveUgen subclass: #DWGPluckedStiff
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

DWGPluckedStiff comment: 'Plucked physical model.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!DWGPluckedStiff class methodsFor: 'instance creation'!
freq: freq amp: amp gate: gate pos: pos c1: c1 c3: c3 inp: inp release: release fB: fB
    | aDWGPluckedStiff |
    aDWGPluckedStiff := self new.
    aDWGPluckedStiff paramValues: {freq. amp. gate. pos. c1. c3. inp. release. fB}.
    ^aDWGPluckedStiff
!
freq: freq amp: amp gate: gate pos: pos c1: c1 c3: c3 inp: inp release: release fB: fB mul: mul
    ^(DWGPluckedStiff freq: freq amp: amp gate: gate pos: pos c1: c1 c3: c3 inp: inp release: release fB: fB) * mul
!
freq: freq amp: amp gate: gate pos: pos c1: c1 c3: c3 inp: inp release: release fB: fB mul: mul add: add
    ^(DWGPluckedStiff freq: freq amp: amp gate: gate pos: pos c1: c1 c3: c3 inp: inp release: release fB: fB) * mul + add
!
primaryFactoryMethod
    ^#freq:amp:gate:pos:c1:c3:inp:release:fB:
!
!

PrimitiveUgen subclass: #EnvGen
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

EnvGen comment: 'Envelope generator'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!EnvGen class methodsFor: 'instance creation'!
gate: gate levelScale: levelScale levelBias: levelBias timeScale: timeScale doneAction: doneAction envelope: envelope
    | anEnvGen |
    anEnvGen := self new.
    anEnvGen paramValues: {gate. levelScale. levelBias. timeScale. doneAction}.
    anEnvGen mceValues: {envelope}.
    ^anEnvGen
!
gate: gate levelScale: levelScale levelBias: levelBias timeScale: timeScale doneAction: doneAction envelope: envelope mul: mul
    ^(EnvGen gate: gate levelScale: levelScale levelBias: levelBias timeScale: timeScale doneAction: doneAction envelope: envelope) * mul
!
gate: gate levelScale: levelScale levelBias: levelBias timeScale: timeScale doneAction: doneAction envelope: envelope mul: mul add: add
    ^(EnvGen gate: gate levelScale: levelScale levelBias: levelBias timeScale: timeScale doneAction: doneAction envelope: envelope) * mul + add
!
primaryFactoryMethod
    ^#gate:levelScale:levelBias:timeScale:doneAction:envelope:
!
!

PrimitiveUgen subclass: #ExpRand
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

ExpRand comment: 'Exponential single random number generator.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!ExpRand class methodsFor: 'instance creation'!
lo: lo hi: hi
    | anExpRand |
    anExpRand := self new.
    anExpRand paramValues: {lo. hi}.
    ^anExpRand
!
lo: lo hi: hi mul: mul
    ^(ExpRand lo: lo hi: hi) * mul
!
lo: lo hi: hi mul: mul add: add
    ^(ExpRand lo: lo hi: hi) * mul + add
!
primaryFactoryMethod
    ^#lo:hi:
!
!

PrimitiveUgen subclass: #FBSineL
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

FBSineL comment: 'Feedback sine with chaotic phase indexing'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!FBSineL class methodsFor: 'instance creation'!
freq: freq im: im fb: fb a: a c: c xi: xi yi: yi
    | aFBSineL |
    aFBSineL := self new.
    aFBSineL paramValues: {freq. im. fb. a. c. xi. yi}.
    ^aFBSineL
!
freq: freq im: im fb: fb a: a c: c xi: xi yi: yi mul: mul
    ^(FBSineL freq: freq im: im fb: fb a: a c: c xi: xi yi: yi) * mul
!
freq: freq im: im fb: fb a: a c: c xi: xi yi: yi mul: mul add: add
    ^(FBSineL freq: freq im: im fb: fb a: a c: c xi: xi yi: yi) * mul + add
!
primaryFactoryMethod
    ^#freq:im:fb:a:c:xi:yi:
!
!

PrimitiveUgen subclass: #FBSineC
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

FBSineC comment: 'Feedback sine with chaotic phase indexing'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!FBSineC class methodsFor: 'instance creation'!
freq: freq im: im fb: fb a: a c: c xi: xi yi: yi
    | aFBSineC |
    aFBSineC := self new.
    aFBSineC paramValues: {freq. im. fb. a. c. xi. yi}.
    ^aFBSineC
!
freq: freq im: im fb: fb a: a c: c xi: xi yi: yi mul: mul
    ^(FBSineC freq: freq im: im fb: fb a: a c: c xi: xi yi: yi) * mul
!
freq: freq im: im fb: fb a: a c: c xi: xi yi: yi mul: mul add: add
    ^(FBSineC freq: freq im: im fb: fb a: a c: c xi: xi yi: yi) * mul + add
!
primaryFactoryMethod
    ^#freq:im:fb:a:c:xi:yi:
!
!

PrimitiveUgen subclass: #FFT
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

FFT comment: 'Fast Fourier Transform'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!FFT class methodsFor: 'instance creation'!
buffer: buffer in: in hop: hop wintype: wintype active: active winsize: winsize
    | aFFT |
    aFFT := self new.
    aFFT paramValues: {buffer. in. hop. wintype. active. winsize}.
    ^aFFT
!
buffer: buffer in: in hop: hop wintype: wintype active: active winsize: winsize mul: mul
    ^(FFT buffer: buffer in: in hop: hop wintype: wintype active: active winsize: winsize) * mul
!
buffer: buffer in: in hop: hop wintype: wintype active: active winsize: winsize mul: mul add: add
    ^(FFT buffer: buffer in: in hop: hop wintype: wintype active: active winsize: winsize) * mul + add
!
primaryFactoryMethod
    ^#buffer:in:hop:wintype:active:winsize:
!
!

PrimitiveUgen subclass: #Fold
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Fold comment: 'Fold a signal outside given thresholds.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Fold class methodsFor: 'instance creation'!
in: in lo: lo hi: hi
    | aFold |
    aFold := self new.
    aFold paramValues: {in. lo. hi}.
    ^aFold
!
in: in lo: lo hi: hi mul: mul
    ^(Fold in: in lo: lo hi: hi) * mul
!
in: in lo: lo hi: hi mul: mul add: add
    ^(Fold in: in lo: lo hi: hi) * mul + add
!
primaryFactoryMethod
    ^#in:lo:hi:
!
!

PrimitiveUgen subclass: #Formant
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Formant comment: 'Formant oscillator'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Formant class methodsFor: 'instance creation'!
fundfreq: fundfreq formfreq: formfreq bwfreq: bwfreq
    | aFormant |
    aFormant := self new.
    aFormant paramValues: {fundfreq. formfreq. bwfreq}.
    ^aFormant
!
fundfreq: fundfreq formfreq: formfreq bwfreq: bwfreq mul: mul
    ^(Formant fundfreq: fundfreq formfreq: formfreq bwfreq: bwfreq) * mul
!
fundfreq: fundfreq formfreq: formfreq bwfreq: bwfreq mul: mul add: add
    ^(Formant fundfreq: fundfreq formfreq: formfreq bwfreq: bwfreq) * mul + add
!
primaryFactoryMethod
    ^#fundfreq:formfreq:bwfreq:
!
!

PrimitiveUgen subclass: #Formlet
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Formlet comment: 'FOF-like filter.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Formlet class methodsFor: 'instance creation'!
in: in freq: freq attacktime: attacktime decaytime: decaytime
    | aFormlet |
    aFormlet := self new.
    aFormlet paramValues: {in. freq. attacktime. decaytime}.
    ^aFormlet
!
in: in freq: freq attacktime: attacktime decaytime: decaytime mul: mul
    ^(Formlet in: in freq: freq attacktime: attacktime decaytime: decaytime) * mul
!
in: in freq: freq attacktime: attacktime decaytime: decaytime mul: mul add: add
    ^(Formlet in: in freq: freq attacktime: attacktime decaytime: decaytime) * mul + add
!
primaryFactoryMethod
    ^#in:freq:attacktime:decaytime:
!
!

PrimitiveUgen subclass: #FOS
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

FOS comment: 'First order filter section.'!


!FOS methodsFor: 'initialization'!
initialize
    super initialize.
    ugenName := #FOS.
    paramNames := #(#in #a0 #a1 #b1).
    mceNames := #().
    ugenRate := #(0).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    paramValues := {}.
    mceValues := {}.
!
!


!FOS class methodsFor: 'instance creation'!
in: in a0: a0 a1: a1 b1: b1
    | aFOS |
    aFOS := self new.
    aFOS paramValues: {in. a0. a1. b1}.
    ^aFOS
!
in: in a0: a0 a1: a1 b1: b1 mul: mul
    ^(FOS in: in a0: a0 a1: a1 b1: b1) * mul
!
in: in a0: a0 a1: a1 b1: b1 mul: mul add: add
    ^(FOS in: in a0: a0 a1: a1 b1: b1) * mul + add
!
primaryFactoryMethod
    ^#in:a0:a1:b1:
!
!

PrimitiveUgen subclass: #FreqShift
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

FreqShift comment: 'Frequency Shifter.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!FreqShift class methodsFor: 'instance creation'!
in: in freq: freq phase: phase
    | aFreqShift |
    aFreqShift := self new.
    aFreqShift paramValues: {in. freq. phase}.
    ^aFreqShift
!
in: in freq: freq phase: phase mul: mul
    ^(FreqShift in: in freq: freq phase: phase) * mul
!
in: in freq: freq phase: phase mul: mul add: add
    ^(FreqShift in: in freq: freq phase: phase) * mul + add
!
primaryFactoryMethod
    ^#in:freq:phase:
!
!

PrimitiveUgen subclass: #FSinOsc
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

FSinOsc comment: 'Fast sine oscillator.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!FSinOsc class methodsFor: 'instance creation'!
freq: freq iphase: iphase
    | aFSinOsc |
    aFSinOsc := self new.
    aFSinOsc paramValues: {freq. iphase}.
    ^aFSinOsc
!
freq: freq iphase: iphase mul: mul
    ^(FSinOsc freq: freq iphase: iphase) * mul
!
freq: freq iphase: iphase mul: mul add: add
    ^(FSinOsc freq: freq iphase: iphase) * mul + add
!
primaryFactoryMethod
    ^#freq:iphase:
!
!

PrimitiveUgen subclass: #FreeVerb
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

FreeVerb comment: 'A reverb'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!FreeVerb class methodsFor: 'instance creation'!
in: in mix: mix room: room damp: damp
    | aFreeVerb |
    aFreeVerb := self new.
    aFreeVerb paramValues: {in. mix. room. damp}.
    ^aFreeVerb
!
in: in mix: mix room: room damp: damp mul: mul
    ^(FreeVerb in: in mix: mix room: room damp: damp) * mul
!
in: in mix: mix room: room damp: damp mul: mul add: add
    ^(FreeVerb in: in mix: mix room: room damp: damp) * mul + add
!
primaryFactoryMethod
    ^#in:mix:room:damp:
!
!

PrimitiveUgen subclass: #FreeVerb2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

FreeVerb2 comment: 'A two-channel reverb'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!FreeVerb2 class methodsFor: 'instance creation'!
in: in in2: in2 mix: mix room: room damp: damp
    | aFreeVerb2 |
    aFreeVerb2 := self new.
    aFreeVerb2 paramValues: {in. in2. mix. room. damp}.
    ^aFreeVerb2
!
in: in in2: in2 mix: mix room: room damp: damp mul: mul
    ^(FreeVerb2 in: in in2: in2 mix: mix room: room damp: damp) * mul
!
in: in in2: in2 mix: mix room: room damp: damp mul: mul add: add
    ^(FreeVerb2 in: in in2: in2 mix: mix room: room damp: damp) * mul + add
!
primaryFactoryMethod
    ^#in:in2:mix:room:damp:
!
!

PrimitiveUgen subclass: #Gate
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Gate comment: 'Gate or hold.'!


!Gate methodsFor: 'initialization'!
initialize
    super initialize.
    ugenName := #Gate.
    paramNames := #(#in #trig).
    mceNames := #().
    ugenRate := #(0).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    paramValues := {}.
    mceValues := {}.
!
!


!Gate class methodsFor: 'instance creation'!
in: in trig: trig
    | aGate |
    aGate := self new.
    aGate paramValues: {in. trig}.
    ^aGate
!
in: in trig: trig mul: mul
    ^(Gate in: in trig: trig) * mul
!
in: in trig: trig mul: mul add: add
    ^(Gate in: in trig: trig) * mul + add
!
primaryFactoryMethod
    ^#in:trig:
!
!

PrimitiveUgen subclass: #Gendy1
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Gendy1 comment: 'Dynamic stochastic synthesis generator.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Gendy1 class methodsFor: 'instance creation'!
ampdist: ampdist durdist: durdist adparam: adparam ddparam: ddparam minfreq: minfreq maxfreq: maxfreq ampscale: ampscale durscale: durscale initCPs: initCPs knum: knum
    | aGendy1 |
    aGendy1 := self new.
    aGendy1 paramValues: {ampdist. durdist. adparam. ddparam. minfreq. maxfreq. ampscale. durscale. initCPs. knum}.
    ^aGendy1
!
ampdist: ampdist durdist: durdist adparam: adparam ddparam: ddparam minfreq: minfreq maxfreq: maxfreq ampscale: ampscale durscale: durscale initCPs: initCPs knum: knum mul: mul
    ^(Gendy1 ampdist: ampdist durdist: durdist adparam: adparam ddparam: ddparam minfreq: minfreq maxfreq: maxfreq ampscale: ampscale durscale: durscale initCPs: initCPs knum: knum) * mul
!
ampdist: ampdist durdist: durdist adparam: adparam ddparam: ddparam minfreq: minfreq maxfreq: maxfreq ampscale: ampscale durscale: durscale initCPs: initCPs knum: knum mul: mul add: add
    ^(Gendy1 ampdist: ampdist durdist: durdist adparam: adparam ddparam: ddparam minfreq: minfreq maxfreq: maxfreq ampscale: ampscale durscale: durscale initCPs: initCPs knum: knum) * mul + add
!
primaryFactoryMethod
    ^#ampdist:durdist:adparam:ddparam:minfreq:maxfreq:ampscale:durscale:initCPs:knum:
!
!

PrimitiveUgen subclass: #GrainFM
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

GrainFM comment: 'Granular synthesis with frequency modulated sine tones'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!GrainFM class methodsFor: 'instance creation'!
numChan: numChan trigger: trigger dur: dur carfreq: carfreq modfreq: modfreq index: index pan: pan envbufnum: envbufnum maxGrains: maxGrains
    | aGrainFM |
    aGrainFM := self new.
    aGrainFM numChan: numChan.
    aGrainFM paramValues: {trigger. dur. carfreq. modfreq. index. pan. envbufnum. maxGrains}.
    ^aGrainFM
!
numChan: numChan trigger: trigger dur: dur carfreq: carfreq modfreq: modfreq index: index pan: pan envbufnum: envbufnum maxGrains: maxGrains mul: mul
    ^(GrainFM numChan: numChan trigger: trigger dur: dur carfreq: carfreq modfreq: modfreq index: index pan: pan envbufnum: envbufnum maxGrains: maxGrains) * mul
!
numChan: numChan trigger: trigger dur: dur carfreq: carfreq modfreq: modfreq index: index pan: pan envbufnum: envbufnum maxGrains: maxGrains mul: mul add: add
    ^(GrainFM numChan: numChan trigger: trigger dur: dur carfreq: carfreq modfreq: modfreq index: index pan: pan envbufnum: envbufnum maxGrains: maxGrains) * mul + add
!
primaryFactoryMethod
    ^#numChan:trigger:dur:carfreq:modfreq:index:pan:envbufnum:maxGrains:
!
!

PrimitiveUgen subclass: #GrainSin
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

GrainSin comment: 'Granular synthesis with sine tones'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!GrainSin class methodsFor: 'instance creation'!
numChan: numChan trigger: trigger dur: dur freq: freq pan: pan envbufnum: envbufnum maxGrains: maxGrains
    | aGrainSin |
    aGrainSin := self new.
    aGrainSin numChan: numChan.
    aGrainSin paramValues: {trigger. dur. freq. pan. envbufnum. maxGrains}.
    ^aGrainSin
!
numChan: numChan trigger: trigger dur: dur freq: freq pan: pan envbufnum: envbufnum maxGrains: maxGrains mul: mul
    ^(GrainSin numChan: numChan trigger: trigger dur: dur freq: freq pan: pan envbufnum: envbufnum maxGrains: maxGrains) * mul
!
numChan: numChan trigger: trigger dur: dur freq: freq pan: pan envbufnum: envbufnum maxGrains: maxGrains mul: mul add: add
    ^(GrainSin numChan: numChan trigger: trigger dur: dur freq: freq pan: pan envbufnum: envbufnum maxGrains: maxGrains) * mul + add
!
primaryFactoryMethod
    ^#numChan:trigger:dur:freq:pan:envbufnum:maxGrains:
!
!

PrimitiveUgen subclass: #GrayNoise
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

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
    paramValues := {}.
    mceValues := {}.
!
!


!GrayNoise class methodsFor: 'instance creation'!
mul: mul
    ^(GrayNoise new) * mul
!
mul: mul add: add
    ^(GrayNoise new) * mul + add
!
primaryFactoryMethod
    ^#new
!
!

PrimitiveUgen subclass: #GVerb
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

GVerb comment: 'A two-channel reverb'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!GVerb class methodsFor: 'instance creation'!
in: in roomsize: roomsize revtime: revtime damping: damping inputbw: inputbw spread: spread drylevel: drylevel earlyreflevel: earlyreflevel taillevel: taillevel maxroomsize: maxroomsize
    | aGVerb |
    aGVerb := self new.
    aGVerb paramValues: {in. roomsize. revtime. damping. inputbw. spread. drylevel. earlyreflevel. taillevel. maxroomsize}.
    ^aGVerb
!
in: in roomsize: roomsize revtime: revtime damping: damping inputbw: inputbw spread: spread drylevel: drylevel earlyreflevel: earlyreflevel taillevel: taillevel maxroomsize: maxroomsize mul: mul
    ^(GVerb in: in roomsize: roomsize revtime: revtime damping: damping inputbw: inputbw spread: spread drylevel: drylevel earlyreflevel: earlyreflevel taillevel: taillevel maxroomsize: maxroomsize) * mul
!
in: in roomsize: roomsize revtime: revtime damping: damping inputbw: inputbw spread: spread drylevel: drylevel earlyreflevel: earlyreflevel taillevel: taillevel maxroomsize: maxroomsize mul: mul add: add
    ^(GVerb in: in roomsize: roomsize revtime: revtime damping: damping inputbw: inputbw spread: spread drylevel: drylevel earlyreflevel: earlyreflevel taillevel: taillevel maxroomsize: maxroomsize) * mul + add
!
primaryFactoryMethod
    ^#in:roomsize:revtime:damping:inputbw:spread:drylevel:earlyreflevel:taillevel:maxroomsize:
!
!

PrimitiveUgen subclass: #Hasher
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Hasher comment: 'Scrambled value with a hash function.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Hasher class methodsFor: 'instance creation'!
in: in
    | aHasher |
    aHasher := self new.
    aHasher paramValues: {in}.
    ^aHasher
!
in: in mul: mul
    ^(Hasher in: in) * mul
!
in: in mul: mul add: add
    ^(Hasher in: in) * mul + add
!
primaryFactoryMethod
    ^#in:
!
!

PrimitiveUgen subclass: #HenonL
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

HenonL comment: 'Henon map chaotic generator'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!HenonL class methodsFor: 'instance creation'!
freq: freq a: a b: b x0: x0 x1: x1
    | aHenonL |
    aHenonL := self new.
    aHenonL paramValues: {freq. a. b. x0. x1}.
    ^aHenonL
!
freq: freq a: a b: b x0: x0 x1: x1 mul: mul
    ^(HenonL freq: freq a: a b: b x0: x0 x1: x1) * mul
!
freq: freq a: a b: b x0: x0 x1: x1 mul: mul add: add
    ^(HenonL freq: freq a: a b: b x0: x0 x1: x1) * mul + add
!
primaryFactoryMethod
    ^#freq:a:b:x0:x1:
!
!

PrimitiveUgen subclass: #HenonC
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

HenonC comment: 'Henon map chaotic generator'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!HenonC class methodsFor: 'instance creation'!
freq: freq a: a b: b x0: x0 x1: x1
    | aHenonC |
    aHenonC := self new.
    aHenonC paramValues: {freq. a. b. x0. x1}.
    ^aHenonC
!
freq: freq a: a b: b x0: x0 x1: x1 mul: mul
    ^(HenonC freq: freq a: a b: b x0: x0 x1: x1) * mul
!
freq: freq a: a b: b x0: x0 x1: x1 mul: mul add: add
    ^(HenonC freq: freq a: a b: b x0: x0 x1: x1) * mul + add
!
primaryFactoryMethod
    ^#freq:a:b:x0:x1:
!
!

PrimitiveUgen subclass: #HPF
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

HPF comment: '2nd order Butterworth highpass filter.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!HPF class methodsFor: 'instance creation'!
in: in freq: freq
    | aHPF |
    aHPF := self new.
    aHPF paramValues: {in. freq}.
    ^aHPF
!
in: in freq: freq mul: mul
    ^(HPF in: in freq: freq) * mul
!
in: in freq: freq mul: mul add: add
    ^(HPF in: in freq: freq) * mul + add
!
primaryFactoryMethod
    ^#in:freq:
!
!

PrimitiveUgen subclass: #HPZ1
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

HPZ1 comment: 'Two point difference filter'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!HPZ1 class methodsFor: 'instance creation'!
in: in
    | aHPZ1 |
    aHPZ1 := self new.
    aHPZ1 paramValues: {in}.
    ^aHPZ1
!
in: in mul: mul
    ^(HPZ1 in: in) * mul
!
in: in mul: mul add: add
    ^(HPZ1 in: in) * mul + add
!
primaryFactoryMethod
    ^#in:
!
!

PrimitiveUgen subclass: #HPZ2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

HPZ2 comment: 'Two zero fixed midcut.'!


!HPZ2 methodsFor: 'initialization'!
initialize
    super initialize.
    ugenName := #HPZ2.
    paramNames := #(#in).
    mceNames := #().
    ugenRate := #(0).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    paramValues := {}.
    mceValues := {}.
!
!


!HPZ2 class methodsFor: 'instance creation'!
in: in
    | aHPZ2 |
    aHPZ2 := self new.
    aHPZ2 paramValues: {in}.
    ^aHPZ2
!
in: in mul: mul
    ^(HPZ2 in: in) * mul
!
in: in mul: mul add: add
    ^(HPZ2 in: in) * mul + add
!
primaryFactoryMethod
    ^#in:
!
!

PrimitiveUgen subclass: #IFFT
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

IFFT comment: 'Inverse Fast Fourier Transform'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!IFFT class methodsFor: 'instance creation'!
buffer: buffer wintype: wintype winsize: winsize
    | anIFFT |
    anIFFT := self new.
    anIFFT paramValues: {buffer. wintype. winsize}.
    ^anIFFT
!
buffer: buffer wintype: wintype winsize: winsize mul: mul
    ^(IFFT buffer: buffer wintype: wintype winsize: winsize) * mul
!
buffer: buffer wintype: wintype winsize: winsize mul: mul add: add
    ^(IFFT buffer: buffer wintype: wintype winsize: winsize) * mul + add
!
primaryFactoryMethod
    ^#buffer:wintype:winsize:
!
!

PrimitiveUgen subclass: #Impulse
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Impulse comment: 'Impulse oscillator.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Impulse class methodsFor: 'instance creation'!
freq: freq phase: phase
    | anImpulse |
    anImpulse := self new.
    anImpulse paramValues: {freq. phase}.
    ^anImpulse
!
freq: freq phase: phase mul: mul
    ^(Impulse freq: freq phase: phase) * mul
!
freq: freq phase: phase mul: mul add: add
    ^(Impulse freq: freq phase: phase) * mul + add
!
primaryFactoryMethod
    ^#freq:phase:
!
!

PrimitiveUgen subclass: #In
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

In comment: 'Read a signal from a bus.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!In class methodsFor: 'instance creation'!
numChan: numChan bus: bus
    | anIn |
    anIn := self new.
    anIn numChan: numChan.
    anIn paramValues: {bus}.
    ^anIn
!
numChan: numChan bus: bus mul: mul
    ^(In numChan: numChan bus: bus) * mul
!
numChan: numChan bus: bus mul: mul add: add
    ^(In numChan: numChan bus: bus) * mul + add
!
primaryFactoryMethod
    ^#numChan:bus:
!
!

PrimitiveUgen subclass: #Index
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Index comment: 'Index into a table with a signal'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Index class methodsFor: 'instance creation'!
bufnum: bufnum in: in
    | anIndex |
    anIndex := self new.
    anIndex paramValues: {bufnum. in}.
    ^anIndex
!
bufnum: bufnum in: in mul: mul
    ^(Index bufnum: bufnum in: in) * mul
!
bufnum: bufnum in: in mul: mul add: add
    ^(Index bufnum: bufnum in: in) * mul + add
!
primaryFactoryMethod
    ^#bufnum:in:
!
!

PrimitiveUgen subclass: #IndexInBetween
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

IndexInBetween comment: 'Finds the (lowest) point in the Buffer at which the input signal lies in-between the two values'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!IndexInBetween class methodsFor: 'instance creation'!
bufnum: bufnum in: in
    | anIndexInBetween |
    anIndexInBetween := self new.
    anIndexInBetween paramValues: {bufnum. in}.
    ^anIndexInBetween
!
bufnum: bufnum in: in mul: mul
    ^(IndexInBetween bufnum: bufnum in: in) * mul
!
bufnum: bufnum in: in mul: mul add: add
    ^(IndexInBetween bufnum: bufnum in: in) * mul + add
!
primaryFactoryMethod
    ^#bufnum:in:
!
!

PrimitiveUgen subclass: #InFeedback
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

InFeedback comment: 'Read signal from a bus with a current or one cycle old timestamp.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!InFeedback class methodsFor: 'instance creation'!
numChan: numChan bus: bus
    | anInFeedback |
    anInFeedback := self new.
    anInFeedback numChan: numChan.
    anInFeedback paramValues: {bus}.
    ^anInFeedback
!
numChan: numChan bus: bus mul: mul
    ^(InFeedback numChan: numChan bus: bus) * mul
!
numChan: numChan bus: bus mul: mul add: add
    ^(InFeedback numChan: numChan bus: bus) * mul + add
!
primaryFactoryMethod
    ^#numChan:bus:
!
!

PrimitiveUgen subclass: #InRange
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

InRange comment: 'Tests if a signal is within a given range.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!InRange class methodsFor: 'instance creation'!
in: in lo: lo hi: hi
    | anInRange |
    anInRange := self new.
    anInRange paramValues: {in. lo. hi}.
    ^anInRange
!
in: in lo: lo hi: hi mul: mul
    ^(InRange in: in lo: lo hi: hi) * mul
!
in: in lo: lo hi: hi mul: mul add: add
    ^(InRange in: in lo: lo hi: hi) * mul + add
!
primaryFactoryMethod
    ^#in:lo:hi:
!
!

PrimitiveUgen subclass: #IRand
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

IRand comment: 'Single integer random number generator.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!IRand class methodsFor: 'instance creation'!
lo: lo hi: hi
    | anIRand |
    anIRand := self new.
    anIRand paramValues: {lo. hi}.
    ^anIRand
!
lo: lo hi: hi mul: mul
    ^(IRand lo: lo hi: hi) * mul
!
lo: lo hi: hi mul: mul add: add
    ^(IRand lo: lo hi: hi) * mul + add
!
primaryFactoryMethod
    ^#lo:hi:
!
!

PrimitiveUgen subclass: #Integrator
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Integrator comment: 'A leaky integrator.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Integrator class methodsFor: 'instance creation'!
in: in coef: coef
    | anIntegrator |
    anIntegrator := self new.
    anIntegrator paramValues: {in. coef}.
    ^anIntegrator
!
in: in coef: coef mul: mul
    ^(Integrator in: in coef: coef) * mul
!
in: in coef: coef mul: mul add: add
    ^(Integrator in: in coef: coef) * mul + add
!
primaryFactoryMethod
    ^#in:coef:
!
!

PrimitiveUgen subclass: #K2A
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

K2A comment: 'Control to audio rate converter.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!K2A class methodsFor: 'instance creation'!
in: in
    | aK2A |
    aK2A := self new.
    aK2A paramValues: {in}.
    ^aK2A
!
in: in mul: mul
    ^(K2A in: in) * mul
!
in: in mul: mul add: add
    ^(K2A in: in) * mul + add
!
primaryFactoryMethod
    ^#in:
!
!

PrimitiveUgen subclass: #KeyState
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

KeyState comment: 'Respond to the state of a key'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!KeyState class methodsFor: 'instance creation'!
keycode: keycode minval: minval maxval: maxval lag: lag
    | aKeyState |
    aKeyState := self new.
    aKeyState paramValues: {keycode. minval. maxval. lag}.
    ^aKeyState
!
keycode: keycode minval: minval maxval: maxval lag: lag mul: mul
    ^(KeyState keycode: keycode minval: minval maxval: maxval lag: lag) * mul
!
keycode: keycode minval: minval maxval: maxval lag: lag mul: mul add: add
    ^(KeyState keycode: keycode minval: minval maxval: maxval lag: lag) * mul + add
!
primaryFactoryMethod
    ^#keycode:minval:maxval:lag:
!
!

PrimitiveUgen subclass: #Klang
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Klang comment: 'Sine oscillator bank'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Klang class methodsFor: 'instance creation'!
freqscale: freqscale freqoffset: freqoffset specificationsArrayRef: specificationsArrayRef
    | aKlang |
    aKlang := self new.
    aKlang paramValues: {freqscale. freqoffset}.
    aKlang mceValues: {specificationsArrayRef}.
    ^aKlang
!
freqscale: freqscale freqoffset: freqoffset specificationsArrayRef: specificationsArrayRef mul: mul
    ^(Klang freqscale: freqscale freqoffset: freqoffset specificationsArrayRef: specificationsArrayRef) * mul
!
freqscale: freqscale freqoffset: freqoffset specificationsArrayRef: specificationsArrayRef mul: mul add: add
    ^(Klang freqscale: freqscale freqoffset: freqoffset specificationsArrayRef: specificationsArrayRef) * mul + add
!
primaryFactoryMethod
    ^#freqscale:freqoffset:specificationsArrayRef:
!
!

PrimitiveUgen subclass: #Klank
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Klank comment: 'Bank of resonators'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Klank class methodsFor: 'instance creation'!
input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef
    | aKlank |
    aKlank := self new.
    aKlank paramValues: {input. freqscale. freqoffset. decayscale}.
    aKlank mceValues: {specificationsArrayRef}.
    ^aKlank
!
input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef mul: mul
    ^(Klank input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef) * mul
!
input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef mul: mul add: add
    ^(Klank input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef) * mul + add
!
primaryFactoryMethod
    ^#input:freqscale:freqoffset:decayscale:specificationsArrayRef:
!
!

PrimitiveUgen subclass: #LFBrownNoise1
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LFBrownNoise1 comment: 'random walk linear interp'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!LFBrownNoise1 class methodsFor: 'instance creation'!
freq: freq dev: dev dist: dist
    | aLFBrownNoise1 |
    aLFBrownNoise1 := self new.
    aLFBrownNoise1 paramValues: {freq. dev. dist}.
    ^aLFBrownNoise1
!
freq: freq dev: dev dist: dist mul: mul
    ^(LFBrownNoise1 freq: freq dev: dev dist: dist) * mul
!
freq: freq dev: dev dist: dist mul: mul add: add
    ^(LFBrownNoise1 freq: freq dev: dev dist: dist) * mul + add
!
primaryFactoryMethod
    ^#freq:dev:dist:
!
!

PrimitiveUgen subclass: #LFClipNoise
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LFClipNoise comment: 'Clipped noise'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!LFClipNoise class methodsFor: 'instance creation'!
freq: freq
    | aLFClipNoise |
    aLFClipNoise := self new.
    aLFClipNoise paramValues: {freq}.
    ^aLFClipNoise
!
freq: freq mul: mul
    ^(LFClipNoise freq: freq) * mul
!
freq: freq mul: mul add: add
    ^(LFClipNoise freq: freq) * mul + add
!
primaryFactoryMethod
    ^#freq:
!
!

PrimitiveUgen subclass: #LFCub
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LFCub comment: 'A sine like shape made of two cubic pieces'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!LFCub class methodsFor: 'instance creation'!
freq: freq iphase: iphase
    | aLFCub |
    aLFCub := self new.
    aLFCub paramValues: {freq. iphase}.
    ^aLFCub
!
freq: freq iphase: iphase mul: mul
    ^(LFCub freq: freq iphase: iphase) * mul
!
freq: freq iphase: iphase mul: mul add: add
    ^(LFCub freq: freq iphase: iphase) * mul + add
!
primaryFactoryMethod
    ^#freq:iphase:
!
!

PrimitiveUgen subclass: #LFDNoise1
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LFDNoise1 comment: 'Dynamic ramp noise'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!LFDNoise1 class methodsFor: 'instance creation'!
freq: freq
    | aLFDNoise1 |
    aLFDNoise1 := self new.
    aLFDNoise1 paramValues: {freq}.
    ^aLFDNoise1
!
freq: freq mul: mul
    ^(LFDNoise1 freq: freq) * mul
!
freq: freq mul: mul add: add
    ^(LFDNoise1 freq: freq) * mul + add
!
primaryFactoryMethod
    ^#freq:
!
!

PrimitiveUgen subclass: #LFDNoise3
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LFDNoise3 comment: 'Dynamic cubic noise'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!LFDNoise3 class methodsFor: 'instance creation'!
freq: freq
    | aLFDNoise3 |
    aLFDNoise3 := self new.
    aLFDNoise3 paramValues: {freq}.
    ^aLFDNoise3
!
freq: freq mul: mul
    ^(LFDNoise3 freq: freq) * mul
!
freq: freq mul: mul add: add
    ^(LFDNoise3 freq: freq) * mul + add
!
primaryFactoryMethod
    ^#freq:
!
!

PrimitiveUgen subclass: #LFGauss
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LFGauss comment: 'Gaussian function oscillator'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!LFGauss class methodsFor: 'instance creation'!
duration: duration width: width iphase: iphase loop: loop doneAction: doneAction
    | aLFGauss |
    aLFGauss := self new.
    aLFGauss paramValues: {duration. width. iphase. loop. doneAction}.
    ^aLFGauss
!
duration: duration width: width iphase: iphase loop: loop doneAction: doneAction mul: mul
    ^(LFGauss duration: duration width: width iphase: iphase loop: loop doneAction: doneAction) * mul
!
duration: duration width: width iphase: iphase loop: loop doneAction: doneAction mul: mul add: add
    ^(LFGauss duration: duration width: width iphase: iphase loop: loop doneAction: doneAction) * mul + add
!
primaryFactoryMethod
    ^#duration:width:iphase:loop:doneAction:
!
!

PrimitiveUgen subclass: #LFNoise0
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LFNoise0 comment: 'Step noise'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!LFNoise0 class methodsFor: 'instance creation'!
freq: freq
    | aLFNoise0 |
    aLFNoise0 := self new.
    aLFNoise0 paramValues: {freq}.
    ^aLFNoise0
!
freq: freq mul: mul
    ^(LFNoise0 freq: freq) * mul
!
freq: freq mul: mul add: add
    ^(LFNoise0 freq: freq) * mul + add
!
primaryFactoryMethod
    ^#freq:
!
!

PrimitiveUgen subclass: #LFNoise1
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LFNoise1 comment: 'Ramp noise'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!LFNoise1 class methodsFor: 'instance creation'!
freq: freq
    | aLFNoise1 |
    aLFNoise1 := self new.
    aLFNoise1 paramValues: {freq}.
    ^aLFNoise1
!
freq: freq mul: mul
    ^(LFNoise1 freq: freq) * mul
!
freq: freq mul: mul add: add
    ^(LFNoise1 freq: freq) * mul + add
!
primaryFactoryMethod
    ^#freq:
!
!

PrimitiveUgen subclass: #LFNoise2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LFNoise2 comment: 'Quadratic noise.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!LFNoise2 class methodsFor: 'instance creation'!
freq: freq
    | aLFNoise2 |
    aLFNoise2 := self new.
    aLFNoise2 paramValues: {freq}.
    ^aLFNoise2
!
freq: freq mul: mul
    ^(LFNoise2 freq: freq) * mul
!
freq: freq mul: mul add: add
    ^(LFNoise2 freq: freq) * mul + add
!
primaryFactoryMethod
    ^#freq:
!
!

PrimitiveUgen subclass: #LFPar
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LFPar comment: 'Parabolic oscillator'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!LFPar class methodsFor: 'instance creation'!
freq: freq iphase: iphase
    | aLFPar |
    aLFPar := self new.
    aLFPar paramValues: {freq. iphase}.
    ^aLFPar
!
freq: freq iphase: iphase mul: mul
    ^(LFPar freq: freq iphase: iphase) * mul
!
freq: freq iphase: iphase mul: mul add: add
    ^(LFPar freq: freq iphase: iphase) * mul + add
!
primaryFactoryMethod
    ^#freq:iphase:
!
!

PrimitiveUgen subclass: #LFPulse
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LFPulse comment: 'pulse oscillator'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!LFPulse class methodsFor: 'instance creation'!
freq: freq iphase: iphase width: width
    | aLFPulse |
    aLFPulse := self new.
    aLFPulse paramValues: {freq. iphase. width}.
    ^aLFPulse
!
freq: freq iphase: iphase width: width mul: mul
    ^(LFPulse freq: freq iphase: iphase width: width) * mul
!
freq: freq iphase: iphase width: width mul: mul add: add
    ^(LFPulse freq: freq iphase: iphase width: width) * mul + add
!
primaryFactoryMethod
    ^#freq:iphase:width:
!
!

PrimitiveUgen subclass: #LFSaw
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LFSaw comment: 'Sawtooth oscillator'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!LFSaw class methodsFor: 'instance creation'!
freq: freq iphase: iphase
    | aLFSaw |
    aLFSaw := self new.
    aLFSaw paramValues: {freq. iphase}.
    ^aLFSaw
!
freq: freq iphase: iphase mul: mul
    ^(LFSaw freq: freq iphase: iphase) * mul
!
freq: freq iphase: iphase mul: mul add: add
    ^(LFSaw freq: freq iphase: iphase) * mul + add
!
primaryFactoryMethod
    ^#freq:iphase:
!
!

PrimitiveUgen subclass: #LFTri
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LFTri comment: 'Triangle oscillator'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!LFTri class methodsFor: 'instance creation'!
freq: freq iphase: iphase
    | aLFTri |
    aLFTri := self new.
    aLFTri paramValues: {freq. iphase}.
    ^aLFTri
!
freq: freq iphase: iphase mul: mul
    ^(LFTri freq: freq iphase: iphase) * mul
!
freq: freq iphase: iphase mul: mul add: add
    ^(LFTri freq: freq iphase: iphase) * mul + add
!
primaryFactoryMethod
    ^#freq:iphase:
!
!

PrimitiveUgen subclass: #LPF
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LPF comment: '2nd order Butterworth lowpass filter'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!LPF class methodsFor: 'instance creation'!
in: in freq: freq
    | aLPF |
    aLPF := self new.
    aLPF paramValues: {in. freq}.
    ^aLPF
!
in: in freq: freq mul: mul
    ^(LPF in: in freq: freq) * mul
!
in: in freq: freq mul: mul add: add
    ^(LPF in: in freq: freq) * mul + add
!
primaryFactoryMethod
    ^#in:freq:
!
!

PrimitiveUgen subclass: #Lag
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Lag comment: 'Exponential lag'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Lag class methodsFor: 'instance creation'!
in: in lagTime: lagTime
    | aLag |
    aLag := self new.
    aLag paramValues: {in. lagTime}.
    ^aLag
!
in: in lagTime: lagTime mul: mul
    ^(Lag in: in lagTime: lagTime) * mul
!
in: in lagTime: lagTime mul: mul add: add
    ^(Lag in: in lagTime: lagTime) * mul + add
!
primaryFactoryMethod
    ^#in:lagTime:
!
!

PrimitiveUgen subclass: #LagUD
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LagUD comment: 'Exponential lag'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!LagUD class methodsFor: 'instance creation'!
in: in lagTimeU: lagTimeU lagTimeD: lagTimeD
    | aLagUD |
    aLagUD := self new.
    aLagUD paramValues: {in. lagTimeU. lagTimeD}.
    ^aLagUD
!
in: in lagTimeU: lagTimeU lagTimeD: lagTimeD mul: mul
    ^(LagUD in: in lagTimeU: lagTimeU lagTimeD: lagTimeD) * mul
!
in: in lagTimeU: lagTimeU lagTimeD: lagTimeD mul: mul add: add
    ^(LagUD in: in lagTimeU: lagTimeU lagTimeD: lagTimeD) * mul + add
!
primaryFactoryMethod
    ^#in:lagTimeU:lagTimeD:
!
!

PrimitiveUgen subclass: #Lag2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Lag2 comment: 'Exponential lag'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Lag2 class methodsFor: 'instance creation'!
in: in lagTime: lagTime
    | aLag2 |
    aLag2 := self new.
    aLag2 paramValues: {in. lagTime}.
    ^aLag2
!
in: in lagTime: lagTime mul: mul
    ^(Lag2 in: in lagTime: lagTime) * mul
!
in: in lagTime: lagTime mul: mul add: add
    ^(Lag2 in: in lagTime: lagTime) * mul + add
!
primaryFactoryMethod
    ^#in:lagTime:
!
!

PrimitiveUgen subclass: #Lag3
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Lag3 comment: 'Exponential lag'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Lag3 class methodsFor: 'instance creation'!
in: in lagTime: lagTime
    | aLag3 |
    aLag3 := self new.
    aLag3 paramValues: {in. lagTime}.
    ^aLag3
!
in: in lagTime: lagTime mul: mul
    ^(Lag3 in: in lagTime: lagTime) * mul
!
in: in lagTime: lagTime mul: mul add: add
    ^(Lag3 in: in lagTime: lagTime) * mul + add
!
primaryFactoryMethod
    ^#in:lagTime:
!
!

PrimitiveUgen subclass: #Lag3UD
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Lag3UD comment: 'Exponential lag'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Lag3UD class methodsFor: 'instance creation'!
in: in lagTimeU: lagTimeU lagTimeD: lagTimeD
    | aLag3UD |
    aLag3UD := self new.
    aLag3UD paramValues: {in. lagTimeU. lagTimeD}.
    ^aLag3UD
!
in: in lagTimeU: lagTimeU lagTimeD: lagTimeD mul: mul
    ^(Lag3UD in: in lagTimeU: lagTimeU lagTimeD: lagTimeD) * mul
!
in: in lagTimeU: lagTimeU lagTimeD: lagTimeD mul: mul add: add
    ^(Lag3UD in: in lagTimeU: lagTimeU lagTimeD: lagTimeD) * mul + add
!
primaryFactoryMethod
    ^#in:lagTimeU:lagTimeD:
!
!

PrimitiveUgen subclass: #Latch
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Latch comment: 'Sample and hold'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Latch class methodsFor: 'instance creation'!
in: in trig: trig
    | aLatch |
    aLatch := self new.
    aLatch paramValues: {in. trig}.
    ^aLatch
!
in: in trig: trig mul: mul
    ^(Latch in: in trig: trig) * mul
!
in: in trig: trig mul: mul add: add
    ^(Latch in: in trig: trig) * mul + add
!
primaryFactoryMethod
    ^#in:trig:
!
!

PrimitiveUgen subclass: #LatoocarfianC
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LatoocarfianC comment: 'Latoocarfian chaotic generator'!


!LatoocarfianC methodsFor: 'initialization'!
initialize
    super initialize.
    ugenName := #LatoocarfianC.
    paramNames := #(#freq #a #b #c #d #xi #yi).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    paramValues := {}.
    mceValues := {}.
!
!


!LatoocarfianC class methodsFor: 'instance creation'!
freq: freq a: a b: b c: c d: d xi: xi yi: yi
    | aLatoocarfianC |
    aLatoocarfianC := self new.
    aLatoocarfianC paramValues: {freq. a. b. c. d. xi. yi}.
    ^aLatoocarfianC
!
freq: freq a: a b: b c: c d: d xi: xi yi: yi mul: mul
    ^(LatoocarfianC freq: freq a: a b: b c: c d: d xi: xi yi: yi) * mul
!
freq: freq a: a b: b c: c d: d xi: xi yi: yi mul: mul add: add
    ^(LatoocarfianC freq: freq a: a b: b c: c d: d xi: xi yi: yi) * mul + add
!
primaryFactoryMethod
    ^#freq:a:b:c:d:xi:yi:
!
!

PrimitiveUgen subclass: #LeakDC
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LeakDC comment: 'Remove DC'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!LeakDC class methodsFor: 'instance creation'!
in: in coef: coef
    | aLeakDC |
    aLeakDC := self new.
    aLeakDC paramValues: {in. coef}.
    ^aLeakDC
!
in: in coef: coef mul: mul
    ^(LeakDC in: in coef: coef) * mul
!
in: in coef: coef mul: mul add: add
    ^(LeakDC in: in coef: coef) * mul + add
!
primaryFactoryMethod
    ^#in:coef:
!
!

PrimitiveUgen subclass: #Limiter
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Limiter comment: 'Peak limiter'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Limiter class methodsFor: 'instance creation'!
in: in level: level dur: dur
    | aLimiter |
    aLimiter := self new.
    aLimiter paramValues: {in. level. dur}.
    ^aLimiter
!
in: in level: level dur: dur mul: mul
    ^(Limiter in: in level: level dur: dur) * mul
!
in: in level: level dur: dur mul: mul add: add
    ^(Limiter in: in level: level dur: dur) * mul + add
!
primaryFactoryMethod
    ^#in:level:dur:
!
!

PrimitiveUgen subclass: #LinCongC
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LinCongC comment: 'Linear congruential chaotic generator'!


!LinCongC methodsFor: 'initialization'!
initialize
    super initialize.
    ugenName := #LinCongC.
    paramNames := #(#freq #a #c #m #xi).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    paramValues := {}.
    mceValues := {}.
!
!


!LinCongC class methodsFor: 'instance creation'!
freq: freq a: a c: c m: m xi: xi
    | aLinCongC |
    aLinCongC := self new.
    aLinCongC paramValues: {freq. a. c. m. xi}.
    ^aLinCongC
!
freq: freq a: a c: c m: m xi: xi mul: mul
    ^(LinCongC freq: freq a: a c: c m: m xi: xi) * mul
!
freq: freq a: a c: c m: m xi: xi mul: mul add: add
    ^(LinCongC freq: freq a: a c: c m: m xi: xi) * mul + add
!
primaryFactoryMethod
    ^#freq:a:c:m:xi:
!
!

PrimitiveUgen subclass: #Line
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Line comment: 'Line generator.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Line class methodsFor: 'instance creation'!
start: start end: end dur: dur doneAction: doneAction
    | aLine |
    aLine := self new.
    aLine paramValues: {start. end. dur. doneAction}.
    ^aLine
!
start: start end: end dur: dur doneAction: doneAction mul: mul
    ^(Line start: start end: end dur: dur doneAction: doneAction) * mul
!
start: start end: end dur: dur doneAction: doneAction mul: mul add: add
    ^(Line start: start end: end dur: dur doneAction: doneAction) * mul + add
!
primaryFactoryMethod
    ^#start:end:dur:doneAction:
!
!

PrimitiveUgen subclass: #Linen
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Linen comment: 'Simple linear envelope generator.'!


!Linen methodsFor: 'initialization'!
initialize
    super initialize.
    ugenName := #Linen.
    paramNames := #(#gate #attackTime #susLevel #releaseTime #doneAction).
    mceNames := #().
    ugenRate := #kr.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    paramValues := {}.
    mceValues := {}.
!
!


!Linen class methodsFor: 'instance creation'!
gate: gate attackTime: attackTime susLevel: susLevel releaseTime: releaseTime doneAction: doneAction
    | aLinen |
    aLinen := self new.
    aLinen paramValues: {gate. attackTime. susLevel. releaseTime. doneAction}.
    ^aLinen
!
gate: gate attackTime: attackTime susLevel: susLevel releaseTime: releaseTime doneAction: doneAction mul: mul
    ^(Linen gate: gate attackTime: attackTime susLevel: susLevel releaseTime: releaseTime doneAction: doneAction) * mul
!
gate: gate attackTime: attackTime susLevel: susLevel releaseTime: releaseTime doneAction: doneAction mul: mul add: add
    ^(Linen gate: gate attackTime: attackTime susLevel: susLevel releaseTime: releaseTime doneAction: doneAction) * mul + add
!
primaryFactoryMethod
    ^#gate:attackTime:susLevel:releaseTime:doneAction:
!
!

PrimitiveUgen subclass: #LinExp
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LinExp comment: 'Map a linear range to an exponential range'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!LinExp class methodsFor: 'instance creation'!
in: in srclo: srclo srchi: srchi dstlo: dstlo dsthi: dsthi
    | aLinExp |
    aLinExp := self new.
    aLinExp paramValues: {in. srclo. srchi. dstlo. dsthi}.
    ^aLinExp
!
in: in srclo: srclo srchi: srchi dstlo: dstlo dsthi: dsthi mul: mul
    ^(LinExp in: in srclo: srclo srchi: srchi dstlo: dstlo dsthi: dsthi) * mul
!
in: in srclo: srclo srchi: srchi dstlo: dstlo dsthi: dsthi mul: mul add: add
    ^(LinExp in: in srclo: srclo srchi: srchi dstlo: dstlo dsthi: dsthi) * mul + add
!
primaryFactoryMethod
    ^#in:srclo:srchi:dstlo:dsthi:
!
!

PrimitiveUgen subclass: #LinPan2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LinPan2 comment: 'Two channel linear pan.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!LinPan2 class methodsFor: 'instance creation'!
in: in pos: pos level: level
    | aLinPan2 |
    aLinPan2 := self new.
    aLinPan2 paramValues: {in. pos. level}.
    ^aLinPan2
!
in: in pos: pos level: level mul: mul
    ^(LinPan2 in: in pos: pos level: level) * mul
!
in: in pos: pos level: level mul: mul add: add
    ^(LinPan2 in: in pos: pos level: level) * mul + add
!
primaryFactoryMethod
    ^#in:pos:level:
!
!

PrimitiveUgen subclass: #LinRand
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LinRand comment: 'Skewed random number generator.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!LinRand class methodsFor: 'instance creation'!
lo: lo hi: hi minmax: minmax
    | aLinRand |
    aLinRand := self new.
    aLinRand paramValues: {lo. hi. minmax}.
    ^aLinRand
!
lo: lo hi: hi minmax: minmax mul: mul
    ^(LinRand lo: lo hi: hi minmax: minmax) * mul
!
lo: lo hi: hi minmax: minmax mul: mul add: add
    ^(LinRand lo: lo hi: hi minmax: minmax) * mul + add
!
primaryFactoryMethod
    ^#lo:hi:minmax:
!
!

PrimitiveUgen subclass: #LinXFade2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LinXFade2 comment: 'Two channel linear crossfade.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!LinXFade2 class methodsFor: 'instance creation'!
inA: inA inB: inB pan: pan
    | aLinXFade2 |
    aLinXFade2 := self new.
    aLinXFade2 paramValues: {inA. inB. pan}.
    ^aLinXFade2
!
inA: inA inB: inB pan: pan mul: mul
    ^(LinXFade2 inA: inA inB: inB pan: pan) * mul
!
inA: inA inB: inB pan: pan mul: mul add: add
    ^(LinXFade2 inA: inA inB: inB pan: pan) * mul + add
!
primaryFactoryMethod
    ^#inA:inB:pan:
!
!

PrimitiveUgen subclass: #LocalBuf
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LocalBuf comment: 'Allocate a buffer local to the synth'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!LocalBuf class methodsFor: 'instance creation'!
numChannels: numChannels numFrames: numFrames
    | aLocalBuf |
    aLocalBuf := self new.
    aLocalBuf paramValues: {numChannels. numFrames}.
    ^aLocalBuf
!
numChannels: numChannels numFrames: numFrames mul: mul
    ^(LocalBuf numChannels: numChannels numFrames: numFrames) * mul
!
numChannels: numChannels numFrames: numFrames mul: mul add: add
    ^(LocalBuf numChannels: numChannels numFrames: numFrames) * mul + add
!
primaryFactoryMethod
    ^#numChannels:numFrames:
!
!

PrimitiveUgen subclass: #LocalIn
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LocalIn comment: 'Define and read from buses local to a synth.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!LocalIn class methodsFor: 'instance creation'!
numChan: numChan default: default
    | aLocalIn |
    aLocalIn := self new.
    aLocalIn numChan: numChan.
    aLocalIn paramValues: {}.
    aLocalIn mceValues: {default}.
    ^aLocalIn
!
numChan: numChan default: default mul: mul
    ^(LocalIn numChan: numChan default: default) * mul
!
numChan: numChan default: default mul: mul add: add
    ^(LocalIn numChan: numChan default: default) * mul + add
!
primaryFactoryMethod
    ^#numChan:default:
!
!

PrimitiveUgen subclass: #LocalOut
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LocalOut comment: 'Write to buses local to a synth.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!LocalOut class methodsFor: 'instance creation'!
channelsArray: channelsArray
    | aLocalOut |
    aLocalOut := self new.
    aLocalOut paramValues: {}.
    aLocalOut mceValues: {channelsArray}.
    ^aLocalOut
!
channelsArray: channelsArray mul: mul
    ^(LocalOut channelsArray: channelsArray) * mul
!
channelsArray: channelsArray mul: mul add: add
    ^(LocalOut channelsArray: channelsArray) * mul + add
!
primaryFactoryMethod
    ^#channelsArray:
!
!

PrimitiveUgen subclass: #LorenzL
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LorenzL comment: 'Lorenz chaotic generator'!


!LorenzL methodsFor: 'initialization'!
initialize
    super initialize.
    ugenName := #LorenzL.
    paramNames := #(#freq #s #r #b #h #xi #yi #zi).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    paramValues := {}.
    mceValues := {}.
!
!


!LorenzL class methodsFor: 'instance creation'!
freq: freq s: s r: r b: b h: h xi: xi yi: yi zi: zi
    | aLorenzL |
    aLorenzL := self new.
    aLorenzL paramValues: {freq. s. r. b. h. xi. yi. zi}.
    ^aLorenzL
!
freq: freq s: s r: r b: b h: h xi: xi yi: yi zi: zi mul: mul
    ^(LorenzL freq: freq s: s r: r b: b h: h xi: xi yi: yi zi: zi) * mul
!
freq: freq s: s r: r b: b h: h xi: xi yi: yi zi: zi mul: mul add: add
    ^(LorenzL freq: freq s: s r: r b: b h: h xi: xi yi: yi zi: zi) * mul + add
!
primaryFactoryMethod
    ^#freq:s:r:b:h:xi:yi:zi:
!
!

PrimitiveUgen subclass: #LPZ1
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LPZ1 comment: 'Two point average filter'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!LPZ1 class methodsFor: 'instance creation'!
in: in
    | aLPZ1 |
    aLPZ1 := self new.
    aLPZ1 paramValues: {in}.
    ^aLPZ1
!
in: in mul: mul
    ^(LPZ1 in: in) * mul
!
in: in mul: mul add: add
    ^(LPZ1 in: in) * mul + add
!
primaryFactoryMethod
    ^#in:
!
!

PrimitiveUgen subclass: #LPZ2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LPZ2 comment: 'Two zero fixed lowpass'!


!LPZ2 methodsFor: 'initialization'!
initialize
    super initialize.
    ugenName := #LPZ2.
    paramNames := #(#in).
    mceNames := #().
    ugenRate := #(0).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    paramValues := {}.
    mceValues := {}.
!
!


!LPZ2 class methodsFor: 'instance creation'!
in: in
    | aLPZ2 |
    aLPZ2 := self new.
    aLPZ2 paramValues: {in}.
    ^aLPZ2
!
in: in mul: mul
    ^(LPZ2 in: in) * mul
!
in: in mul: mul add: add
    ^(LPZ2 in: in) * mul + add
!
primaryFactoryMethod
    ^#in:
!
!

PrimitiveUgen subclass: #MantissaMask
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

MantissaMask comment: 'Reduce precision.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!MantissaMask class methodsFor: 'instance creation'!
in: in bits: bits
    | aMantissaMask |
    aMantissaMask := self new.
    aMantissaMask paramValues: {in. bits}.
    ^aMantissaMask
!
in: in bits: bits mul: mul
    ^(MantissaMask in: in bits: bits) * mul
!
in: in bits: bits mul: mul add: add
    ^(MantissaMask in: in bits: bits) * mul + add
!
primaryFactoryMethod
    ^#in:bits:
!
!

PrimitiveUgen subclass: #MaxLocalBufs
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

MaxLocalBufs comment: 'LocalBuf count'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!MaxLocalBufs class methodsFor: 'instance creation'!
count: count
    | aMaxLocalBufs |
    aMaxLocalBufs := self new.
    aMaxLocalBufs paramValues: {count}.
    ^aMaxLocalBufs
!
count: count mul: mul
    ^(MaxLocalBufs count: count) * mul
!
count: count mul: mul add: add
    ^(MaxLocalBufs count: count) * mul + add
!
primaryFactoryMethod
    ^#count:
!
!

PrimitiveUgen subclass: #Median
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Median comment: 'Median filter.'!


!Median methodsFor: 'initialization'!
initialize
    super initialize.
    ugenName := #Median.
    paramNames := #(#length #in).
    mceNames := #().
    ugenRate := #(1).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    paramValues := {}.
    mceValues := {}.
!
!


!Median class methodsFor: 'instance creation'!
length: length in: in
    | aMedian |
    aMedian := self new.
    aMedian paramValues: {length. in}.
    ^aMedian
!
length: length in: in mul: mul
    ^(Median length: length in: in) * mul
!
length: length in: in mul: mul add: add
    ^(Median length: length in: in) * mul + add
!
primaryFactoryMethod
    ^#length:in:
!
!

PrimitiveUgen subclass: #ModDif
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

ModDif comment: 'Minimum difference of two values in modulo arithmetics'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!ModDif class methodsFor: 'instance creation'!
x: x y: y mod: mod
    | aModDif |
    aModDif := self new.
    aModDif paramValues: {x. y. mod}.
    ^aModDif
!
x: x y: y mod: mod mul: mul
    ^(ModDif x: x y: y mod: mod) * mul
!
x: x y: y mod: mod mul: mul add: add
    ^(ModDif x: x y: y mod: mod) * mul + add
!
primaryFactoryMethod
    ^#x:y:mod:
!
!

PrimitiveUgen subclass: #MoogFF
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

MoogFF comment: 'Moog VCF implementation, designed by Federico Fontana'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!MoogFF class methodsFor: 'instance creation'!
in: in freq: freq gain: gain reset: reset
    | aMoogFF |
    aMoogFF := self new.
    aMoogFF paramValues: {in. freq. gain. reset}.
    ^aMoogFF
!
in: in freq: freq gain: gain reset: reset mul: mul
    ^(MoogFF in: in freq: freq gain: gain reset: reset) * mul
!
in: in freq: freq gain: gain reset: reset mul: mul add: add
    ^(MoogFF in: in freq: freq gain: gain reset: reset) * mul + add
!
primaryFactoryMethod
    ^#in:freq:gain:reset:
!
!

PrimitiveUgen subclass: #MouseButton
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

MouseButton comment: 'Mouse button UGen.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!MouseButton class methodsFor: 'instance creation'!
minval: minval maxval: maxval lag: lag
    | aMouseButton |
    aMouseButton := self new.
    aMouseButton paramValues: {minval. maxval. lag}.
    ^aMouseButton
!
minval: minval maxval: maxval lag: lag mul: mul
    ^(MouseButton minval: minval maxval: maxval lag: lag) * mul
!
minval: minval maxval: maxval lag: lag mul: mul add: add
    ^(MouseButton minval: minval maxval: maxval lag: lag) * mul + add
!
primaryFactoryMethod
    ^#minval:maxval:lag:
!
!

PrimitiveUgen subclass: #MouseX
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

MouseX comment: 'Cursor tracking UGen.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!MouseX class methodsFor: 'instance creation'!
minval: minval maxval: maxval warp: warp lag: lag
    | aMouseX |
    aMouseX := self new.
    aMouseX paramValues: {minval. maxval. warp. lag}.
    ^aMouseX
!
minval: minval maxval: maxval warp: warp lag: lag mul: mul
    ^(MouseX minval: minval maxval: maxval warp: warp lag: lag) * mul
!
minval: minval maxval: maxval warp: warp lag: lag mul: mul add: add
    ^(MouseX minval: minval maxval: maxval warp: warp lag: lag) * mul + add
!
primaryFactoryMethod
    ^#minval:maxval:warp:lag:
!
!

PrimitiveUgen subclass: #MouseY
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

MouseY comment: 'Cursor tracking UGen.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!MouseY class methodsFor: 'instance creation'!
minval: minval maxval: maxval warp: warp lag: lag
    | aMouseY |
    aMouseY := self new.
    aMouseY paramValues: {minval. maxval. warp. lag}.
    ^aMouseY
!
minval: minval maxval: maxval warp: warp lag: lag mul: mul
    ^(MouseY minval: minval maxval: maxval warp: warp lag: lag) * mul
!
minval: minval maxval: maxval warp: warp lag: lag mul: mul add: add
    ^(MouseY minval: minval maxval: maxval warp: warp lag: lag) * mul + add
!
primaryFactoryMethod
    ^#minval:maxval:warp:lag:
!
!

PrimitiveUgen subclass: #MulAdd
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

MulAdd comment: 'Multiply add'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!MulAdd class methodsFor: 'instance creation'!
in: in mul: mul add: add
    | aMulAdd |
    ((mul = 1) and: (add = 0)) ifTrue: [ ^in ].
    aMulAdd := self new.
    aMulAdd paramValues: {in. mul. add}.
    ^aMulAdd
!
primaryFactoryMethod
    ^#in:mul:add:
!
!

PrimitiveUgen subclass: #Normalizer
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Normalizer comment: 'Flattens dynamics.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Normalizer class methodsFor: 'instance creation'!
in: in level: level dur: dur
    | aNormalizer |
    aNormalizer := self new.
    aNormalizer paramValues: {in. level. dur}.
    ^aNormalizer
!
in: in level: level dur: dur mul: mul
    ^(Normalizer in: in level: level dur: dur) * mul
!
in: in level: level dur: dur mul: mul add: add
    ^(Normalizer in: in level: level dur: dur) * mul + add
!
primaryFactoryMethod
    ^#in:level:dur:
!
!

PrimitiveUgen subclass: #NRand
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

NRand comment: 'Sum of uniform distributions.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!NRand class methodsFor: 'instance creation'!
lo: lo hi: hi n: n
    | aNRand |
    aNRand := self new.
    aNRand paramValues: {lo. hi. n}.
    ^aNRand
!
lo: lo hi: hi n: n mul: mul
    ^(NRand lo: lo hi: hi n: n) * mul
!
lo: lo hi: hi n: n mul: mul add: add
    ^(NRand lo: lo hi: hi n: n) * mul + add
!
primaryFactoryMethod
    ^#lo:hi:n:
!
!

PrimitiveUgen subclass: #NumOutputBuses
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

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
    paramValues := {}.
    mceValues := {}.
!
!


!NumOutputBuses class methodsFor: 'instance creation'!
mul: mul
    ^(NumOutputBuses new) * mul
!
mul: mul add: add
    ^(NumOutputBuses new) * mul + add
!
primaryFactoryMethod
    ^#new
!
!

PrimitiveUgen subclass: #OnePole
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

OnePole comment: 'One pole filter.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!OnePole class methodsFor: 'instance creation'!
in: in coef: coef
    | anOnePole |
    anOnePole := self new.
    anOnePole paramValues: {in. coef}.
    ^anOnePole
!
in: in coef: coef mul: mul
    ^(OnePole in: in coef: coef) * mul
!
in: in coef: coef mul: mul add: add
    ^(OnePole in: in coef: coef) * mul + add
!
primaryFactoryMethod
    ^#in:coef:
!
!

PrimitiveUgen subclass: #OneZero
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

OneZero comment: 'One zero filter.'!


!OneZero methodsFor: 'initialization'!
initialize
    super initialize.
    ugenName := #OneZero.
    paramNames := #(#in #coef).
    mceNames := #().
    ugenRate := #(0).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    paramValues := {}.
    mceValues := {}.
!
!


!OneZero class methodsFor: 'instance creation'!
in: in coef: coef
    | anOneZero |
    anOneZero := self new.
    anOneZero paramValues: {in. coef}.
    ^anOneZero
!
in: in coef: coef mul: mul
    ^(OneZero in: in coef: coef) * mul
!
in: in coef: coef mul: mul add: add
    ^(OneZero in: in coef: coef) * mul + add
!
primaryFactoryMethod
    ^#in:coef:
!
!

PrimitiveUgen subclass: #Osc
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Osc comment: 'Interpolating wavetable oscillator.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Osc class methodsFor: 'instance creation'!
bufnum: bufnum freq: freq phase: phase
    | anOsc |
    anOsc := self new.
    anOsc paramValues: {bufnum. freq. phase}.
    ^anOsc
!
bufnum: bufnum freq: freq phase: phase mul: mul
    ^(Osc bufnum: bufnum freq: freq phase: phase) * mul
!
bufnum: bufnum freq: freq phase: phase mul: mul add: add
    ^(Osc bufnum: bufnum freq: freq phase: phase) * mul + add
!
primaryFactoryMethod
    ^#bufnum:freq:phase:
!
!

PrimitiveUgen subclass: #Out
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Out comment: 'Write a signal to a bus.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Out class methodsFor: 'instance creation'!
bus: bus channelsArray: channelsArray
    | anOut |
    anOut := self new.
    anOut paramValues: {bus}.
    anOut mceValues: {channelsArray}.
    ^anOut
!
bus: bus channelsArray: channelsArray mul: mul
    ^(Out bus: bus channelsArray: channelsArray) * mul
!
bus: bus channelsArray: channelsArray mul: mul add: add
    ^(Out bus: bus channelsArray: channelsArray) * mul + add
!
primaryFactoryMethod
    ^#bus:channelsArray:
!
!

PrimitiveUgen subclass: #Pan2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Pan2 comment: 'Two channel equal power pan.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Pan2 class methodsFor: 'instance creation'!
in: in pos: pos level: level
    | aPan2 |
    aPan2 := self new.
    aPan2 paramValues: {in. pos. level}.
    ^aPan2
!
in: in pos: pos level: level mul: mul
    ^(Pan2 in: in pos: pos level: level) * mul
!
in: in pos: pos level: level mul: mul add: add
    ^(Pan2 in: in pos: pos level: level) * mul + add
!
primaryFactoryMethod
    ^#in:pos:level:
!
!

PrimitiveUgen subclass: #PanAz
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PanAz comment: 'Azimuth panner'!


!PanAz methodsFor: 'initialization'!
initialize
    super initialize.
    ugenName := #PanAz.
    paramNames := #(#in #pos #level #width #orientation).
    mceNames := #().
    ugenRate := #(0).
    numChan := nil.
    specialIndex := 0.
    ugenId := nil.
    paramValues := {}.
    mceValues := {}.
!
!


!PanAz class methodsFor: 'instance creation'!
numChan: numChan in: in pos: pos level: level width: width orientation: orientation
    | aPanAz |
    aPanAz := self new.
    aPanAz numChan: numChan.
    aPanAz paramValues: {in. pos. level. width. orientation}.
    ^aPanAz
!
numChan: numChan in: in pos: pos level: level width: width orientation: orientation mul: mul
    ^(PanAz numChan: numChan in: in pos: pos level: level width: width orientation: orientation) * mul
!
numChan: numChan in: in pos: pos level: level width: width orientation: orientation mul: mul add: add
    ^(PanAz numChan: numChan in: in pos: pos level: level width: width orientation: orientation) * mul + add
!
primaryFactoryMethod
    ^#numChan:in:pos:level:width:orientation:
!
!

PrimitiveUgen subclass: #PanB
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PanB comment: 'Ambisonic B-format panner.'!


!PanB methodsFor: 'initialization'!
initialize
    super initialize.
    ugenName := #PanB.
    paramNames := #(#in #azimuth #elevation #gain).
    mceNames := #().
    ugenRate := #ar.
    numChan := 4.
    specialIndex := 0.
    ugenId := nil.
    paramValues := {}.
    mceValues := {}.
!
!


!PanB class methodsFor: 'instance creation'!
in: in azimuth: azimuth elevation: elevation gain: gain
    | aPanB |
    aPanB := self new.
    aPanB paramValues: {in. azimuth. elevation. gain}.
    ^aPanB
!
in: in azimuth: azimuth elevation: elevation gain: gain mul: mul
    ^(PanB in: in azimuth: azimuth elevation: elevation gain: gain) * mul
!
in: in azimuth: azimuth elevation: elevation gain: gain mul: mul add: add
    ^(PanB in: in azimuth: azimuth elevation: elevation gain: gain) * mul + add
!
primaryFactoryMethod
    ^#in:azimuth:elevation:gain:
!
!

PrimitiveUgen subclass: #PeakFollower
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PeakFollower comment: 'Track peak signal amplitude.'!


!PeakFollower methodsFor: 'initialization'!
initialize
    super initialize.
    ugenName := #PeakFollower.
    paramNames := #(#in #decay).
    mceNames := #().
    ugenRate := #(0).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    paramValues := {}.
    mceValues := {}.
!
!


!PeakFollower class methodsFor: 'instance creation'!
in: in decay: decay
    | aPeakFollower |
    aPeakFollower := self new.
    aPeakFollower paramValues: {in. decay}.
    ^aPeakFollower
!
in: in decay: decay mul: mul
    ^(PeakFollower in: in decay: decay) * mul
!
in: in decay: decay mul: mul add: add
    ^(PeakFollower in: in decay: decay) * mul + add
!
primaryFactoryMethod
    ^#in:decay:
!
!

PrimitiveUgen subclass: #Perlin3
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Perlin3 comment: '3D Perlin Noise'!


!Perlin3 methodsFor: 'initialization'!
initialize
    super initialize.
    ugenName := #Perlin3.
    paramNames := #(#x #y #z).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    paramValues := {}.
    mceValues := {}.
!
!


!Perlin3 class methodsFor: 'instance creation'!
x: x y: y z: z
    | aPerlin3 |
    aPerlin3 := self new.
    aPerlin3 paramValues: {x. y. z}.
    ^aPerlin3
!
x: x y: y z: z mul: mul
    ^(Perlin3 x: x y: y z: z) * mul
!
x: x y: y z: z mul: mul add: add
    ^(Perlin3 x: x y: y z: z) * mul + add
!
primaryFactoryMethod
    ^#x:y:z:
!
!

PrimitiveUgen subclass: #Phasor
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Phasor comment: 'A resettable linear ramp between two levels.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Phasor class methodsFor: 'instance creation'!
trig: trig rate: rate start: start end: end resetPos: resetPos
    | aPhasor |
    aPhasor := self new.
    aPhasor paramValues: {trig. rate. start. end. resetPos}.
    ^aPhasor
!
trig: trig rate: rate start: start end: end resetPos: resetPos mul: mul
    ^(Phasor trig: trig rate: rate start: start end: end resetPos: resetPos) * mul
!
trig: trig rate: rate start: start end: end resetPos: resetPos mul: mul add: add
    ^(Phasor trig: trig rate: rate start: start end: end resetPos: resetPos) * mul + add
!
primaryFactoryMethod
    ^#trig:rate:start:end:resetPos:
!
!

PrimitiveUgen subclass: #PinkNoise
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

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
    paramValues := {}.
    mceValues := {}.
!
!


!PinkNoise class methodsFor: 'instance creation'!
mul: mul
    ^(PinkNoise new) * mul
!
mul: mul add: add
    ^(PinkNoise new) * mul + add
!
primaryFactoryMethod
    ^#new
!
!

PrimitiveUgen subclass: #Pitch
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Pitch comment: 'Autocorrelation pitch follower'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Pitch class methodsFor: 'instance creation'!
in: in initFreq: initFreq minFreq: minFreq maxFreq: maxFreq execFreq: execFreq maxBinsPerOctave: maxBinsPerOctave median: median ampThreshold: ampThreshold peakThreshold: peakThreshold downSample: downSample clar: clar
    | aPitch |
    aPitch := self new.
    aPitch paramValues: {in. initFreq. minFreq. maxFreq. execFreq. maxBinsPerOctave. median. ampThreshold. peakThreshold. downSample. clar}.
    ^aPitch
!
in: in initFreq: initFreq minFreq: minFreq maxFreq: maxFreq execFreq: execFreq maxBinsPerOctave: maxBinsPerOctave median: median ampThreshold: ampThreshold peakThreshold: peakThreshold downSample: downSample clar: clar mul: mul
    ^(Pitch in: in initFreq: initFreq minFreq: minFreq maxFreq: maxFreq execFreq: execFreq maxBinsPerOctave: maxBinsPerOctave median: median ampThreshold: ampThreshold peakThreshold: peakThreshold downSample: downSample clar: clar) * mul
!
in: in initFreq: initFreq minFreq: minFreq maxFreq: maxFreq execFreq: execFreq maxBinsPerOctave: maxBinsPerOctave median: median ampThreshold: ampThreshold peakThreshold: peakThreshold downSample: downSample clar: clar mul: mul add: add
    ^(Pitch in: in initFreq: initFreq minFreq: minFreq maxFreq: maxFreq execFreq: execFreq maxBinsPerOctave: maxBinsPerOctave median: median ampThreshold: ampThreshold peakThreshold: peakThreshold downSample: downSample clar: clar) * mul + add
!
primaryFactoryMethod
    ^#in:initFreq:minFreq:maxFreq:execFreq:maxBinsPerOctave:median:ampThreshold:peakThreshold:downSample:clar:
!
!

PrimitiveUgen subclass: #PitchShift
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PitchShift comment: 'Time domain pitch shifter.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!PitchShift class methodsFor: 'instance creation'!
in: in windowSize: windowSize pitchRatio: pitchRatio pitchDispersion: pitchDispersion timeDispersion: timeDispersion
    | aPitchShift |
    aPitchShift := self new.
    aPitchShift paramValues: {in. windowSize. pitchRatio. pitchDispersion. timeDispersion}.
    ^aPitchShift
!
in: in windowSize: windowSize pitchRatio: pitchRatio pitchDispersion: pitchDispersion timeDispersion: timeDispersion mul: mul
    ^(PitchShift in: in windowSize: windowSize pitchRatio: pitchRatio pitchDispersion: pitchDispersion timeDispersion: timeDispersion) * mul
!
in: in windowSize: windowSize pitchRatio: pitchRatio pitchDispersion: pitchDispersion timeDispersion: timeDispersion mul: mul add: add
    ^(PitchShift in: in windowSize: windowSize pitchRatio: pitchRatio pitchDispersion: pitchDispersion timeDispersion: timeDispersion) * mul + add
!
primaryFactoryMethod
    ^#in:windowSize:pitchRatio:pitchDispersion:timeDispersion:
!
!

PrimitiveUgen subclass: #PlayBuf
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PlayBuf comment: 'Sample playback oscillator.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!PlayBuf class methodsFor: 'instance creation'!
numChan: numChan bufnum: bufnum rate: rate trigger: trigger startPos: startPos loop: loop doneAction: doneAction
    | aPlayBuf |
    aPlayBuf := self new.
    aPlayBuf numChan: numChan.
    aPlayBuf paramValues: {bufnum. rate. trigger. startPos. loop. doneAction}.
    ^aPlayBuf
!
numChan: numChan bufnum: bufnum rate: rate trigger: trigger startPos: startPos loop: loop doneAction: doneAction mul: mul
    ^(PlayBuf numChan: numChan bufnum: bufnum rate: rate trigger: trigger startPos: startPos loop: loop doneAction: doneAction) * mul
!
numChan: numChan bufnum: bufnum rate: rate trigger: trigger startPos: startPos loop: loop doneAction: doneAction mul: mul add: add
    ^(PlayBuf numChan: numChan bufnum: bufnum rate: rate trigger: trigger startPos: startPos loop: loop doneAction: doneAction) * mul + add
!
primaryFactoryMethod
    ^#numChan:bufnum:rate:trigger:startPos:loop:doneAction:
!
!

PrimitiveUgen subclass: #Pluck
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Pluck comment: 'A Karplus-Strong UGen'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Pluck class methodsFor: 'instance creation'!
in: in trig: trig maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime coef: coef
    | aPluck |
    aPluck := self new.
    aPluck paramValues: {in. trig. maxdelaytime. delaytime. decaytime. coef}.
    ^aPluck
!
in: in trig: trig maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime coef: coef mul: mul
    ^(Pluck in: in trig: trig maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime coef: coef) * mul
!
in: in trig: trig maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime coef: coef mul: mul add: add
    ^(Pluck in: in trig: trig maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime coef: coef) * mul + add
!
primaryFactoryMethod
    ^#in:trig:maxdelaytime:delaytime:decaytime:coef:
!
!

PrimitiveUgen subclass: #Pulse
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Pulse comment: 'Band limited pulse wave.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Pulse class methodsFor: 'instance creation'!
freq: freq width: width
    | aPulse |
    aPulse := self new.
    aPulse paramValues: {freq. width}.
    ^aPulse
!
freq: freq width: width mul: mul
    ^(Pulse freq: freq width: width) * mul
!
freq: freq width: width mul: mul add: add
    ^(Pulse freq: freq width: width) * mul + add
!
primaryFactoryMethod
    ^#freq:width:
!
!

PrimitiveUgen subclass: #PulseCount
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PulseCount comment: 'Pulse counter.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!PulseCount class methodsFor: 'instance creation'!
trig: trig reset: reset
    | aPulseCount |
    aPulseCount := self new.
    aPulseCount paramValues: {trig. reset}.
    ^aPulseCount
!
trig: trig reset: reset mul: mul
    ^(PulseCount trig: trig reset: reset) * mul
!
trig: trig reset: reset mul: mul add: add
    ^(PulseCount trig: trig reset: reset) * mul + add
!
primaryFactoryMethod
    ^#trig:reset:
!
!

PrimitiveUgen subclass: #PulseDivider
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PulseDivider comment: 'Pulse divider.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!PulseDivider class methodsFor: 'instance creation'!
trig: trig div: div start: start
    | aPulseDivider |
    aPulseDivider := self new.
    aPulseDivider paramValues: {trig. div. start}.
    ^aPulseDivider
!
trig: trig div: div start: start mul: mul
    ^(PulseDivider trig: trig div: div start: start) * mul
!
trig: trig div: div start: start mul: mul add: add
    ^(PulseDivider trig: trig div: div start: start) * mul + add
!
primaryFactoryMethod
    ^#trig:div:start:
!
!

PrimitiveUgen subclass: #PV_RandComb
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PV_RandComb comment: 'Pass random bins.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!PV_RandComb class methodsFor: 'instance creation'!
buffer: buffer wipe: wipe trig: trig
    | aPV_RandComb |
    aPV_RandComb := self new.
    aPV_RandComb paramValues: {buffer. wipe. trig}.
    ^aPV_RandComb
!
buffer: buffer wipe: wipe trig: trig mul: mul
    ^(PV_RandComb buffer: buffer wipe: wipe trig: trig) * mul
!
buffer: buffer wipe: wipe trig: trig mul: mul add: add
    ^(PV_RandComb buffer: buffer wipe: wipe trig: trig) * mul + add
!
primaryFactoryMethod
    ^#buffer:wipe:trig:
!
!

PrimitiveUgen subclass: #QuadL
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

QuadL comment: 'General quadratic map chaotic generator'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!QuadL class methodsFor: 'instance creation'!
freq: freq a: a b: b c: c xi: xi
    | aQuadL |
    aQuadL := self new.
    aQuadL paramValues: {freq. a. b. c. xi}.
    ^aQuadL
!
freq: freq a: a b: b c: c xi: xi mul: mul
    ^(QuadL freq: freq a: a b: b c: c xi: xi) * mul
!
freq: freq a: a b: b c: c xi: xi mul: mul add: add
    ^(QuadL freq: freq a: a b: b c: c xi: xi) * mul + add
!
primaryFactoryMethod
    ^#freq:a:b:c:xi:
!
!

PrimitiveUgen subclass: #QuadC
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

QuadC comment: 'General quadratic map chaotic generator'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!QuadC class methodsFor: 'instance creation'!
freq: freq a: a b: b c: c xi: xi
    | aQuadC |
    aQuadC := self new.
    aQuadC paramValues: {freq. a. b. c. xi}.
    ^aQuadC
!
freq: freq a: a b: b c: c xi: xi mul: mul
    ^(QuadC freq: freq a: a b: b c: c xi: xi) * mul
!
freq: freq a: a b: b c: c xi: xi mul: mul add: add
    ^(QuadC freq: freq a: a b: b c: c xi: xi) * mul + add
!
primaryFactoryMethod
    ^#freq:a:b:c:xi:
!
!

PrimitiveUgen subclass: #RHPF
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

RHPF comment: 'A resonant high pass filter.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!RHPF class methodsFor: 'instance creation'!
in: in freq: freq rq: rq
    | aRHPF |
    aRHPF := self new.
    aRHPF paramValues: {in. freq. rq}.
    ^aRHPF
!
in: in freq: freq rq: rq mul: mul
    ^(RHPF in: in freq: freq rq: rq) * mul
!
in: in freq: freq rq: rq mul: mul add: add
    ^(RHPF in: in freq: freq rq: rq) * mul + add
!
primaryFactoryMethod
    ^#in:freq:rq:
!
!

PrimitiveUgen subclass: #RLPF
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

RLPF comment: 'A resonant low pass filter.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!RLPF class methodsFor: 'instance creation'!
in: in freq: freq rq: rq
    | aRLPF |
    aRLPF := self new.
    aRLPF paramValues: {in. freq. rq}.
    ^aRLPF
!
in: in freq: freq rq: rq mul: mul
    ^(RLPF in: in freq: freq rq: rq) * mul
!
in: in freq: freq rq: rq mul: mul add: add
    ^(RLPF in: in freq: freq rq: rq) * mul + add
!
primaryFactoryMethod
    ^#in:freq:rq:
!
!

PrimitiveUgen subclass: #Rand
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Rand comment: 'Single random number generator.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Rand class methodsFor: 'instance creation'!
lo: lo hi: hi
    | aRand |
    aRand := self new.
    aRand paramValues: {lo. hi}.
    ^aRand
!
lo: lo hi: hi mul: mul
    ^(Rand lo: lo hi: hi) * mul
!
lo: lo hi: hi mul: mul add: add
    ^(Rand lo: lo hi: hi) * mul + add
!
primaryFactoryMethod
    ^#lo:hi:
!
!

PrimitiveUgen subclass: #RecordBuf
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

RecordBuf comment: 'Record or overdub into a Buffer.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!RecordBuf class methodsFor: 'instance creation'!
bufnum: bufnum offset: offset recLevel: recLevel preLevel: preLevel run: run loop: loop trigger: trigger doneAction: doneAction inputArray: inputArray
    | aRecordBuf |
    aRecordBuf := self new.
    aRecordBuf paramValues: {bufnum. offset. recLevel. preLevel. run. loop. trigger. doneAction}.
    aRecordBuf mceValues: {inputArray}.
    ^aRecordBuf
!
bufnum: bufnum offset: offset recLevel: recLevel preLevel: preLevel run: run loop: loop trigger: trigger doneAction: doneAction inputArray: inputArray mul: mul
    ^(RecordBuf bufnum: bufnum offset: offset recLevel: recLevel preLevel: preLevel run: run loop: loop trigger: trigger doneAction: doneAction inputArray: inputArray) * mul
!
bufnum: bufnum offset: offset recLevel: recLevel preLevel: preLevel run: run loop: loop trigger: trigger doneAction: doneAction inputArray: inputArray mul: mul add: add
    ^(RecordBuf bufnum: bufnum offset: offset recLevel: recLevel preLevel: preLevel run: run loop: loop trigger: trigger doneAction: doneAction inputArray: inputArray) * mul + add
!
primaryFactoryMethod
    ^#bufnum:offset:recLevel:preLevel:run:loop:trigger:doneAction:inputArray:
!
!

PrimitiveUgen subclass: #ReplaceOut
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

ReplaceOut comment: 'Send signal to a bus, overwriting previous contents.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!ReplaceOut class methodsFor: 'instance creation'!
bus: bus channelsArray: channelsArray
    | aReplaceOut |
    aReplaceOut := self new.
    aReplaceOut paramValues: {bus}.
    aReplaceOut mceValues: {channelsArray}.
    ^aReplaceOut
!
bus: bus channelsArray: channelsArray mul: mul
    ^(ReplaceOut bus: bus channelsArray: channelsArray) * mul
!
bus: bus channelsArray: channelsArray mul: mul add: add
    ^(ReplaceOut bus: bus channelsArray: channelsArray) * mul + add
!
primaryFactoryMethod
    ^#bus:channelsArray:
!
!

PrimitiveUgen subclass: #Resonz
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Resonz comment: 'Resonant filter.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Resonz class methodsFor: 'instance creation'!
in: in freq: freq bwr: bwr
    | aResonz |
    aResonz := self new.
    aResonz paramValues: {in. freq. bwr}.
    ^aResonz
!
in: in freq: freq bwr: bwr mul: mul
    ^(Resonz in: in freq: freq bwr: bwr) * mul
!
in: in freq: freq bwr: bwr mul: mul add: add
    ^(Resonz in: in freq: freq bwr: bwr) * mul + add
!
primaryFactoryMethod
    ^#in:freq:bwr:
!
!

PrimitiveUgen subclass: #Ringz
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Ringz comment: 'Ringing filter.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Ringz class methodsFor: 'instance creation'!
in: in freq: freq decaytime: decaytime
    | aRingz |
    aRingz := self new.
    aRingz paramValues: {in. freq. decaytime}.
    ^aRingz
!
in: in freq: freq decaytime: decaytime mul: mul
    ^(Ringz in: in freq: freq decaytime: decaytime) * mul
!
in: in freq: freq decaytime: decaytime mul: mul add: add
    ^(Ringz in: in freq: freq decaytime: decaytime) * mul + add
!
primaryFactoryMethod
    ^#in:freq:decaytime:
!
!

PrimitiveUgen subclass: #RunningMax
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

RunningMax comment: 'Track maximum level.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!RunningMax class methodsFor: 'instance creation'!
in: in trig: trig
    | aRunningMax |
    aRunningMax := self new.
    aRunningMax paramValues: {in. trig}.
    ^aRunningMax
!
in: in trig: trig mul: mul
    ^(RunningMax in: in trig: trig) * mul
!
in: in trig: trig mul: mul add: add
    ^(RunningMax in: in trig: trig) * mul + add
!
primaryFactoryMethod
    ^#in:trig:
!
!

PrimitiveUgen subclass: #RunningSum
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

RunningSum comment: 'Running sum over n frames'!


!RunningSum methodsFor: 'initialization'!
initialize
    super initialize.
    ugenName := #RunningSum.
    paramNames := #(#in #numsamp).
    mceNames := #().
    ugenRate := #(0).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    paramValues := {}.
    mceValues := {}.
!
!


!RunningSum class methodsFor: 'instance creation'!
in: in numsamp: numsamp
    | aRunningSum |
    aRunningSum := self new.
    aRunningSum paramValues: {in. numsamp}.
    ^aRunningSum
!
in: in numsamp: numsamp mul: mul
    ^(RunningSum in: in numsamp: numsamp) * mul
!
in: in numsamp: numsamp mul: mul add: add
    ^(RunningSum in: in numsamp: numsamp) * mul + add
!
primaryFactoryMethod
    ^#in:numsamp:
!
!

PrimitiveUgen subclass: #Rotate2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Rotate2 comment: 'Rotate a sound field.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Rotate2 class methodsFor: 'instance creation'!
x: x y: y pos: pos
    | aRotate2 |
    aRotate2 := self new.
    aRotate2 paramValues: {x. y. pos}.
    ^aRotate2
!
x: x y: y pos: pos mul: mul
    ^(Rotate2 x: x y: y pos: pos) * mul
!
x: x y: y pos: pos mul: mul add: add
    ^(Rotate2 x: x y: y pos: pos) * mul + add
!
primaryFactoryMethod
    ^#x:y:pos:
!
!

PrimitiveUgen subclass: #SampleDur
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

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
    paramValues := {}.
    mceValues := {}.
!
!


!SampleDur class methodsFor: 'instance creation'!
mul: mul
    ^(SampleDur new) * mul
!
mul: mul add: add
    ^(SampleDur new) * mul + add
!
primaryFactoryMethod
    ^#new
!
!

PrimitiveUgen subclass: #SampleRate
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

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
    paramValues := {}.
    mceValues := {}.
!
!


!SampleRate class methodsFor: 'instance creation'!
mul: mul
    ^(SampleRate new) * mul
!
mul: mul add: add
    ^(SampleRate new) * mul + add
!
primaryFactoryMethod
    ^#new
!
!

PrimitiveUgen subclass: #Saw
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Saw comment: 'Band limited sawtooth.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Saw class methodsFor: 'instance creation'!
freq: freq
    | aSaw |
    aSaw := self new.
    aSaw paramValues: {freq}.
    ^aSaw
!
freq: freq mul: mul
    ^(Saw freq: freq) * mul
!
freq: freq mul: mul add: add
    ^(Saw freq: freq) * mul + add
!
primaryFactoryMethod
    ^#freq:
!
!

PrimitiveUgen subclass: #Schmidt
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Schmidt comment: 'Schmidt trigger.'!


!Schmidt methodsFor: 'initialization'!
initialize
    super initialize.
    ugenName := #Schmidt.
    paramNames := #(#in #lo #hi).
    mceNames := #().
    ugenRate := #(0).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    paramValues := {}.
    mceValues := {}.
!
!


!Schmidt class methodsFor: 'instance creation'!
in: in lo: lo hi: hi
    | aSchmidt |
    aSchmidt := self new.
    aSchmidt paramValues: {in. lo. hi}.
    ^aSchmidt
!
in: in lo: lo hi: hi mul: mul
    ^(Schmidt in: in lo: lo hi: hi) * mul
!
in: in lo: lo hi: hi mul: mul add: add
    ^(Schmidt in: in lo: lo hi: hi) * mul + add
!
primaryFactoryMethod
    ^#in:lo:hi:
!
!

PrimitiveUgen subclass: #Select
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Select comment: 'Select output from an array of inputs.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Select class methodsFor: 'instance creation'!
which: which array: array
    | aSelect |
    aSelect := self new.
    aSelect paramValues: {which}.
    aSelect mceValues: {array}.
    ^aSelect
!
which: which array: array mul: mul
    ^(Select which: which array: array) * mul
!
which: which array: array mul: mul add: add
    ^(Select which: which array: array) * mul + add
!
primaryFactoryMethod
    ^#which:array:
!
!

PrimitiveUgen subclass: #SetBuf
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

SetBuf comment: 'Set local buffer'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!SetBuf class methodsFor: 'instance creation'!
buf: buf offset: offset length: length array: array
    | aSetBuf |
    aSetBuf := self new.
    aSetBuf paramValues: {buf. offset. length}.
    aSetBuf mceValues: {array}.
    ^aSetBuf
!
buf: buf offset: offset length: length array: array mul: mul
    ^(SetBuf buf: buf offset: offset length: length array: array) * mul
!
buf: buf offset: offset length: length array: array mul: mul add: add
    ^(SetBuf buf: buf offset: offset length: length array: array) * mul + add
!
primaryFactoryMethod
    ^#buf:offset:length:array:
!
!

PrimitiveUgen subclass: #SetResetFF
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

SetResetFF comment: 'Set-reset flip flop.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!SetResetFF class methodsFor: 'instance creation'!
trig: trig reset: reset
    | aSetResetFF |
    aSetResetFF := self new.
    aSetResetFF paramValues: {trig. reset}.
    ^aSetResetFF
!
trig: trig reset: reset mul: mul
    ^(SetResetFF trig: trig reset: reset) * mul
!
trig: trig reset: reset mul: mul add: add
    ^(SetResetFF trig: trig reset: reset) * mul + add
!
primaryFactoryMethod
    ^#trig:reset:
!
!

PrimitiveUgen subclass: #SinOsc
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

SinOsc comment: 'Interpolating sine wavetable oscillator.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!SinOsc class methodsFor: 'instance creation'!
freq: freq phase: phase
    | aSinOsc |
    aSinOsc := self new.
    aSinOsc paramValues: {freq. phase}.
    ^aSinOsc
!
freq: freq phase: phase mul: mul
    ^(SinOsc freq: freq phase: phase) * mul
!
freq: freq phase: phase mul: mul add: add
    ^(SinOsc freq: freq phase: phase) * mul + add
!
primaryFactoryMethod
    ^#freq:phase:
!
!

PrimitiveUgen subclass: #SinOscFB
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

SinOscFB comment: 'Feedback FM oscillator'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!SinOscFB class methodsFor: 'instance creation'!
freq: freq feedback: feedback
    | aSinOscFB |
    aSinOscFB := self new.
    aSinOscFB paramValues: {freq. feedback}.
    ^aSinOscFB
!
freq: freq feedback: feedback mul: mul
    ^(SinOscFB freq: freq feedback: feedback) * mul
!
freq: freq feedback: feedback mul: mul add: add
    ^(SinOscFB freq: freq feedback: feedback) * mul + add
!
primaryFactoryMethod
    ^#freq:feedback:
!
!

PrimitiveUgen subclass: #Slew
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Slew comment: 'Slew rate limiter.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Slew class methodsFor: 'instance creation'!
in: in up: up dn: dn
    | aSlew |
    aSlew := self new.
    aSlew paramValues: {in. up. dn}.
    ^aSlew
!
in: in up: up dn: dn mul: mul
    ^(Slew in: in up: up dn: dn) * mul
!
in: in up: up dn: dn mul: mul add: add
    ^(Slew in: in up: up dn: dn) * mul + add
!
primaryFactoryMethod
    ^#in:up:dn:
!
!

PrimitiveUgen subclass: #Slope
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Slope comment: 'Slope of signal'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Slope class methodsFor: 'instance creation'!
in: in
    | aSlope |
    aSlope := self new.
    aSlope paramValues: {in}.
    ^aSlope
!
in: in mul: mul
    ^(Slope in: in) * mul
!
in: in mul: mul add: add
    ^(Slope in: in) * mul + add
!
primaryFactoryMethod
    ^#in:
!
!

PrimitiveUgen subclass: #SOS
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

SOS comment: 'Second order filter section (biquad).'!


!SOS methodsFor: 'initialization'!
initialize
    super initialize.
    ugenName := #SOS.
    paramNames := #(#in #a0 #a1 #a2 #b1 #b2).
    mceNames := #().
    ugenRate := #(0).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    paramValues := {}.
    mceValues := {}.
!
!


!SOS class methodsFor: 'instance creation'!
in: in a0: a0 a1: a1 a2: a2 b1: b1 b2: b2
    | aSOS |
    aSOS := self new.
    aSOS paramValues: {in. a0. a1. a2. b1. b2}.
    ^aSOS
!
in: in a0: a0 a1: a1 a2: a2 b1: b1 b2: b2 mul: mul
    ^(SOS in: in a0: a0 a1: a1 a2: a2 b1: b1 b2: b2) * mul
!
in: in a0: a0 a1: a1 a2: a2 b1: b1 b2: b2 mul: mul add: add
    ^(SOS in: in a0: a0 a1: a1 a2: a2 b1: b1 b2: b2) * mul + add
!
primaryFactoryMethod
    ^#in:a0:a1:a2:b1:b2:
!
!

PrimitiveUgen subclass: #Stepper
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Stepper comment: 'Pulse counter.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Stepper class methodsFor: 'instance creation'!
trig: trig reset: reset min: min max: max step: step resetval: resetval
    | aStepper |
    aStepper := self new.
    aStepper paramValues: {trig. reset. min. max. step. resetval}.
    ^aStepper
!
trig: trig reset: reset min: min max: max step: step resetval: resetval mul: mul
    ^(Stepper trig: trig reset: reset min: min max: max step: step resetval: resetval) * mul
!
trig: trig reset: reset min: min max: max step: step resetval: resetval mul: mul add: add
    ^(Stepper trig: trig reset: reset min: min max: max step: step resetval: resetval) * mul + add
!
primaryFactoryMethod
    ^#trig:reset:min:max:step:resetval:
!
!

PrimitiveUgen subclass: #Sweep
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Sweep comment: 'Triggered linear ramp'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Sweep class methodsFor: 'instance creation'!
trig: trig rate: rate
    | aSweep |
    aSweep := self new.
    aSweep paramValues: {trig. rate}.
    ^aSweep
!
trig: trig rate: rate mul: mul
    ^(Sweep trig: trig rate: rate) * mul
!
trig: trig rate: rate mul: mul add: add
    ^(Sweep trig: trig rate: rate) * mul + add
!
primaryFactoryMethod
    ^#trig:rate:
!
!

PrimitiveUgen subclass: #SyncSaw
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

SyncSaw comment: 'Hard sync sawtooth wave.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!SyncSaw class methodsFor: 'instance creation'!
syncFreq: syncFreq sawFreq: sawFreq
    | aSyncSaw |
    aSyncSaw := self new.
    aSyncSaw paramValues: {syncFreq. sawFreq}.
    ^aSyncSaw
!
syncFreq: syncFreq sawFreq: sawFreq mul: mul
    ^(SyncSaw syncFreq: syncFreq sawFreq: sawFreq) * mul
!
syncFreq: syncFreq sawFreq: sawFreq mul: mul add: add
    ^(SyncSaw syncFreq: syncFreq sawFreq: sawFreq) * mul + add
!
primaryFactoryMethod
    ^#syncFreq:sawFreq:
!
!

PrimitiveUgen subclass: #TDuty
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

TDuty comment: 'Demand results as trigger from demand rate UGens.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!TDuty class methodsFor: 'instance creation'!
dur: dur reset: reset doneAction: doneAction level: level gapFirst: gapFirst
    | aTDuty |
    aTDuty := self new.
    aTDuty paramValues: {dur. reset. doneAction. level. gapFirst}.
    ^aTDuty
!
dur: dur reset: reset doneAction: doneAction level: level gapFirst: gapFirst mul: mul
    ^(TDuty dur: dur reset: reset doneAction: doneAction level: level gapFirst: gapFirst) * mul
!
dur: dur reset: reset doneAction: doneAction level: level gapFirst: gapFirst mul: mul add: add
    ^(TDuty dur: dur reset: reset doneAction: doneAction level: level gapFirst: gapFirst) * mul + add
!
primaryFactoryMethod
    ^#dur:reset:doneAction:level:gapFirst:
!
!

PrimitiveUgen subclass: #TExpRand
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

TExpRand comment: 'Triggered exponential random number generator.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!TExpRand class methodsFor: 'instance creation'!
lo: lo hi: hi trig: trig
    | aTExpRand |
    aTExpRand := self new.
    aTExpRand paramValues: {lo. hi. trig}.
    ^aTExpRand
!
lo: lo hi: hi trig: trig mul: mul
    ^(TExpRand lo: lo hi: hi trig: trig) * mul
!
lo: lo hi: hi trig: trig mul: mul add: add
    ^(TExpRand lo: lo hi: hi trig: trig) * mul + add
!
primaryFactoryMethod
    ^#lo:hi:trig:
!
!

PrimitiveUgen subclass: #TGrains
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

TGrains comment: 'Buffer granulator.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!TGrains class methodsFor: 'instance creation'!
numChan: numChan trigger: trigger bufnum: bufnum rate: rate centerPos: centerPos dur: dur pan: pan amp: amp interp: interp
    | aTGrains |
    aTGrains := self new.
    aTGrains numChan: numChan.
    aTGrains paramValues: {trigger. bufnum. rate. centerPos. dur. pan. amp. interp}.
    ^aTGrains
!
numChan: numChan trigger: trigger bufnum: bufnum rate: rate centerPos: centerPos dur: dur pan: pan amp: amp interp: interp mul: mul
    ^(TGrains numChan: numChan trigger: trigger bufnum: bufnum rate: rate centerPos: centerPos dur: dur pan: pan amp: amp interp: interp) * mul
!
numChan: numChan trigger: trigger bufnum: bufnum rate: rate centerPos: centerPos dur: dur pan: pan amp: amp interp: interp mul: mul add: add
    ^(TGrains numChan: numChan trigger: trigger bufnum: bufnum rate: rate centerPos: centerPos dur: dur pan: pan amp: amp interp: interp) * mul + add
!
primaryFactoryMethod
    ^#numChan:trigger:bufnum:rate:centerPos:dur:pan:amp:interp:
!
!

PrimitiveUgen subclass: #Timer
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Timer comment: 'Returns time since last triggered.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Timer class methodsFor: 'instance creation'!
trig: trig
    | aTimer |
    aTimer := self new.
    aTimer paramValues: {trig}.
    ^aTimer
!
trig: trig mul: mul
    ^(Timer trig: trig) * mul
!
trig: trig mul: mul add: add
    ^(Timer trig: trig) * mul + add
!
primaryFactoryMethod
    ^#trig:
!
!

PrimitiveUgen subclass: #TIRand
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

TIRand comment: 'Triggered integer random number generator.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!TIRand class methodsFor: 'instance creation'!
lo: lo hi: hi trig: trig
    | aTIRand |
    aTIRand := self new.
    aTIRand paramValues: {lo. hi. trig}.
    ^aTIRand
!
lo: lo hi: hi trig: trig mul: mul
    ^(TIRand lo: lo hi: hi trig: trig) * mul
!
lo: lo hi: hi trig: trig mul: mul add: add
    ^(TIRand lo: lo hi: hi trig: trig) * mul + add
!
primaryFactoryMethod
    ^#lo:hi:trig:
!
!

PrimitiveUgen subclass: #ToggleFF
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

ToggleFF comment: 'Toggle flip flop.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!ToggleFF class methodsFor: 'instance creation'!
trig: trig
    | aToggleFF |
    aToggleFF := self new.
    aToggleFF paramValues: {trig}.
    ^aToggleFF
!
trig: trig mul: mul
    ^(ToggleFF trig: trig) * mul
!
trig: trig mul: mul add: add
    ^(ToggleFF trig: trig) * mul + add
!
primaryFactoryMethod
    ^#trig:
!
!

PrimitiveUgen subclass: #TRand
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

TRand comment: 'Triggered random number generator.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!TRand class methodsFor: 'instance creation'!
lo: lo hi: hi trig: trig
    | aTRand |
    aTRand := self new.
    aTRand paramValues: {lo. hi. trig}.
    ^aTRand
!
lo: lo hi: hi trig: trig mul: mul
    ^(TRand lo: lo hi: hi trig: trig) * mul
!
lo: lo hi: hi trig: trig mul: mul add: add
    ^(TRand lo: lo hi: hi trig: trig) * mul + add
!
primaryFactoryMethod
    ^#lo:hi:trig:
!
!

PrimitiveUgen subclass: #Trig
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Trig comment: 'Timed trigger.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Trig class methodsFor: 'instance creation'!
in: in dur: dur
    | aTrig |
    aTrig := self new.
    aTrig paramValues: {in. dur}.
    ^aTrig
!
in: in dur: dur mul: mul
    ^(Trig in: in dur: dur) * mul
!
in: in dur: dur mul: mul add: add
    ^(Trig in: in dur: dur) * mul + add
!
primaryFactoryMethod
    ^#in:dur:
!
!

PrimitiveUgen subclass: #Trig1
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Trig1 comment: 'Timed trigger.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Trig1 class methodsFor: 'instance creation'!
in: in dur: dur
    | aTrig1 |
    aTrig1 := self new.
    aTrig1 paramValues: {in. dur}.
    ^aTrig1
!
in: in dur: dur mul: mul
    ^(Trig1 in: in dur: dur) * mul
!
in: in dur: dur mul: mul add: add
    ^(Trig1 in: in dur: dur) * mul + add
!
primaryFactoryMethod
    ^#in:dur:
!
!

PrimitiveUgen subclass: #TwoPole
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

TwoPole comment: 'Two pole filter.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!TwoPole class methodsFor: 'instance creation'!
in: in freq: freq radius: radius
    | aTwoPole |
    aTwoPole := self new.
    aTwoPole paramValues: {in. freq. radius}.
    ^aTwoPole
!
in: in freq: freq radius: radius mul: mul
    ^(TwoPole in: in freq: freq radius: radius) * mul
!
in: in freq: freq radius: radius mul: mul add: add
    ^(TwoPole in: in freq: freq radius: radius) * mul + add
!
primaryFactoryMethod
    ^#in:freq:radius:
!
!

PrimitiveUgen subclass: #TwoZero
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

TwoZero comment: 'Two zero filter.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!TwoZero class methodsFor: 'instance creation'!
in: in freq: freq radius: radius
    | aTwoZero |
    aTwoZero := self new.
    aTwoZero paramValues: {in. freq. radius}.
    ^aTwoZero
!
in: in freq: freq radius: radius mul: mul
    ^(TwoZero in: in freq: freq radius: radius) * mul
!
in: in freq: freq radius: radius mul: mul add: add
    ^(TwoZero in: in freq: freq radius: radius) * mul + add
!
primaryFactoryMethod
    ^#in:freq:radius:
!
!

PrimitiveUgen subclass: #VarSaw
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

VarSaw comment: 'Variable duty saw'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!VarSaw class methodsFor: 'instance creation'!
freq: freq iphase: iphase width: width
    | aVarSaw |
    aVarSaw := self new.
    aVarSaw paramValues: {freq. iphase. width}.
    ^aVarSaw
!
freq: freq iphase: iphase width: width mul: mul
    ^(VarSaw freq: freq iphase: iphase width: width) * mul
!
freq: freq iphase: iphase width: width mul: mul add: add
    ^(VarSaw freq: freq iphase: iphase width: width) * mul + add
!
primaryFactoryMethod
    ^#freq:iphase:width:
!
!

PrimitiveUgen subclass: #VBJonVerb
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

VBJonVerb comment: 'artifical reverberator'!


!VBJonVerb methodsFor: 'initialization'!
initialize
    super initialize.
    ugenName := #VBJonVerb.
    paramNames := #(#in #decay #damping #inputbw #erfl #tail).
    mceNames := #().
    ugenRate := #(0).
    numChan := 2.
    specialIndex := 0.
    ugenId := nil.
    paramValues := {}.
    mceValues := {}.
!
!


!VBJonVerb class methodsFor: 'instance creation'!
in: in decay: decay damping: damping inputbw: inputbw erfl: erfl tail: tail
    | aVBJonVerb |
    aVBJonVerb := self new.
    aVBJonVerb paramValues: {in. decay. damping. inputbw. erfl. tail}.
    ^aVBJonVerb
!
in: in decay: decay damping: damping inputbw: inputbw erfl: erfl tail: tail mul: mul
    ^(VBJonVerb in: in decay: decay damping: damping inputbw: inputbw erfl: erfl tail: tail) * mul
!
in: in decay: decay damping: damping inputbw: inputbw erfl: erfl tail: tail mul: mul add: add
    ^(VBJonVerb in: in decay: decay damping: damping inputbw: inputbw erfl: erfl tail: tail) * mul + add
!
primaryFactoryMethod
    ^#in:decay:damping:inputbw:erfl:tail:
!
!

PrimitiveUgen subclass: #Vibrato
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Vibrato comment: 'The Vibrato oscillator models a slow frequency modulation.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Vibrato class methodsFor: 'instance creation'!
freq: freq rate: rate depth: depth delay: delay onset: onset rateVariation: rateVariation depthVariation: depthVariation iphase: iphase trig: trig
    | aVibrato |
    aVibrato := self new.
    aVibrato paramValues: {freq. rate. depth. delay. onset. rateVariation. depthVariation. iphase. trig}.
    ^aVibrato
!
freq: freq rate: rate depth: depth delay: delay onset: onset rateVariation: rateVariation depthVariation: depthVariation iphase: iphase trig: trig mul: mul
    ^(Vibrato freq: freq rate: rate depth: depth delay: delay onset: onset rateVariation: rateVariation depthVariation: depthVariation iphase: iphase trig: trig) * mul
!
freq: freq rate: rate depth: depth delay: delay onset: onset rateVariation: rateVariation depthVariation: depthVariation iphase: iphase trig: trig mul: mul add: add
    ^(Vibrato freq: freq rate: rate depth: depth delay: delay onset: onset rateVariation: rateVariation depthVariation: depthVariation iphase: iphase trig: trig) * mul + add
!
primaryFactoryMethod
    ^#freq:rate:depth:delay:onset:rateVariation:depthVariation:iphase:trig:
!
!

PrimitiveUgen subclass: #WaveLoss
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

WaveLoss comment: 'Lose bits of your waves'!


!WaveLoss methodsFor: 'initialization'!
initialize
    super initialize.
    ugenName := #WaveLoss.
    paramNames := #(#in #drop #outof #mode).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    paramValues := {}.
    mceValues := {}.
!
!


!WaveLoss class methodsFor: 'instance creation'!
in: in drop: drop outof: outof mode: mode
    | aWaveLoss |
    aWaveLoss := self new.
    aWaveLoss paramValues: {in. drop. outof. mode}.
    ^aWaveLoss
!
in: in drop: drop outof: outof mode: mode mul: mul
    ^(WaveLoss in: in drop: drop outof: outof mode: mode) * mul
!
in: in drop: drop outof: outof mode: mode mul: mul add: add
    ^(WaveLoss in: in drop: drop outof: outof mode: mode) * mul + add
!
primaryFactoryMethod
    ^#in:drop:outof:mode:
!
!

PrimitiveUgen subclass: #WhiteNoise
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

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
    paramValues := {}.
    mceValues := {}.
!
!


!WhiteNoise class methodsFor: 'instance creation'!
mul: mul
    ^(WhiteNoise new) * mul
!
mul: mul add: add
    ^(WhiteNoise new) * mul + add
!
primaryFactoryMethod
    ^#new
!
!

PrimitiveUgen subclass: #Wrap
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Wrap comment: 'Wrap a signal outside given thresholds.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Wrap class methodsFor: 'instance creation'!
in: in lo: lo hi: hi
    | aWrap |
    aWrap := self new.
    aWrap paramValues: {in. lo. hi}.
    ^aWrap
!
in: in lo: lo hi: hi mul: mul
    ^(Wrap in: in lo: lo hi: hi) * mul
!
in: in lo: lo hi: hi mul: mul add: add
    ^(Wrap in: in lo: lo hi: hi) * mul + add
!
primaryFactoryMethod
    ^#in:lo:hi:
!
!

PrimitiveUgen subclass: #WrapIndex
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

WrapIndex comment: 'Index into a table with a signal.'!


!WrapIndex methodsFor: 'initialization'!
initialize
    super initialize.
    ugenName := #WrapIndex.
    paramNames := #(#bufnum #in).
    mceNames := #().
    ugenRate := #(1).
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    paramValues := {}.
    mceValues := {}.
!
!


!WrapIndex class methodsFor: 'instance creation'!
bufnum: bufnum in: in
    | aWrapIndex |
    aWrapIndex := self new.
    aWrapIndex paramValues: {bufnum. in}.
    ^aWrapIndex
!
bufnum: bufnum in: in mul: mul
    ^(WrapIndex bufnum: bufnum in: in) * mul
!
bufnum: bufnum in: in mul: mul add: add
    ^(WrapIndex bufnum: bufnum in: in) * mul + add
!
primaryFactoryMethod
    ^#bufnum:in:
!
!

PrimitiveUgen subclass: #XFade2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

XFade2 comment: 'Equal power two channel cross fade.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!XFade2 class methodsFor: 'instance creation'!
inA: inA inB: inB pan: pan level: level
    | aXFade2 |
    aXFade2 := self new.
    aXFade2 paramValues: {inA. inB. pan. level}.
    ^aXFade2
!
inA: inA inB: inB pan: pan level: level mul: mul
    ^(XFade2 inA: inA inB: inB pan: pan level: level) * mul
!
inA: inA inB: inB pan: pan level: level mul: mul add: add
    ^(XFade2 inA: inA inB: inB pan: pan level: level) * mul + add
!
primaryFactoryMethod
    ^#inA:inB:pan:level:
!
!

PrimitiveUgen subclass: #XLine
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

XLine comment: 'Exponential line generator.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!XLine class methodsFor: 'instance creation'!
start: start end: end dur: dur doneAction: doneAction
    | aXLine |
    aXLine := self new.
    aXLine paramValues: {start. end. dur. doneAction}.
    ^aXLine
!
start: start end: end dur: dur doneAction: doneAction mul: mul
    ^(XLine start: start end: end dur: dur doneAction: doneAction) * mul
!
start: start end: end dur: dur doneAction: doneAction mul: mul add: add
    ^(XLine start: start end: end dur: dur doneAction: doneAction) * mul + add
!
primaryFactoryMethod
    ^#start:end:dur:doneAction:
!
!

PrimitiveUgen subclass: #ZeroCrossing
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

ZeroCrossing comment: 'Zero crossing frequency follower'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!ZeroCrossing class methodsFor: 'instance creation'!
in: in
    | aZeroCrossing |
    aZeroCrossing := self new.
    aZeroCrossing paramValues: {in}.
    ^aZeroCrossing
!
in: in mul: mul
    ^(ZeroCrossing in: in) * mul
!
in: in mul: mul add: add
    ^(ZeroCrossing in: in) * mul + add
!
primaryFactoryMethod
    ^#in:
!
!

PrimitiveUgen subclass: #MoogLadder
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

MoogLadder comment: 'Moog Filter Emulation'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!MoogLadder class methodsFor: 'instance creation'!
in: in ffreq: ffreq res: res
    | aMoogLadder |
    aMoogLadder := self new.
    aMoogLadder paramValues: {in. ffreq. res}.
    ^aMoogLadder
!
in: in ffreq: ffreq res: res mul: mul
    ^(MoogLadder in: in ffreq: ffreq res: res) * mul
!
in: in ffreq: ffreq res: res mul: mul add: add
    ^(MoogLadder in: in ffreq: ffreq res: res) * mul + add
!
primaryFactoryMethod
    ^#in:ffreq:res:
!
!

PrimitiveUgen subclass: #GreyholeRaw
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

GreyholeRaw comment: 'algorithmic delay'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!GreyholeRaw class methodsFor: 'instance creation'!
in1: in1 in2: in2 damping: damping delaytime: delaytime diffusion: diffusion feedback: feedback moddepth: moddepth modfreq: modfreq size: size
    | aGreyholeRaw |
    aGreyholeRaw := self new.
    aGreyholeRaw paramValues: {in1. in2. damping. delaytime. diffusion. feedback. moddepth. modfreq. size}.
    ^aGreyholeRaw
!
in1: in1 in2: in2 damping: damping delaytime: delaytime diffusion: diffusion feedback: feedback moddepth: moddepth modfreq: modfreq size: size mul: mul
    ^(GreyholeRaw in1: in1 in2: in2 damping: damping delaytime: delaytime diffusion: diffusion feedback: feedback moddepth: moddepth modfreq: modfreq size: size) * mul
!
in1: in1 in2: in2 damping: damping delaytime: delaytime diffusion: diffusion feedback: feedback moddepth: moddepth modfreq: modfreq size: size mul: mul add: add
    ^(GreyholeRaw in1: in1 in2: in2 damping: damping delaytime: delaytime diffusion: diffusion feedback: feedback moddepth: moddepth modfreq: modfreq size: size) * mul + add
!
primaryFactoryMethod
    ^#in1:in2:damping:delaytime:diffusion:feedback:moddepth:modfreq:size:
!
!

PrimitiveUgen subclass: #CrossoverDistortion
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

CrossoverDistortion comment: 'class B/AB power amp distortion simulation'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!CrossoverDistortion class methodsFor: 'instance creation'!
in: in amp: amp smooth: smooth
    | aCrossoverDistortion |
    aCrossoverDistortion := self new.
    aCrossoverDistortion paramValues: {in. amp. smooth}.
    ^aCrossoverDistortion
!
in: in amp: amp smooth: smooth mul: mul
    ^(CrossoverDistortion in: in amp: amp smooth: smooth) * mul
!
in: in amp: amp smooth: smooth mul: mul add: add
    ^(CrossoverDistortion in: in amp: amp smooth: smooth) * mul + add
!
primaryFactoryMethod
    ^#in:amp:smooth:
!
!

PrimitiveUgen subclass: #Friction
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Friction comment: 'A physical model of a system with dry-friction. A chaotic filter.'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Friction class methodsFor: 'instance creation'!
in: in friction: friction spring: spring damp: damp mass: mass beltmass: beltmass
    | aFriction |
    aFriction := self new.
    aFriction paramValues: {in. friction. spring. damp. mass. beltmass}.
    ^aFriction
!
in: in friction: friction spring: spring damp: damp mass: mass beltmass: beltmass mul: mul
    ^(Friction in: in friction: friction spring: spring damp: damp mass: mass beltmass: beltmass) * mul
!
in: in friction: friction spring: spring damp: damp mass: mass beltmass: beltmass mul: mul add: add
    ^(Friction in: in friction: friction spring: spring damp: damp mass: mass beltmass: beltmass) * mul + add
!
primaryFactoryMethod
    ^#in:friction:spring:damp:mass:beltmass:
!
!

PrimitiveUgen subclass: #MembraneCircle
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

MembraneCircle comment: 'Waveguide mesh physical models of drum membranes'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!MembraneCircle class methodsFor: 'instance creation'!
excitation: excitation tension: tension loss: loss
    | aMembraneCircle |
    aMembraneCircle := self new.
    aMembraneCircle paramValues: {excitation. tension. loss}.
    ^aMembraneCircle
!
excitation: excitation tension: tension loss: loss mul: mul
    ^(MembraneCircle excitation: excitation tension: tension loss: loss) * mul
!
excitation: excitation tension: tension loss: loss mul: mul add: add
    ^(MembraneCircle excitation: excitation tension: tension loss: loss) * mul + add
!
primaryFactoryMethod
    ^#excitation:tension:loss:
!
!

PrimitiveUgen subclass: #VOSIM
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

VOSIM comment: 'vosim pulse generator'!


!VOSIM methodsFor: 'initialization'!
initialize
    super initialize.
    ugenName := #VOSIM.
    paramNames := #(#trig #freq #nCycles #decay).
    mceNames := #().
    ugenRate := #ar.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    paramValues := {}.
    mceValues := {}.
!
!


!VOSIM class methodsFor: 'instance creation'!
trig: trig freq: freq nCycles: nCycles decay: decay
    | aVOSIM |
    aVOSIM := self new.
    aVOSIM paramValues: {trig. freq. nCycles. decay}.
    ^aVOSIM
!
trig: trig freq: freq nCycles: nCycles decay: decay mul: mul
    ^(VOSIM trig: trig freq: freq nCycles: nCycles decay: decay) * mul
!
trig: trig freq: freq nCycles: nCycles decay: decay mul: mul add: add
    ^(VOSIM trig: trig freq: freq nCycles: nCycles decay: decay) * mul + add
!
primaryFactoryMethod
    ^#trig:freq:nCycles:decay:
!
!

PrimitiveUgen subclass: #MiRings
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

MiRings comment: 'a resonator'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!MiRings class methodsFor: 'instance creation'!
in: in trig: trig pit: pit struct: struct bright: bright damp: damp pos: pos model: model poly: poly intern_exciter: intern_exciter easteregg: easteregg bypass: bypass
    | aMiRings |
    aMiRings := self new.
    aMiRings paramValues: {in. trig. pit. struct. bright. damp. pos. model. poly. intern_exciter. easteregg. bypass}.
    ^aMiRings
!
in: in trig: trig pit: pit struct: struct bright: bright damp: damp pos: pos model: model poly: poly intern_exciter: intern_exciter easteregg: easteregg bypass: bypass mul: mul
    ^(MiRings in: in trig: trig pit: pit struct: struct bright: bright damp: damp pos: pos model: model poly: poly intern_exciter: intern_exciter easteregg: easteregg bypass: bypass) * mul
!
in: in trig: trig pit: pit struct: struct bright: bright damp: damp pos: pos model: model poly: poly intern_exciter: intern_exciter easteregg: easteregg bypass: bypass mul: mul add: add
    ^(MiRings in: in trig: trig pit: pit struct: struct bright: bright damp: damp pos: pos model: model poly: poly intern_exciter: intern_exciter easteregg: easteregg bypass: bypass) * mul + add
!
primaryFactoryMethod
    ^#in:trig:pit:struct:bright:damp:pos:model:poly:intern_exciter:easteregg:bypass:
!
!

PrimitiveUgen subclass: #AnalogFoldOsc
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

AnalogFoldOsc comment: '(Undocumented class)'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!AnalogFoldOsc class methodsFor: 'instance creation'!
freq: freq amp: amp
    | anAnalogFoldOsc |
    anAnalogFoldOsc := self new.
    anAnalogFoldOsc paramValues: {freq. amp}.
    ^anAnalogFoldOsc
!
freq: freq amp: amp mul: mul
    ^(AnalogFoldOsc freq: freq amp: amp) * mul
!
freq: freq amp: amp mul: mul add: add
    ^(AnalogFoldOsc freq: freq amp: amp) * mul + add
!
primaryFactoryMethod
    ^#freq:amp:
!
!

PrimitiveUgen subclass: #RCD
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

RCD comment: 'rotating clock divider'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!RCD class methodsFor: 'instance creation'!
clock: clock rotate: rotate reset: reset div: div spread: spread auto: auto len: len down: down gates: gates
    | aRCD |
    aRCD := self new.
    aRCD paramValues: {clock. rotate. reset. div. spread. auto. len. down. gates}.
    ^aRCD
!
clock: clock rotate: rotate reset: reset div: div spread: spread auto: auto len: len down: down gates: gates mul: mul
    ^(RCD clock: clock rotate: rotate reset: reset div: div spread: spread auto: auto len: len down: down gates: gates) * mul
!
clock: clock rotate: rotate reset: reset div: div spread: spread auto: auto len: len down: down gates: gates mul: mul add: add
    ^(RCD clock: clock rotate: rotate reset: reset div: div spread: spread auto: auto len: len down: down gates: gates) * mul + add
!
primaryFactoryMethod
    ^#clock:rotate:reset:div:spread:auto:len:down:gates:
!
!

PrimitiveUgen subclass: #SCM
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

SCM comment: 'shuffling clock multiplier'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!SCM class methodsFor: 'instance creation'!
clock: clock bpm: bpm rotate: rotate slip: slip shuffle: shuffle skip: skip pw: pw
    | aSCM |
    aSCM := self new.
    aSCM paramValues: {clock. bpm. rotate. slip. shuffle. skip. pw}.
    ^aSCM
!
clock: clock bpm: bpm rotate: rotate slip: slip shuffle: shuffle skip: skip pw: pw mul: mul
    ^(SCM clock: clock bpm: bpm rotate: rotate slip: slip shuffle: shuffle skip: skip pw: pw) * mul
!
clock: clock bpm: bpm rotate: rotate slip: slip shuffle: shuffle skip: skip pw: pw mul: mul add: add
    ^(SCM clock: clock bpm: bpm rotate: rotate slip: slip shuffle: shuffle skip: skip pw: pw) * mul + add
!
primaryFactoryMethod
    ^#clock:bpm:rotate:slip:shuffle:skip:pw:
!
!

PrimitiveUgen subclass: #DustRange
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

DustRange comment: '(Undocumented class)'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!DustRange class methodsFor: 'instance creation'!
iotMin: iotMin iotMax: iotMax
    | aDustRange |
    aDustRange := self new.
    aDustRange paramValues: {iotMin. iotMax}.
    ^aDustRange
!
iotMin: iotMin iotMax: iotMax mul: mul
    ^(DustRange iotMin: iotMin iotMax: iotMax) * mul
!
iotMin: iotMin iotMax: iotMax mul: mul add: add
    ^(DustRange iotMin: iotMin iotMax: iotMax) * mul + add
!
primaryFactoryMethod
    ^#iotMin:iotMax:
!
!

PrimitiveUgen subclass: #ExpRandN
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

ExpRandN comment: '(Undocumented class)'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!ExpRandN class methodsFor: 'instance creation'!
numChan: numChan lo: lo hi: hi
    | anExpRandN |
    anExpRandN := self new.
    anExpRandN numChan: numChan.
    anExpRandN paramValues: {lo. hi}.
    ^anExpRandN
!
numChan: numChan lo: lo hi: hi mul: mul
    ^(ExpRandN numChan: numChan lo: lo hi: hi) * mul
!
numChan: numChan lo: lo hi: hi mul: mul add: add
    ^(ExpRandN numChan: numChan lo: lo hi: hi) * mul + add
!
primaryFactoryMethod
    ^#numChan:lo:hi:
!
!

PrimitiveUgen subclass: #LinRandN
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LinRandN comment: '(Undocumented class)'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!LinRandN class methodsFor: 'instance creation'!
numChan: numChan lo: lo hi: hi minmax: minmax
    | aLinRandN |
    aLinRandN := self new.
    aLinRandN numChan: numChan.
    aLinRandN paramValues: {lo. hi. minmax}.
    ^aLinRandN
!
numChan: numChan lo: lo hi: hi minmax: minmax mul: mul
    ^(LinRandN numChan: numChan lo: lo hi: hi minmax: minmax) * mul
!
numChan: numChan lo: lo hi: hi minmax: minmax mul: mul add: add
    ^(LinRandN numChan: numChan lo: lo hi: hi minmax: minmax) * mul + add
!
primaryFactoryMethod
    ^#numChan:lo:hi:minmax:
!
!

PrimitiveUgen subclass: #RandN
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

RandN comment: '(Undocumented class)'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!RandN class methodsFor: 'instance creation'!
numChan: numChan lo: lo hi: hi
    | aRandN |
    aRandN := self new.
    aRandN numChan: numChan.
    aRandN paramValues: {lo. hi}.
    ^aRandN
!
numChan: numChan lo: lo hi: hi mul: mul
    ^(RandN numChan: numChan lo: lo hi: hi) * mul
!
numChan: numChan lo: lo hi: hi mul: mul add: add
    ^(RandN numChan: numChan lo: lo hi: hi) * mul + add
!
primaryFactoryMethod
    ^#numChan:lo:hi:
!
!

PrimitiveUgen subclass: #TScramble
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

TScramble comment: '(Undocumented class)'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!TScramble class methodsFor: 'instance creation'!
trigger: trigger inputs: inputs
    | aTScramble |
    aTScramble := self new.
    aTScramble paramValues: {trigger}.
    aTScramble mceValues: {inputs}.
    ^aTScramble
!
trigger: trigger inputs: inputs mul: mul
    ^(TScramble trigger: trigger inputs: inputs) * mul
!
trigger: trigger inputs: inputs mul: mul add: add
    ^(TScramble trigger: trigger inputs: inputs) * mul + add
!
primaryFactoryMethod
    ^#trigger:inputs:
!
!

PrimitiveUgen subclass: #DX7
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

DX7 comment: '(Undocumented class)'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!DX7 class methodsFor: 'instance creation'!
bufnum: bufnum on: on off: off data: data vc: vc mnn: mnn vel: vel pw: pw mw: mw bc: bc fc: fc
    | aDX7 |
    aDX7 := self new.
    aDX7 paramValues: {bufnum. on. off. data. vc. mnn. vel. pw. mw. bc. fc}.
    ^aDX7
!
bufnum: bufnum on: on off: off data: data vc: vc mnn: mnn vel: vel pw: pw mw: mw bc: bc fc: fc mul: mul
    ^(DX7 bufnum: bufnum on: on off: off data: data vc: vc mnn: mnn vel: vel pw: pw mw: mw bc: bc fc: fc) * mul
!
bufnum: bufnum on: on off: off data: data vc: vc mnn: mnn vel: vel pw: pw mw: mw bc: bc fc: fc mul: mul add: add
    ^(DX7 bufnum: bufnum on: on off: off data: data vc: vc mnn: mnn vel: vel pw: pw mw: mw bc: bc fc: fc) * mul + add
!
primaryFactoryMethod
    ^#bufnum:on:off:data:vc:mnn:vel:pw:mw:bc:fc:
!
!

PrimitiveUgen subclass: #RDX7Env
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

RDX7Env comment: '(Undocumented class)'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!RDX7Env class methodsFor: 'instance creation'!
gate: gate data: data r1: r1 r2: r2 r3: r3 r4: r4 l1: l1 l2: l2 l3: l3 l4: l4 ol: ol
    | aRDX7Env |
    aRDX7Env := self new.
    aRDX7Env paramValues: {gate. data. r1. r2. r3. r4. l1. l2. l3. l4. ol}.
    ^aRDX7Env
!
gate: gate data: data r1: r1 r2: r2 r3: r3 r4: r4 l1: l1 l2: l2 l3: l3 l4: l4 ol: ol mul: mul
    ^(RDX7Env gate: gate data: data r1: r1 r2: r2 r3: r3 r4: r4 l1: l1 l2: l2 l3: l3 l4: l4 ol: ol) * mul
!
gate: gate data: data r1: r1 r2: r2 r3: r3 r4: r4 l1: l1 l2: l2 l3: l3 l4: l4 ol: ol mul: mul add: add
    ^(RDX7Env gate: gate data: data r1: r1 r2: r2 r3: r3 r4: r4 l1: l1 l2: l2 l3: l3 l4: l4 ol: ol) * mul + add
!
primaryFactoryMethod
    ^#gate:data:r1:r2:r3:r4:l1:l2:l3:l4:ol:
!
!

PrimitiveUgen subclass: #ObxdFilter
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

ObxdFilter comment: '(Undocumented class)'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!ObxdFilter class methodsFor: 'instance creation'!
in: in cutoff: cutoff resonance: resonance multimode: multimode bandpass: bandpass fourpole: fourpole
    | anObxdFilter |
    anObxdFilter := self new.
    anObxdFilter paramValues: {in. cutoff. resonance. multimode. bandpass. fourpole}.
    ^anObxdFilter
!
in: in cutoff: cutoff resonance: resonance multimode: multimode bandpass: bandpass fourpole: fourpole mul: mul
    ^(ObxdFilter in: in cutoff: cutoff resonance: resonance multimode: multimode bandpass: bandpass fourpole: fourpole) * mul
!
in: in cutoff: cutoff resonance: resonance multimode: multimode bandpass: bandpass fourpole: fourpole mul: mul add: add
    ^(ObxdFilter in: in cutoff: cutoff resonance: resonance multimode: multimode bandpass: bandpass fourpole: fourpole) * mul + add
!
primaryFactoryMethod
    ^#in:cutoff:resonance:multimode:bandpass:fourpole:
!
!

PrimitiveUgen subclass: #SvfBp
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

SvfBp comment: '(Undocumented class)'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!SvfBp class methodsFor: 'instance creation'!
in: in freq: freq q: q
    | aSvfBp |
    aSvfBp := self new.
    aSvfBp paramValues: {in. freq. q}.
    ^aSvfBp
!
in: in freq: freq q: q mul: mul
    ^(SvfBp in: in freq: freq q: q) * mul
!
in: in freq: freq q: q mul: mul add: add
    ^(SvfBp in: in freq: freq q: q) * mul + add
!
primaryFactoryMethod
    ^#in:freq:q:
!
!

PrimitiveUgen subclass: #SvfHp
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

SvfHp comment: '(Undocumented class)'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!SvfHp class methodsFor: 'instance creation'!
in: in freq: freq q: q
    | aSvfHp |
    aSvfHp := self new.
    aSvfHp paramValues: {in. freq. q}.
    ^aSvfHp
!
in: in freq: freq q: q mul: mul
    ^(SvfHp in: in freq: freq q: q) * mul
!
in: in freq: freq q: q mul: mul add: add
    ^(SvfHp in: in freq: freq q: q) * mul + add
!
primaryFactoryMethod
    ^#in:freq:q:
!
!

PrimitiveUgen subclass: #SvfLp
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

SvfLp comment: '(Undocumented class)'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!SvfLp class methodsFor: 'instance creation'!
in: in freq: freq q: q
    | aSvfLp |
    aSvfLp := self new.
    aSvfLp paramValues: {in. freq. q}.
    ^aSvfLp
!
in: in freq: freq q: q mul: mul
    ^(SvfLp in: in freq: freq q: q) * mul
!
in: in freq: freq q: q mul: mul add: add
    ^(SvfLp in: in freq: freq q: q) * mul + add
!
primaryFactoryMethod
    ^#in:freq:q:
!
!

PrimitiveUgen subclass: #Bezier
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Bezier comment: '(Undocumented class)'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Bezier class methodsFor: 'instance creation'!
haltAfter: haltAfter dx: dx freq: freq phase: phase param: param
    | aBezier |
    aBezier := self new.
    aBezier paramValues: {haltAfter. dx. freq. phase}.
    aBezier mceValues: {param}.
    ^aBezier
!
haltAfter: haltAfter dx: dx freq: freq phase: phase param: param mul: mul
    ^(Bezier haltAfter: haltAfter dx: dx freq: freq phase: phase param: param) * mul
!
haltAfter: haltAfter dx: dx freq: freq phase: phase param: param mul: mul add: add
    ^(Bezier haltAfter: haltAfter dx: dx freq: freq phase: phase param: param) * mul + add
!
primaryFactoryMethod
    ^#haltAfter:dx:freq:phase:param:
!
!

PrimitiveUgen subclass: #Freezer
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Freezer comment: '(Undocumented class)'!


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
    paramValues := {}.
    mceValues := {}.
!
!


!Freezer class methodsFor: 'instance creation'!
bufnum: bufnum left: left right: right gain: gain increment: increment incrementOffset: incrementOffset incrementRandom: incrementRandom rightRandom: rightRandom syncPhaseTrigger: syncPhaseTrigger randomizePhaseTrigger: randomizePhaseTrigger numberOfLoops: numberOfLoops
    | aFreezer |
    aFreezer := self new.
    aFreezer paramValues: {bufnum. left. right. gain. increment. incrementOffset. incrementRandom. rightRandom. syncPhaseTrigger. randomizePhaseTrigger. numberOfLoops}.
    ^aFreezer
!
bufnum: bufnum left: left right: right gain: gain increment: increment incrementOffset: incrementOffset incrementRandom: incrementRandom rightRandom: rightRandom syncPhaseTrigger: syncPhaseTrigger randomizePhaseTrigger: randomizePhaseTrigger numberOfLoops: numberOfLoops mul: mul
    ^(Freezer bufnum: bufnum left: left right: right gain: gain increment: increment incrementOffset: incrementOffset incrementRandom: incrementRandom rightRandom: rightRandom syncPhaseTrigger: syncPhaseTrigger randomizePhaseTrigger: randomizePhaseTrigger numberOfLoops: numberOfLoops) * mul
!
bufnum: bufnum left: left right: right gain: gain increment: increment incrementOffset: incrementOffset incrementRandom: incrementRandom rightRandom: rightRandom syncPhaseTrigger: syncPhaseTrigger randomizePhaseTrigger: randomizePhaseTrigger numberOfLoops: numberOfLoops mul: mul add: add
    ^(Freezer bufnum: bufnum left: left right: right gain: gain increment: increment incrementOffset: incrementOffset incrementRandom: incrementRandom rightRandom: rightRandom syncPhaseTrigger: syncPhaseTrigger randomizePhaseTrigger: randomizePhaseTrigger numberOfLoops: numberOfLoops) * mul + add
!
primaryFactoryMethod
    ^#bufnum:left:right:gain:increment:incrementOffset:incrementRandom:rightRandom:syncPhaseTrigger:randomizePhaseTrigger:numberOfLoops:
!
!


!AbstractUgen methodsFor: 'arithmetic'!
  + b ^ BinaryOpUgen specialIndex: 0 a: self b: b!
  - b ^ BinaryOpUgen specialIndex: 1 a: self b: b!
  * b ^ BinaryOpUgen specialIndex: 2 a: self b: b!
  / b ^ BinaryOpUgen specialIndex: 4 a: self b: b!
  % b ^ BinaryOpUgen specialIndex: 5 a: self b: b!
  /= b ^ BinaryOpUgen specialIndex: 7 a: self b: b!
  < b ^ BinaryOpUgen specialIndex: 8 a: self b: b!
  > b ^ BinaryOpUgen specialIndex: 9 a: self b: b!
  <= b ^ BinaryOpUgen specialIndex: 10 a: self b: b!
  >= b ^ BinaryOpUgen specialIndex: 11 a: self b: b!
  min: b ^ BinaryOpUgen specialIndex: 12 a: self b: b!
  max: b ^ BinaryOpUgen specialIndex: 13 a: self b: b!
  bitAnd: b ^ BinaryOpUgen specialIndex: 14 a: self b: b!
  bitOr: b ^ BinaryOpUgen specialIndex: 15 a: self b: b!
  lcm: b ^ BinaryOpUgen specialIndex: 17 a: self b: b!
  gcd: b ^ BinaryOpUgen specialIndex: 18 a: self b: b!
  roundTo: b ^ BinaryOpUgen specialIndex: 19 a: self b: b!
  truncateTo: b ^ BinaryOpUgen specialIndex: 21 a: self b: b!
  raisedTo: b ^ BinaryOpUgen specialIndex: 25 a: self b: b!
  bitShiftLeft: b ^ BinaryOpUgen specialIndex: 26 a: self b: b!
  bitShiftRight: b ^ BinaryOpUgen specialIndex: 27 a: self b: b!
  amClip: b ^ BinaryOpUgen specialIndex: 40 a: self b: b!
  clip2: b ^ BinaryOpUgen specialIndex: 42 a: self b: b!
  fold2: b ^ BinaryOpUgen specialIndex: 44 a: self b: b!
!

!AbstractUgen methodsFor: 'arithmetic'!
  negated ^ UnaryOpUgen specialIndex: 0 a: self!
  abs ^ UnaryOpUgen specialIndex: 5 a: self!
  ceil ^ UnaryOpUgen specialIndex: 8 a: self!
  floor ^ UnaryOpUgen specialIndex: 9 a: self!
  frac ^ UnaryOpUgen specialIndex: 10 a: self!
  sign ^ UnaryOpUgen specialIndex: 11 a: self!
  squared ^ UnaryOpUgen specialIndex: 12 a: self!
  cubed ^ UnaryOpUgen specialIndex: 13 a: self!
  sqrt ^ UnaryOpUgen specialIndex: 14 a: self!
  exp ^ UnaryOpUgen specialIndex: 15 a: self!
  reciprocal ^ UnaryOpUgen specialIndex: 16 a: self!
  midiCps ^ UnaryOpUgen specialIndex: 17 a: self!
  cpsMidi ^ UnaryOpUgen specialIndex: 18 a: self!
  midiRatio ^ UnaryOpUgen specialIndex: 19 a: self!
  ratioMidi ^ UnaryOpUgen specialIndex: 20 a: self!
  dbAmp ^ UnaryOpUgen specialIndex: 21 a: self!
  ampDb ^ UnaryOpUgen specialIndex: 22 a: self!
  ln ^ UnaryOpUgen specialIndex: 25 a: self!
  log2 ^ UnaryOpUgen specialIndex: 26 a: self!
  log ^ UnaryOpUgen specialIndex: 27 a: self!
  sin ^ UnaryOpUgen specialIndex: 28 a: self!
  cos ^ UnaryOpUgen specialIndex: 29 a: self!
  tanh ^ UnaryOpUgen specialIndex: 36 a: self!
  distort ^ UnaryOpUgen specialIndex: 42 a: self!
  softClip ^ UnaryOpUgen specialIndex: 43 a: self!
!

