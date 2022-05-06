PrimitiveUgen subclass: #AllpassC
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

AllpassC comment: 'Schroeder allpass delay line with cubic interpolation.'!

!AllpassC class methodsFor: 'instance creation'!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
    ^UgenBuilder name: 'AllpassC' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. maxdelaytime. delaytime. decaytime}!
primaryFactoryMethod
    ^#in:maxdelaytime:delaytime:decaytime:!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul
    ^(AllpassC in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add
    ^(AllpassC in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul + add!
!

PrimitiveUgen subclass: #AllpassL
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

AllpassL comment: 'Schroeder allpass delay line with linear interpolation.'!

!AllpassL class methodsFor: 'instance creation'!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
    ^UgenBuilder name: 'AllpassL' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. maxdelaytime. delaytime. decaytime}!
primaryFactoryMethod
    ^#in:maxdelaytime:delaytime:decaytime:!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul
    ^(AllpassL in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add
    ^(AllpassL in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul + add!
!

PrimitiveUgen subclass: #AllpassN
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

AllpassN comment: 'Schroeder allpass delay line with no interpolation.'!

!AllpassN class methodsFor: 'instance creation'!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
    ^UgenBuilder name: 'AllpassN' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. maxdelaytime. delaytime. decaytime}!
primaryFactoryMethod
    ^#in:maxdelaytime:delaytime:decaytime:!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul
    ^(AllpassN in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add
    ^(AllpassN in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul + add!
!

PrimitiveUgen subclass: #AmpComp
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

AmpComp comment: 'Basic psychoacoustic amplitude compensation.'!

!AmpComp class methodsFor: 'instance creation'!
freq: freq root: root exp: exp
    ^UgenBuilder name: 'AmpComp' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq. root. exp}!
primaryFactoryMethod
    ^#freq:root:exp:!
freq: freq root: root exp: exp mul: mul
    ^(AmpComp freq: freq root: root exp: exp) * mul!
freq: freq root: root exp: exp mul: mul add: add
    ^(AmpComp freq: freq root: root exp: exp) * mul + add!
!

PrimitiveUgen subclass: #AmpCompA
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

AmpCompA comment: 'Basic psychoacoustic amplitude compensation (ANSI A-weighting curve).'!

!AmpCompA class methodsFor: 'instance creation'!
freq: freq root: root minAmp: minAmp rootAmp: rootAmp
    ^UgenBuilder name: 'AmpCompA' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq. root. minAmp. rootAmp}!
primaryFactoryMethod
    ^#freq:root:minAmp:rootAmp:!
freq: freq root: root minAmp: minAmp rootAmp: rootAmp mul: mul
    ^(AmpCompA freq: freq root: root minAmp: minAmp rootAmp: rootAmp) * mul!
freq: freq root: root minAmp: minAmp rootAmp: rootAmp mul: mul add: add
    ^(AmpCompA freq: freq root: root minAmp: minAmp rootAmp: rootAmp) * mul + add!
!

PrimitiveUgen subclass: #Amplitude
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Amplitude comment: 'Amplitude follower'!

!Amplitude class methodsFor: 'instance creation'!
in: in attackTime: attackTime releaseTime: releaseTime
    ^UgenBuilder name: 'Amplitude' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {in. attackTime. releaseTime}!
primaryFactoryMethod
    ^#in:attackTime:releaseTime:!
in: in attackTime: attackTime releaseTime: releaseTime mul: mul
    ^(Amplitude in: in attackTime: attackTime releaseTime: releaseTime) * mul!
in: in attackTime: attackTime releaseTime: releaseTime mul: mul add: add
    ^(Amplitude in: in attackTime: attackTime releaseTime: releaseTime) * mul + add!
!

PrimitiveUgen subclass: #Balance2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Balance2 comment: 'Stereo signal balancer'!

!Balance2 class methodsFor: 'instance creation'!
left: left right: right pos: pos level: level
    ^UgenBuilder name: 'Balance2' numChan: 2 rateSpec: #(1 2) specialIndex: 0 inputArray: {left. right. pos. level}!
primaryFactoryMethod
    ^#left:right:pos:level:!
left: left right: right pos: pos level: level mul: mul
    ^(Balance2 left: left right: right pos: pos level: level) * mul!
left: left right: right pos: pos level: level mul: mul add: add
    ^(Balance2 left: left right: right pos: pos level: level) * mul + add!
!

PrimitiveUgen subclass: #BBandPass
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BBandPass comment: 'Band Pass Filter'!

!BBandPass class methodsFor: 'instance creation'!
in: in freq: freq bw: bw
    ^UgenBuilder name: 'BBandPass' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. freq. bw}!
primaryFactoryMethod
    ^#in:freq:bw:!
in: in freq: freq bw: bw mul: mul
    ^(BBandPass in: in freq: freq bw: bw) * mul!
in: in freq: freq bw: bw mul: mul add: add
    ^(BBandPass in: in freq: freq bw: bw) * mul + add!
!

PrimitiveUgen subclass: #BBandStop
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BBandStop comment: 'Band reject filter'!

!BBandStop class methodsFor: 'instance creation'!
in: in freq: freq bw: bw
    ^UgenBuilder name: 'BBandStop' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. freq. bw}!
primaryFactoryMethod
    ^#in:freq:bw:!
in: in freq: freq bw: bw mul: mul
    ^(BBandStop in: in freq: freq bw: bw) * mul!
in: in freq: freq bw: bw mul: mul add: add
    ^(BBandStop in: in freq: freq bw: bw) * mul + add!
!

PrimitiveUgen subclass: #Blip
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Blip comment: 'Band limited impulse oscillator.'!

!Blip class methodsFor: 'instance creation'!
freq: freq numharm: numharm
    ^UgenBuilder name: 'Blip' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq. numharm}!
primaryFactoryMethod
    ^#freq:numharm:!
freq: freq numharm: numharm mul: mul
    ^(Blip freq: freq numharm: numharm) * mul!
freq: freq numharm: numharm mul: mul add: add
    ^(Blip freq: freq numharm: numharm) * mul + add!
!

PrimitiveUgen subclass: #BlockSize
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BlockSize comment: '(Undocumented class)'!

!BlockSize class methodsFor: 'instance creation'!
new
    ^UgenBuilder name: 'BlockSize' numChan: 1 rateSpec: (Rate ir) specialIndex: 0 inputArray: {}!
primaryFactoryMethod
    ^#new!
mul: mul
    ^(BlockSize new) * mul!
mul: mul add: add
    ^(BlockSize new) * mul + add!
!

PrimitiveUgen subclass: #BLowPass
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BLowPass comment: '12db/oct rolloff - 2nd order resonant Low Pass Filter'!

!BLowPass class methodsFor: 'instance creation'!
in: in freq: freq rq: rq
    ^UgenBuilder name: 'BLowPass' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. freq. rq}!
primaryFactoryMethod
    ^#in:freq:rq:!
in: in freq: freq rq: rq mul: mul
    ^(BLowPass in: in freq: freq rq: rq) * mul!
in: in freq: freq rq: rq mul: mul add: add
    ^(BLowPass in: in freq: freq rq: rq) * mul + add!
!

PrimitiveUgen subclass: #BPeakEQ
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BPeakEQ comment: 'Parametric equalizer'!

!BPeakEQ class methodsFor: 'instance creation'!
in: in freq: freq rq: rq db: db
    ^UgenBuilder name: 'BPeakEQ' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. freq. rq. db}!
primaryFactoryMethod
    ^#in:freq:rq:db:!
in: in freq: freq rq: rq db: db mul: mul
    ^(BPeakEQ in: in freq: freq rq: rq db: db) * mul!
in: in freq: freq rq: rq db: db mul: mul add: add
    ^(BPeakEQ in: in freq: freq rq: rq db: db) * mul + add!
!

PrimitiveUgen subclass: #BPF
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BPF comment: '2nd order Butterworth bandpass filter.'!

!BPF class methodsFor: 'instance creation'!
in: in freq: freq rq: rq
    ^UgenBuilder name: 'BPF' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. freq. rq}!
primaryFactoryMethod
    ^#in:freq:rq:!
in: in freq: freq rq: rq mul: mul
    ^(BPF in: in freq: freq rq: rq) * mul!
in: in freq: freq rq: rq mul: mul add: add
    ^(BPF in: in freq: freq rq: rq) * mul + add!
!

PrimitiveUgen subclass: #BPZ2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BPZ2 comment: 'Two zero fixed midpass.'!

!BPZ2 class methodsFor: 'instance creation'!
in: in
    ^UgenBuilder name: 'BPZ2' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in}!
primaryFactoryMethod
    ^#in:!
in: in mul: mul
    ^(BPZ2 in: in) * mul!
in: in mul: mul add: add
    ^(BPZ2 in: in) * mul + add!
!

PrimitiveUgen subclass: #BRF
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BRF comment: '2nd order Butterworth band reject filter.'!

!BRF class methodsFor: 'instance creation'!
in: in freq: freq rq: rq
    ^UgenBuilder name: 'BRF' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. freq. rq}!
primaryFactoryMethod
    ^#in:freq:rq:!
in: in freq: freq rq: rq mul: mul
    ^(BRF in: in freq: freq rq: rq) * mul!
in: in freq: freq rq: rq mul: mul add: add
    ^(BRF in: in freq: freq rq: rq) * mul + add!
!

PrimitiveUgen subclass: #BRZ2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BRZ2 comment: 'Two zero fixed midcut.'!

!BRZ2 class methodsFor: 'instance creation'!
in: in
    ^UgenBuilder name: 'BRZ2' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in}!
primaryFactoryMethod
    ^#in:!
in: in mul: mul
    ^(BRZ2 in: in) * mul!
in: in mul: mul add: add
    ^(BRZ2 in: in) * mul + add!
!

PrimitiveUgen subclass: #BrownNoise
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BrownNoise comment: 'Brown Noise.'!

!BrownNoise class methodsFor: 'instance creation'!
new
    ^UgenBuilder name: 'BrownNoise' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {}!
primaryFactoryMethod
    ^#new!
mul: mul
    ^(BrownNoise new) * mul!
mul: mul add: add
    ^(BrownNoise new) * mul + add!
!

PrimitiveUgen subclass: #BufDur
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BufDur comment: 'Current duration of soundfile in buffer.'!

!BufDur class methodsFor: 'instance creation'!
bufnum: bufnum
    ^UgenBuilder name: 'BufDur' numChan: 1 rateSpec: (Rate kr) specialIndex: 0 inputArray: {bufnum}!
primaryFactoryMethod
    ^#bufnum:!
bufnum: bufnum mul: mul
    ^(BufDur bufnum: bufnum) * mul!
bufnum: bufnum mul: mul add: add
    ^(BufDur bufnum: bufnum) * mul + add!
!

PrimitiveUgen subclass: #BufFrames
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BufFrames comment: 'Current number of frames allocated in the buffer.'!

!BufFrames class methodsFor: 'instance creation'!
bufnum: bufnum
    ^UgenBuilder name: 'BufFrames' numChan: 1 rateSpec: (Rate kr) specialIndex: 0 inputArray: {bufnum}!
primaryFactoryMethod
    ^#bufnum:!
bufnum: bufnum mul: mul
    ^(BufFrames bufnum: bufnum) * mul!
bufnum: bufnum mul: mul add: add
    ^(BufFrames bufnum: bufnum) * mul + add!
!

PrimitiveUgen subclass: #BufRateScale
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BufRateScale comment: 'Buffer rate scaling in respect to server samplerate.'!

!BufRateScale class methodsFor: 'instance creation'!
bufnum: bufnum
    ^UgenBuilder name: 'BufRateScale' numChan: 1 rateSpec: (Rate kr) specialIndex: 0 inputArray: {bufnum}!
primaryFactoryMethod
    ^#bufnum:!
bufnum: bufnum mul: mul
    ^(BufRateScale bufnum: bufnum) * mul!
bufnum: bufnum mul: mul add: add
    ^(BufRateScale bufnum: bufnum) * mul + add!
!

PrimitiveUgen subclass: #BufRd
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BufRd comment: 'Buffer reading oscillator.'!

!BufRd class methodsFor: 'instance creation'!
numChan: numChan bufnum: bufnum phase: phase loop: loop interpolation: interpolation
    ^UgenBuilder name: 'BufRd' numChan: numChan rateSpec: (Rate ar) specialIndex: 0 inputArray: {bufnum. phase. loop. interpolation}!
primaryFactoryMethod
    ^#numChan:bufnum:phase:loop:interpolation:!
numChan: numChan bufnum: bufnum phase: phase loop: loop interpolation: interpolation mul: mul
    ^(BufRd numChan: numChan bufnum: bufnum phase: phase loop: loop interpolation: interpolation) * mul!
numChan: numChan bufnum: bufnum phase: phase loop: loop interpolation: interpolation mul: mul add: add
    ^(BufRd numChan: numChan bufnum: bufnum phase: phase loop: loop interpolation: interpolation) * mul + add!
!

PrimitiveUgen subclass: #BufSampleRate
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BufSampleRate comment: 'Buffer sample rate.'!

!BufSampleRate class methodsFor: 'instance creation'!
bufnum: bufnum
    ^UgenBuilder name: 'BufSampleRate' numChan: 1 rateSpec: (Rate kr) specialIndex: 0 inputArray: {bufnum}!
primaryFactoryMethod
    ^#bufnum:!
bufnum: bufnum mul: mul
    ^(BufSampleRate bufnum: bufnum) * mul!
bufnum: bufnum mul: mul add: add
    ^(BufSampleRate bufnum: bufnum) * mul + add!
!

PrimitiveUgen subclass: #BufWr
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BufWr comment: 'Buffer writing oscillator.'!

!BufWr class methodsFor: 'instance creation'!
bufnum: bufnum phase: phase loop: loop inputArray: inputArray
    ^UgenBuilder name: 'BufWr' numChan: 1 rateSpec: #(4) specialIndex: 0 inputArray: ({bufnum. phase. loop} , (inputArray asArray))!
primaryFactoryMethod
    ^#bufnum:phase:loop:inputArray:!
bufnum: bufnum phase: phase loop: loop inputArray: inputArray mul: mul
    ^(BufWr bufnum: bufnum phase: phase loop: loop inputArray: inputArray) * mul!
bufnum: bufnum phase: phase loop: loop inputArray: inputArray mul: mul add: add
    ^(BufWr bufnum: bufnum phase: phase loop: loop inputArray: inputArray) * mul + add!
!

PrimitiveUgen subclass: #ClearBuf
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

ClearBuf comment: '(Undocumented class)'!

!ClearBuf class methodsFor: 'instance creation'!
buf: buf
    ^UgenBuilder name: 'ClearBuf' numChan: 1 rateSpec: (Rate ir) specialIndex: 0 inputArray: {buf}!
primaryFactoryMethod
    ^#buf:!
buf: buf mul: mul
    ^(ClearBuf buf: buf) * mul!
buf: buf mul: mul add: add
    ^(ClearBuf buf: buf) * mul + add!
!

PrimitiveUgen subclass: #Clip
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Clip comment: 'Clip a signal outside given thresholds.'!

!Clip class methodsFor: 'instance creation'!
in: in lo: lo hi: hi
    ^UgenBuilder name: 'Clip' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. lo. hi}!
primaryFactoryMethod
    ^#in:lo:hi:!
in: in lo: lo hi: hi mul: mul
    ^(Clip in: in lo: lo hi: hi) * mul!
in: in lo: lo hi: hi mul: mul add: add
    ^(Clip in: in lo: lo hi: hi) * mul + add!
!

PrimitiveUgen subclass: #ClipNoise
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

ClipNoise comment: 'Clip Noise.'!

!ClipNoise class methodsFor: 'instance creation'!
new
    ^UgenBuilder name: 'ClipNoise' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {}!
primaryFactoryMethod
    ^#new!
mul: mul
    ^(ClipNoise new) * mul!
mul: mul add: add
    ^(ClipNoise new) * mul + add!
!

PrimitiveUgen subclass: #CoinGate
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

CoinGate comment: 'Statistical gate.'!

!CoinGate class methodsFor: 'instance creation'!
prob: prob in: in
    ^UgenBuilder name: 'CoinGate' numChan: 1 rateSpec: #(2) specialIndex: 0 inputArray: {prob. in}!
primaryFactoryMethod
    ^#prob:in:!
prob: prob in: in mul: mul
    ^(CoinGate prob: prob in: in) * mul!
prob: prob in: in mul: mul add: add
    ^(CoinGate prob: prob in: in) * mul + add!
!

PrimitiveUgen subclass: #CombC
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

CombC comment: 'Comb delay line with cubic interpolation.'!

!CombC class methodsFor: 'instance creation'!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
    ^UgenBuilder name: 'CombC' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. maxdelaytime. delaytime. decaytime}!
primaryFactoryMethod
    ^#in:maxdelaytime:delaytime:decaytime:!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul
    ^(CombC in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add
    ^(CombC in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul + add!
!

PrimitiveUgen subclass: #CombL
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

CombL comment: 'Comb delay line with linear interpolation.'!

!CombL class methodsFor: 'instance creation'!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
    ^UgenBuilder name: 'CombL' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. maxdelaytime. delaytime. decaytime}!
primaryFactoryMethod
    ^#in:maxdelaytime:delaytime:decaytime:!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul
    ^(CombL in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add
    ^(CombL in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul + add!
!

PrimitiveUgen subclass: #CombN
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

CombN comment: 'Comb delay line with no interpolation.'!

!CombN class methodsFor: 'instance creation'!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
    ^UgenBuilder name: 'CombN' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. maxdelaytime. delaytime. decaytime}!
primaryFactoryMethod
    ^#in:maxdelaytime:delaytime:decaytime:!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul
    ^(CombN in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add
    ^(CombN in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime) * mul + add!
!

PrimitiveUgen subclass: #ControlDur
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

ControlDur comment: 'Duration of one block'!

!ControlDur class methodsFor: 'instance creation'!
new
    ^UgenBuilder name: 'ControlDur' numChan: 1 rateSpec: (Rate ir) specialIndex: 0 inputArray: {}!
primaryFactoryMethod
    ^#new!
mul: mul
    ^(ControlDur new) * mul!
mul: mul add: add
    ^(ControlDur new) * mul + add!
!

PrimitiveUgen subclass: #ControlRate
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

ControlRate comment: 'Server control rate.'!

!ControlRate class methodsFor: 'instance creation'!
new
    ^UgenBuilder name: 'ControlRate' numChan: 1 rateSpec: (Rate ir) specialIndex: 0 inputArray: {}!
primaryFactoryMethod
    ^#new!
mul: mul
    ^(ControlRate new) * mul!
mul: mul add: add
    ^(ControlRate new) * mul + add!
!

PrimitiveUgen subclass: #Convolution
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Convolution comment: 'Real-time convolver.'!

!Convolution class methodsFor: 'instance creation'!
in: in kernel: kernel framesize: framesize
    ^UgenBuilder name: 'Convolution' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {in. kernel. framesize}!
primaryFactoryMethod
    ^#in:kernel:framesize:!
in: in kernel: kernel framesize: framesize mul: mul
    ^(Convolution in: in kernel: kernel framesize: framesize) * mul!
in: in kernel: kernel framesize: framesize mul: mul add: add
    ^(Convolution in: in kernel: kernel framesize: framesize) * mul + add!
!

PrimitiveUgen subclass: #Crackle
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Crackle comment: 'Chaotic noise function.'!

!Crackle class methodsFor: 'instance creation'!
chaosParam: chaosParam
    ^UgenBuilder name: 'Crackle' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {chaosParam}!
primaryFactoryMethod
    ^#chaosParam:!
chaosParam: chaosParam mul: mul
    ^(Crackle chaosParam: chaosParam) * mul!
chaosParam: chaosParam mul: mul add: add
    ^(Crackle chaosParam: chaosParam) * mul + add!
!

PrimitiveUgen subclass: #CuspL
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

CuspL comment: 'Cusp map chaotic generator'!

!CuspL class methodsFor: 'instance creation'!
freq: freq a: a b: b xi: xi
    ^UgenBuilder name: 'CuspL' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq. a. b. xi}!
primaryFactoryMethod
    ^#freq:a:b:xi:!
freq: freq a: a b: b xi: xi mul: mul
    ^(CuspL freq: freq a: a b: b xi: xi) * mul!
freq: freq a: a b: b xi: xi mul: mul add: add
    ^(CuspL freq: freq a: a b: b xi: xi) * mul + add!
!

PrimitiveUgen subclass: #Dbufrd
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Dbufrd comment: 'Buffer read demand ugen'!

!Dbufrd class methodsFor: 'instance creation'!
bufnum: bufnum phase: phase loop: loop
    ^UgenBuilder name: 'Dbufrd' numChan: 1 rateSpec: (Rate dr) specialIndex: 0 inputArray: {bufnum. phase. loop}!
primaryFactoryMethod
    ^#bufnum:phase:loop:!
bufnum: bufnum phase: phase loop: loop mul: mul
    ^(Dbufrd bufnum: bufnum phase: phase loop: loop) * mul!
bufnum: bufnum phase: phase loop: loop mul: mul add: add
    ^(Dbufrd bufnum: bufnum phase: phase loop: loop) * mul + add!
!

PrimitiveUgen subclass: #Dbufwr
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Dbufwr comment: 'Buffer write demand ugen'!

!Dbufwr class methodsFor: 'instance creation'!
bufnum: bufnum phase: phase input: input loop: loop
    ^UgenBuilder name: 'Dbufwr' numChan: 1 rateSpec: (Rate dr) specialIndex: 0 inputArray: {bufnum. phase. input. loop}!
primaryFactoryMethod
    ^#bufnum:phase:input:loop:!
bufnum: bufnum phase: phase input: input loop: loop mul: mul
    ^(Dbufwr bufnum: bufnum phase: phase input: input loop: loop) * mul!
bufnum: bufnum phase: phase input: input loop: loop mul: mul add: add
    ^(Dbufwr bufnum: bufnum phase: phase input: input loop: loop) * mul + add!
!

PrimitiveUgen subclass: #DC
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

DC comment: 'Create a constant amplitude signal'!

!DC class methodsFor: 'instance creation'!
in: in
    ^UgenBuilder name: 'DC' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {in}!
primaryFactoryMethod
    ^#in:!
in: in mul: mul
    ^(DC in: in) * mul!
in: in mul: mul add: add
    ^(DC in: in) * mul + add!
!

PrimitiveUgen subclass: #Decay
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Decay comment: 'Exponential decay'!

!Decay class methodsFor: 'instance creation'!
in: in decayTime: decayTime
    ^UgenBuilder name: 'Decay' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. decayTime}!
primaryFactoryMethod
    ^#in:decayTime:!
in: in decayTime: decayTime mul: mul
    ^(Decay in: in decayTime: decayTime) * mul!
in: in decayTime: decayTime mul: mul add: add
    ^(Decay in: in decayTime: decayTime) * mul + add!
!

PrimitiveUgen subclass: #Decay2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Decay2 comment: 'Exponential decay'!

!Decay2 class methodsFor: 'instance creation'!
in: in attackTime: attackTime decayTime: decayTime
    ^UgenBuilder name: 'Decay2' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. attackTime. decayTime}!
primaryFactoryMethod
    ^#in:attackTime:decayTime:!
in: in attackTime: attackTime decayTime: decayTime mul: mul
    ^(Decay2 in: in attackTime: attackTime decayTime: decayTime) * mul!
in: in attackTime: attackTime decayTime: decayTime mul: mul add: add
    ^(Decay2 in: in attackTime: attackTime decayTime: decayTime) * mul + add!
!

PrimitiveUgen subclass: #DegreeToKey
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

DegreeToKey comment: 'Convert signal to modal pitch.'!

!DegreeToKey class methodsFor: 'instance creation'!
bufnum: bufnum in: in octave: octave
    ^UgenBuilder name: 'DegreeToKey' numChan: 1 rateSpec: #(2) specialIndex: 0 inputArray: {bufnum. in. octave}!
primaryFactoryMethod
    ^#bufnum:in:octave:!
bufnum: bufnum in: in octave: octave mul: mul
    ^(DegreeToKey bufnum: bufnum in: in octave: octave) * mul!
bufnum: bufnum in: in octave: octave mul: mul add: add
    ^(DegreeToKey bufnum: bufnum in: in octave: octave) * mul + add!
!

PrimitiveUgen subclass: #Delay1
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Delay1 comment: 'Single sample delay.'!

!Delay1 class methodsFor: 'instance creation'!
in: in
    ^UgenBuilder name: 'Delay1' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in}!
primaryFactoryMethod
    ^#in:!
in: in mul: mul
    ^(Delay1 in: in) * mul!
in: in mul: mul add: add
    ^(Delay1 in: in) * mul + add!
!

PrimitiveUgen subclass: #Delay2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Delay2 comment: 'Two sample delay.'!

!Delay2 class methodsFor: 'instance creation'!
in: in
    ^UgenBuilder name: 'Delay2' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in}!
primaryFactoryMethod
    ^#in:!
in: in mul: mul
    ^(Delay2 in: in) * mul!
in: in mul: mul add: add
    ^(Delay2 in: in) * mul + add!
!

PrimitiveUgen subclass: #DelayC
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

DelayC comment: 'Simple delay line with cubic interpolation.'!

!DelayC class methodsFor: 'instance creation'!
in: in maxdelaytime: maxdelaytime delaytime: delaytime
    ^UgenBuilder name: 'DelayC' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. maxdelaytime. delaytime}!
primaryFactoryMethod
    ^#in:maxdelaytime:delaytime:!
in: in maxdelaytime: maxdelaytime delaytime: delaytime mul: mul
    ^(DelayC in: in maxdelaytime: maxdelaytime delaytime: delaytime) * mul!
in: in maxdelaytime: maxdelaytime delaytime: delaytime mul: mul add: add
    ^(DelayC in: in maxdelaytime: maxdelaytime delaytime: delaytime) * mul + add!
!

PrimitiveUgen subclass: #DelayL
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

DelayL comment: 'Simple delay line with linear interpolation.'!

!DelayL class methodsFor: 'instance creation'!
in: in maxdelaytime: maxdelaytime delaytime: delaytime
    ^UgenBuilder name: 'DelayL' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. maxdelaytime. delaytime}!
primaryFactoryMethod
    ^#in:maxdelaytime:delaytime:!
in: in maxdelaytime: maxdelaytime delaytime: delaytime mul: mul
    ^(DelayL in: in maxdelaytime: maxdelaytime delaytime: delaytime) * mul!
in: in maxdelaytime: maxdelaytime delaytime: delaytime mul: mul add: add
    ^(DelayL in: in maxdelaytime: maxdelaytime delaytime: delaytime) * mul + add!
!

PrimitiveUgen subclass: #DelayN
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

DelayN comment: 'Simple delay line with no interpolation.'!

!DelayN class methodsFor: 'instance creation'!
in: in maxdelaytime: maxdelaytime delaytime: delaytime
    ^UgenBuilder name: 'DelayN' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. maxdelaytime. delaytime}!
primaryFactoryMethod
    ^#in:maxdelaytime:delaytime:!
in: in maxdelaytime: maxdelaytime delaytime: delaytime mul: mul
    ^(DelayN in: in maxdelaytime: maxdelaytime delaytime: delaytime) * mul!
in: in maxdelaytime: maxdelaytime delaytime: delaytime mul: mul add: add
    ^(DelayN in: in maxdelaytime: maxdelaytime delaytime: delaytime) * mul + add!
!

PrimitiveUgen subclass: #Demand
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Demand comment: 'Demand results from demand rate UGens.'!

!Demand class methodsFor: 'instance creation'!
trig: trig reset: reset demandUGens: demandUGens
    ^UgenBuilder name: 'Demand' numChan: demandUGens asArray size rateSpec: #(1) specialIndex: 0 inputArray: ({trig. reset} , (demandUGens asArray))!
primaryFactoryMethod
    ^#trig:reset:demandUGens:!
trig: trig reset: reset demandUGens: demandUGens mul: mul
    ^(Demand trig: trig reset: reset demandUGens: demandUGens) * mul!
trig: trig reset: reset demandUGens: demandUGens mul: mul add: add
    ^(Demand trig: trig reset: reset demandUGens: demandUGens) * mul + add!
!

PrimitiveUgen subclass: #DetectSilence
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

DetectSilence comment: 'Detect when input falls below an amplitude threshold'!

!DetectSilence class methodsFor: 'instance creation'!
in: in amp: amp time: time doneAction: doneAction
    ^UgenBuilder name: 'DetectSilence' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. amp. time. doneAction}!
primaryFactoryMethod
    ^#in:amp:time:doneAction:!
in: in amp: amp time: time doneAction: doneAction mul: mul
    ^(DetectSilence in: in amp: amp time: time doneAction: doneAction) * mul!
in: in amp: amp time: time doneAction: doneAction mul: mul add: add
    ^(DetectSilence in: in amp: amp time: time doneAction: doneAction) * mul + add!
!

PrimitiveUgen subclass: #Diwhite
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Diwhite comment: 'Demand rate white noise random generator.'!

!Diwhite class methodsFor: 'instance creation'!
length: length lo: lo hi: hi
    ^UgenBuilder name: 'Diwhite' numChan: 1 rateSpec: (Rate dr) specialIndex: 0 inputArray: {length. lo. hi}!
primaryFactoryMethod
    ^#length:lo:hi:!
length: length lo: lo hi: hi mul: mul
    ^(Diwhite length: length lo: lo hi: hi) * mul!
length: length lo: lo hi: hi mul: mul add: add
    ^(Diwhite length: length lo: lo hi: hi) * mul + add!
!

PrimitiveUgen subclass: #Drand
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Drand comment: 'Demand rate random sequence generator.'!

!Drand class methodsFor: 'instance creation'!
repeats: repeats list: list
    ^UgenBuilder name: 'Drand' numChan: 1 rateSpec: (Rate dr) specialIndex: 0 inputArray: ({repeats} , (list asArray))!
primaryFactoryMethod
    ^#repeats:list:!
repeats: repeats list: list mul: mul
    ^(Drand repeats: repeats list: list) * mul!
repeats: repeats list: list mul: mul add: add
    ^(Drand repeats: repeats list: list) * mul + add!
!

PrimitiveUgen subclass: #Dseq
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Dseq comment: 'Demand rate sequence generator.'!

!Dseq class methodsFor: 'instance creation'!
repeats: repeats list: list
    ^UgenBuilder name: 'Dseq' numChan: 1 rateSpec: (Rate dr) specialIndex: 0 inputArray: ({repeats} , (list asArray))!
primaryFactoryMethod
    ^#repeats:list:!
repeats: repeats list: list mul: mul
    ^(Dseq repeats: repeats list: list) * mul!
repeats: repeats list: list mul: mul add: add
    ^(Dseq repeats: repeats list: list) * mul + add!
!

PrimitiveUgen subclass: #Dseries
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Dseries comment: 'Demand rate arithmetic series UGen.'!

!Dseries class methodsFor: 'instance creation'!
length: length start: start step: step
    ^UgenBuilder name: 'Dseries' numChan: 1 rateSpec: (Rate dr) specialIndex: 0 inputArray: {length. start. step}!
primaryFactoryMethod
    ^#length:start:step:!
length: length start: start step: step mul: mul
    ^(Dseries length: length start: start step: step) * mul!
length: length start: start step: step mul: mul add: add
    ^(Dseries length: length start: start step: step) * mul + add!
!

PrimitiveUgen subclass: #Dshuf
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Dshuf comment: 'Demand rate random sequence generator'!

!Dshuf class methodsFor: 'instance creation'!
repeats: repeats list: list
    ^UgenBuilder name: 'Dshuf' numChan: 1 rateSpec: (Rate dr) specialIndex: 0 inputArray: ({repeats} , (list asArray))!
primaryFactoryMethod
    ^#repeats:list:!
repeats: repeats list: list mul: mul
    ^(Dshuf repeats: repeats list: list) * mul!
repeats: repeats list: list mul: mul add: add
    ^(Dshuf repeats: repeats list: list) * mul + add!
!

PrimitiveUgen subclass: #Dust
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Dust comment: 'Random impulses.'!

!Dust class methodsFor: 'instance creation'!
density: density
    ^UgenBuilder name: 'Dust' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {density}!
primaryFactoryMethod
    ^#density:!
density: density mul: mul
    ^(Dust density: density) * mul!
density: density mul: mul add: add
    ^(Dust density: density) * mul + add!
!

PrimitiveUgen subclass: #Dust2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Dust2 comment: 'Random impulses.'!

!Dust2 class methodsFor: 'instance creation'!
density: density
    ^UgenBuilder name: 'Dust2' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {density}!
primaryFactoryMethod
    ^#density:!
density: density mul: mul
    ^(Dust2 density: density) * mul!
density: density mul: mul add: add
    ^(Dust2 density: density) * mul + add!
!

PrimitiveUgen subclass: #Duty
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Duty comment: 'Demand results from demand rate UGens.'!

!Duty class methodsFor: 'instance creation'!
dur: dur reset: reset doneAction: doneAction level: level
    ^UgenBuilder name: 'Duty' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {dur. reset. doneAction. level}!
primaryFactoryMethod
    ^#dur:reset:doneAction:level:!
dur: dur reset: reset doneAction: doneAction level: level mul: mul
    ^(Duty dur: dur reset: reset doneAction: doneAction level: level) * mul!
dur: dur reset: reset doneAction: doneAction level: level mul: mul add: add
    ^(Duty dur: dur reset: reset doneAction: doneAction level: level) * mul + add!
!

PrimitiveUgen subclass: #DWGPluckedStiff
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

DWGPluckedStiff comment: 'Plucked physical model.'!

!DWGPluckedStiff class methodsFor: 'instance creation'!
freq: freq amp: amp gate: gate pos: pos c1: c1 c3: c3 inp: inp release: release fB: fB
    ^UgenBuilder name: 'DWGPluckedStiff' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq. amp. gate. pos. c1. c3. inp. release. fB}!
primaryFactoryMethod
    ^#freq:amp:gate:pos:c1:c3:inp:release:fB:!
freq: freq amp: amp gate: gate pos: pos c1: c1 c3: c3 inp: inp release: release fB: fB mul: mul
    ^(DWGPluckedStiff freq: freq amp: amp gate: gate pos: pos c1: c1 c3: c3 inp: inp release: release fB: fB) * mul!
freq: freq amp: amp gate: gate pos: pos c1: c1 c3: c3 inp: inp release: release fB: fB mul: mul add: add
    ^(DWGPluckedStiff freq: freq amp: amp gate: gate pos: pos c1: c1 c3: c3 inp: inp release: release fB: fB) * mul + add!
!

PrimitiveUgen subclass: #EnvGen
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

EnvGen comment: 'Envelope generator'!

!EnvGen class methodsFor: 'instance creation'!
gate: gate levelScale: levelScale levelBias: levelBias timeScale: timeScale doneAction: doneAction envelope: envelope
    ^UgenBuilder name: 'EnvGen' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: ({gate. levelScale. levelBias. timeScale. doneAction} , (envelope asArray))!
primaryFactoryMethod
    ^#gate:levelScale:levelBias:timeScale:doneAction:envelope:!
gate: gate levelScale: levelScale levelBias: levelBias timeScale: timeScale doneAction: doneAction envelope: envelope mul: mul
    ^(EnvGen gate: gate levelScale: levelScale levelBias: levelBias timeScale: timeScale doneAction: doneAction envelope: envelope) * mul!
gate: gate levelScale: levelScale levelBias: levelBias timeScale: timeScale doneAction: doneAction envelope: envelope mul: mul add: add
    ^(EnvGen gate: gate levelScale: levelScale levelBias: levelBias timeScale: timeScale doneAction: doneAction envelope: envelope) * mul + add!
!

PrimitiveUgen subclass: #ExpRand
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

ExpRand comment: 'Exponential single random number generator.'!

!ExpRand class methodsFor: 'instance creation'!
lo: lo hi: hi
    ^UgenBuilder name: 'ExpRand' numChan: 1 rateSpec: (Rate ir) specialIndex: 0 inputArray: {lo. hi}!
primaryFactoryMethod
    ^#lo:hi:!
lo: lo hi: hi mul: mul
    ^(ExpRand lo: lo hi: hi) * mul!
lo: lo hi: hi mul: mul add: add
    ^(ExpRand lo: lo hi: hi) * mul + add!
!

PrimitiveUgen subclass: #FBSineL
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

FBSineL comment: 'Feedback sine with chaotic phase indexing'!

!FBSineL class methodsFor: 'instance creation'!
freq: freq im: im fb: fb a: a c: c xi: xi yi: yi
    ^UgenBuilder name: 'FBSineL' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq. im. fb. a. c. xi. yi}!
primaryFactoryMethod
    ^#freq:im:fb:a:c:xi:yi:!
freq: freq im: im fb: fb a: a c: c xi: xi yi: yi mul: mul
    ^(FBSineL freq: freq im: im fb: fb a: a c: c xi: xi yi: yi) * mul!
freq: freq im: im fb: fb a: a c: c xi: xi yi: yi mul: mul add: add
    ^(FBSineL freq: freq im: im fb: fb a: a c: c xi: xi yi: yi) * mul + add!
!

PrimitiveUgen subclass: #FBSineC
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

FBSineC comment: 'Feedback sine with chaotic phase indexing'!

!FBSineC class methodsFor: 'instance creation'!
freq: freq im: im fb: fb a: a c: c xi: xi yi: yi
    ^UgenBuilder name: 'FBSineC' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq. im. fb. a. c. xi. yi}!
primaryFactoryMethod
    ^#freq:im:fb:a:c:xi:yi:!
freq: freq im: im fb: fb a: a c: c xi: xi yi: yi mul: mul
    ^(FBSineC freq: freq im: im fb: fb a: a c: c xi: xi yi: yi) * mul!
freq: freq im: im fb: fb a: a c: c xi: xi yi: yi mul: mul add: add
    ^(FBSineC freq: freq im: im fb: fb a: a c: c xi: xi yi: yi) * mul + add!
!

PrimitiveUgen subclass: #FFT
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

FFT comment: 'Fast Fourier Transform'!

!FFT class methodsFor: 'instance creation'!
buffer: buffer in: in hop: hop wintype: wintype active: active winsize: winsize
    ^UgenBuilder name: 'FFT' numChan: 1 rateSpec: (Rate kr) specialIndex: 0 inputArray: {buffer. in. hop. wintype. active. winsize}!
primaryFactoryMethod
    ^#buffer:in:hop:wintype:active:winsize:!
buffer: buffer in: in hop: hop wintype: wintype active: active winsize: winsize mul: mul
    ^(FFT buffer: buffer in: in hop: hop wintype: wintype active: active winsize: winsize) * mul!
buffer: buffer in: in hop: hop wintype: wintype active: active winsize: winsize mul: mul add: add
    ^(FFT buffer: buffer in: in hop: hop wintype: wintype active: active winsize: winsize) * mul + add!
!

PrimitiveUgen subclass: #Fold
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Fold comment: 'Fold a signal outside given thresholds.'!

!Fold class methodsFor: 'instance creation'!
in: in lo: lo hi: hi
    ^UgenBuilder name: 'Fold' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. lo. hi}!
primaryFactoryMethod
    ^#in:lo:hi:!
in: in lo: lo hi: hi mul: mul
    ^(Fold in: in lo: lo hi: hi) * mul!
in: in lo: lo hi: hi mul: mul add: add
    ^(Fold in: in lo: lo hi: hi) * mul + add!
!

PrimitiveUgen subclass: #Formant
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Formant comment: 'Formant oscillator'!

!Formant class methodsFor: 'instance creation'!
fundfreq: fundfreq formfreq: formfreq bwfreq: bwfreq
    ^UgenBuilder name: 'Formant' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {fundfreq. formfreq. bwfreq}!
primaryFactoryMethod
    ^#fundfreq:formfreq:bwfreq:!
fundfreq: fundfreq formfreq: formfreq bwfreq: bwfreq mul: mul
    ^(Formant fundfreq: fundfreq formfreq: formfreq bwfreq: bwfreq) * mul!
fundfreq: fundfreq formfreq: formfreq bwfreq: bwfreq mul: mul add: add
    ^(Formant fundfreq: fundfreq formfreq: formfreq bwfreq: bwfreq) * mul + add!
!

PrimitiveUgen subclass: #Formlet
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Formlet comment: 'FOF-like filter.'!

!Formlet class methodsFor: 'instance creation'!
in: in freq: freq attacktime: attacktime decaytime: decaytime
    ^UgenBuilder name: 'Formlet' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. freq. attacktime. decaytime}!
primaryFactoryMethod
    ^#in:freq:attacktime:decaytime:!
in: in freq: freq attacktime: attacktime decaytime: decaytime mul: mul
    ^(Formlet in: in freq: freq attacktime: attacktime decaytime: decaytime) * mul!
in: in freq: freq attacktime: attacktime decaytime: decaytime mul: mul add: add
    ^(Formlet in: in freq: freq attacktime: attacktime decaytime: decaytime) * mul + add!
!

PrimitiveUgen subclass: #FOS
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

FOS comment: 'First order filter section.'!

!FOS class methodsFor: 'instance creation'!
in: in a0: a0 a1: a1 b1: b1
    ^UgenBuilder name: 'FOS' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. a0. a1. b1}!
primaryFactoryMethod
    ^#in:a0:a1:b1:!
in: in a0: a0 a1: a1 b1: b1 mul: mul
    ^(FOS in: in a0: a0 a1: a1 b1: b1) * mul!
in: in a0: a0 a1: a1 b1: b1 mul: mul add: add
    ^(FOS in: in a0: a0 a1: a1 b1: b1) * mul + add!
!

PrimitiveUgen subclass: #FreqShift
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

FreqShift comment: 'Frequency Shifter.'!

!FreqShift class methodsFor: 'instance creation'!
in: in freq: freq phase: phase
    ^UgenBuilder name: 'FreqShift' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {in. freq. phase}!
primaryFactoryMethod
    ^#in:freq:phase:!
in: in freq: freq phase: phase mul: mul
    ^(FreqShift in: in freq: freq phase: phase) * mul!
in: in freq: freq phase: phase mul: mul add: add
    ^(FreqShift in: in freq: freq phase: phase) * mul + add!
!

PrimitiveUgen subclass: #FSinOsc
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

FSinOsc comment: 'Fast sine oscillator.'!

!FSinOsc class methodsFor: 'instance creation'!
freq: freq iphase: iphase
    ^UgenBuilder name: 'FSinOsc' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq. iphase}!
primaryFactoryMethod
    ^#freq:iphase:!
freq: freq iphase: iphase mul: mul
    ^(FSinOsc freq: freq iphase: iphase) * mul!
freq: freq iphase: iphase mul: mul add: add
    ^(FSinOsc freq: freq iphase: iphase) * mul + add!
!

PrimitiveUgen subclass: #FreeVerb
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

FreeVerb comment: 'A reverb'!

!FreeVerb class methodsFor: 'instance creation'!
in: in mix: mix room: room damp: damp
    ^UgenBuilder name: 'FreeVerb' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. mix. room. damp}!
primaryFactoryMethod
    ^#in:mix:room:damp:!
in: in mix: mix room: room damp: damp mul: mul
    ^(FreeVerb in: in mix: mix room: room damp: damp) * mul!
in: in mix: mix room: room damp: damp mul: mul add: add
    ^(FreeVerb in: in mix: mix room: room damp: damp) * mul + add!
!

PrimitiveUgen subclass: #FreeVerb2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

FreeVerb2 comment: 'A two-channel reverb'!

!FreeVerb2 class methodsFor: 'instance creation'!
in: in in2: in2 mix: mix room: room damp: damp
    ^UgenBuilder name: 'FreeVerb2' numChan: 2 rateSpec: #(1) specialIndex: 0 inputArray: {in. in2. mix. room. damp}!
primaryFactoryMethod
    ^#in:in2:mix:room:damp:!
in: in in2: in2 mix: mix room: room damp: damp mul: mul
    ^(FreeVerb2 in: in in2: in2 mix: mix room: room damp: damp) * mul!
in: in in2: in2 mix: mix room: room damp: damp mul: mul add: add
    ^(FreeVerb2 in: in in2: in2 mix: mix room: room damp: damp) * mul + add!
!

PrimitiveUgen subclass: #Gate
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Gate comment: 'Gate or hold.'!

!Gate class methodsFor: 'instance creation'!
in: in trig: trig
    ^UgenBuilder name: 'Gate' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. trig}!
primaryFactoryMethod
    ^#in:trig:!
in: in trig: trig mul: mul
    ^(Gate in: in trig: trig) * mul!
in: in trig: trig mul: mul add: add
    ^(Gate in: in trig: trig) * mul + add!
!

PrimitiveUgen subclass: #Gendy1
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Gendy1 comment: 'Dynamic stochastic synthesis generator.'!

!Gendy1 class methodsFor: 'instance creation'!
ampdist: ampdist durdist: durdist adparam: adparam ddparam: ddparam minfreq: minfreq maxfreq: maxfreq ampscale: ampscale durscale: durscale initCPs: initCPs knum: knum
    ^UgenBuilder name: 'Gendy1' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {ampdist. durdist. adparam. ddparam. minfreq. maxfreq. ampscale. durscale. initCPs. knum}!
primaryFactoryMethod
    ^#ampdist:durdist:adparam:ddparam:minfreq:maxfreq:ampscale:durscale:initCPs:knum:!
ampdist: ampdist durdist: durdist adparam: adparam ddparam: ddparam minfreq: minfreq maxfreq: maxfreq ampscale: ampscale durscale: durscale initCPs: initCPs knum: knum mul: mul
    ^(Gendy1 ampdist: ampdist durdist: durdist adparam: adparam ddparam: ddparam minfreq: minfreq maxfreq: maxfreq ampscale: ampscale durscale: durscale initCPs: initCPs knum: knum) * mul!
ampdist: ampdist durdist: durdist adparam: adparam ddparam: ddparam minfreq: minfreq maxfreq: maxfreq ampscale: ampscale durscale: durscale initCPs: initCPs knum: knum mul: mul add: add
    ^(Gendy1 ampdist: ampdist durdist: durdist adparam: adparam ddparam: ddparam minfreq: minfreq maxfreq: maxfreq ampscale: ampscale durscale: durscale initCPs: initCPs knum: knum) * mul + add!
!

PrimitiveUgen subclass: #GrainFM
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

GrainFM comment: 'Granular synthesis with frequency modulated sine tones'!

!GrainFM class methodsFor: 'instance creation'!
numChan: numChan trigger: trigger dur: dur carfreq: carfreq modfreq: modfreq index: index pan: pan envbufnum: envbufnum maxGrains: maxGrains
    ^UgenBuilder name: 'GrainFM' numChan: numChan rateSpec: (Rate ar) specialIndex: 0 inputArray: {trigger. dur. carfreq. modfreq. index. pan. envbufnum. maxGrains}!
primaryFactoryMethod
    ^#numChan:trigger:dur:carfreq:modfreq:index:pan:envbufnum:maxGrains:!
numChan: numChan trigger: trigger dur: dur carfreq: carfreq modfreq: modfreq index: index pan: pan envbufnum: envbufnum maxGrains: maxGrains mul: mul
    ^(GrainFM numChan: numChan trigger: trigger dur: dur carfreq: carfreq modfreq: modfreq index: index pan: pan envbufnum: envbufnum maxGrains: maxGrains) * mul!
numChan: numChan trigger: trigger dur: dur carfreq: carfreq modfreq: modfreq index: index pan: pan envbufnum: envbufnum maxGrains: maxGrains mul: mul add: add
    ^(GrainFM numChan: numChan trigger: trigger dur: dur carfreq: carfreq modfreq: modfreq index: index pan: pan envbufnum: envbufnum maxGrains: maxGrains) * mul + add!
!

PrimitiveUgen subclass: #GrainSin
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

GrainSin comment: 'Granular synthesis with sine tones'!

!GrainSin class methodsFor: 'instance creation'!
numChan: numChan trigger: trigger dur: dur freq: freq pan: pan envbufnum: envbufnum maxGrains: maxGrains
    ^UgenBuilder name: 'GrainSin' numChan: numChan rateSpec: (Rate ar) specialIndex: 0 inputArray: {trigger. dur. freq. pan. envbufnum. maxGrains}!
primaryFactoryMethod
    ^#numChan:trigger:dur:freq:pan:envbufnum:maxGrains:!
numChan: numChan trigger: trigger dur: dur freq: freq pan: pan envbufnum: envbufnum maxGrains: maxGrains mul: mul
    ^(GrainSin numChan: numChan trigger: trigger dur: dur freq: freq pan: pan envbufnum: envbufnum maxGrains: maxGrains) * mul!
numChan: numChan trigger: trigger dur: dur freq: freq pan: pan envbufnum: envbufnum maxGrains: maxGrains mul: mul add: add
    ^(GrainSin numChan: numChan trigger: trigger dur: dur freq: freq pan: pan envbufnum: envbufnum maxGrains: maxGrains) * mul + add!
!

PrimitiveUgen subclass: #GrayNoise
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

GrayNoise comment: 'Gray Noise.'!

!GrayNoise class methodsFor: 'instance creation'!
new
    ^UgenBuilder name: 'GrayNoise' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {}!
primaryFactoryMethod
    ^#new!
mul: mul
    ^(GrayNoise new) * mul!
mul: mul add: add
    ^(GrayNoise new) * mul + add!
!

PrimitiveUgen subclass: #GVerb
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

GVerb comment: 'A two-channel reverb'!

!GVerb class methodsFor: 'instance creation'!
in: in roomsize: roomsize revtime: revtime damping: damping inputbw: inputbw spread: spread drylevel: drylevel earlyreflevel: earlyreflevel taillevel: taillevel maxroomsize: maxroomsize
    ^UgenBuilder name: 'GVerb' numChan: 2 rateSpec: #(1) specialIndex: 0 inputArray: {in. roomsize. revtime. damping. inputbw. spread. drylevel. earlyreflevel. taillevel. maxroomsize}!
primaryFactoryMethod
    ^#in:roomsize:revtime:damping:inputbw:spread:drylevel:earlyreflevel:taillevel:maxroomsize:!
in: in roomsize: roomsize revtime: revtime damping: damping inputbw: inputbw spread: spread drylevel: drylevel earlyreflevel: earlyreflevel taillevel: taillevel maxroomsize: maxroomsize mul: mul
    ^(GVerb in: in roomsize: roomsize revtime: revtime damping: damping inputbw: inputbw spread: spread drylevel: drylevel earlyreflevel: earlyreflevel taillevel: taillevel maxroomsize: maxroomsize) * mul!
in: in roomsize: roomsize revtime: revtime damping: damping inputbw: inputbw spread: spread drylevel: drylevel earlyreflevel: earlyreflevel taillevel: taillevel maxroomsize: maxroomsize mul: mul add: add
    ^(GVerb in: in roomsize: roomsize revtime: revtime damping: damping inputbw: inputbw spread: spread drylevel: drylevel earlyreflevel: earlyreflevel taillevel: taillevel maxroomsize: maxroomsize) * mul + add!
!

PrimitiveUgen subclass: #Hasher
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Hasher comment: 'Scrambled value with a hash function.'!

!Hasher class methodsFor: 'instance creation'!
in: in
    ^UgenBuilder name: 'Hasher' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in}!
primaryFactoryMethod
    ^#in:!
in: in mul: mul
    ^(Hasher in: in) * mul!
in: in mul: mul add: add
    ^(Hasher in: in) * mul + add!
!

PrimitiveUgen subclass: #HenonL
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

HenonL comment: 'Henon map chaotic generator'!

!HenonL class methodsFor: 'instance creation'!
freq: freq a: a b: b x0: x0 x1: x1
    ^UgenBuilder name: 'HenonL' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq. a. b. x0. x1}!
primaryFactoryMethod
    ^#freq:a:b:x0:x1:!
freq: freq a: a b: b x0: x0 x1: x1 mul: mul
    ^(HenonL freq: freq a: a b: b x0: x0 x1: x1) * mul!
freq: freq a: a b: b x0: x0 x1: x1 mul: mul add: add
    ^(HenonL freq: freq a: a b: b x0: x0 x1: x1) * mul + add!
!

PrimitiveUgen subclass: #HenonC
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

HenonC comment: 'Henon map chaotic generator'!

!HenonC class methodsFor: 'instance creation'!
freq: freq a: a b: b x0: x0 x1: x1
    ^UgenBuilder name: 'HenonC' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq. a. b. x0. x1}!
primaryFactoryMethod
    ^#freq:a:b:x0:x1:!
freq: freq a: a b: b x0: x0 x1: x1 mul: mul
    ^(HenonC freq: freq a: a b: b x0: x0 x1: x1) * mul!
freq: freq a: a b: b x0: x0 x1: x1 mul: mul add: add
    ^(HenonC freq: freq a: a b: b x0: x0 x1: x1) * mul + add!
!

PrimitiveUgen subclass: #HPF
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

HPF comment: '2nd order Butterworth highpass filter.'!

!HPF class methodsFor: 'instance creation'!
in: in freq: freq
    ^UgenBuilder name: 'HPF' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. freq}!
primaryFactoryMethod
    ^#in:freq:!
in: in freq: freq mul: mul
    ^(HPF in: in freq: freq) * mul!
in: in freq: freq mul: mul add: add
    ^(HPF in: in freq: freq) * mul + add!
!

PrimitiveUgen subclass: #HPZ1
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

HPZ1 comment: 'Two point difference filter'!

!HPZ1 class methodsFor: 'instance creation'!
in: in
    ^UgenBuilder name: 'HPZ1' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in}!
primaryFactoryMethod
    ^#in:!
in: in mul: mul
    ^(HPZ1 in: in) * mul!
in: in mul: mul add: add
    ^(HPZ1 in: in) * mul + add!
!

PrimitiveUgen subclass: #HPZ2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

HPZ2 comment: 'Two zero fixed midcut.'!

!HPZ2 class methodsFor: 'instance creation'!
in: in
    ^UgenBuilder name: 'HPZ2' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in}!
primaryFactoryMethod
    ^#in:!
in: in mul: mul
    ^(HPZ2 in: in) * mul!
in: in mul: mul add: add
    ^(HPZ2 in: in) * mul + add!
!

PrimitiveUgen subclass: #IFFT
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

IFFT comment: 'Inverse Fast Fourier Transform'!

!IFFT class methodsFor: 'instance creation'!
buffer: buffer wintype: wintype winsize: winsize
    ^UgenBuilder name: 'IFFT' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {buffer. wintype. winsize}!
primaryFactoryMethod
    ^#buffer:wintype:winsize:!
buffer: buffer wintype: wintype winsize: winsize mul: mul
    ^(IFFT buffer: buffer wintype: wintype winsize: winsize) * mul!
buffer: buffer wintype: wintype winsize: winsize mul: mul add: add
    ^(IFFT buffer: buffer wintype: wintype winsize: winsize) * mul + add!
!

PrimitiveUgen subclass: #Impulse
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Impulse comment: 'Impulse oscillator.'!

!Impulse class methodsFor: 'instance creation'!
freq: freq phase: phase
    ^UgenBuilder name: 'Impulse' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq. phase}!
primaryFactoryMethod
    ^#freq:phase:!
freq: freq phase: phase mul: mul
    ^(Impulse freq: freq phase: phase) * mul!
freq: freq phase: phase mul: mul add: add
    ^(Impulse freq: freq phase: phase) * mul + add!
!

PrimitiveUgen subclass: #In
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

In comment: 'Read a signal from a bus.'!

!In class methodsFor: 'instance creation'!
numChan: numChan bus: bus
    ^UgenBuilder name: 'In' numChan: numChan rateSpec: (Rate ar) specialIndex: 0 inputArray: {bus}!
primaryFactoryMethod
    ^#numChan:bus:!
numChan: numChan bus: bus mul: mul
    ^(In numChan: numChan bus: bus) * mul!
numChan: numChan bus: bus mul: mul add: add
    ^(In numChan: numChan bus: bus) * mul + add!
!

PrimitiveUgen subclass: #Index
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Index comment: 'Index into a table with a signal'!

!Index class methodsFor: 'instance creation'!
bufnum: bufnum in: in
    ^UgenBuilder name: 'Index' numChan: 1 rateSpec: #(2) specialIndex: 0 inputArray: {bufnum. in}!
primaryFactoryMethod
    ^#bufnum:in:!
bufnum: bufnum in: in mul: mul
    ^(Index bufnum: bufnum in: in) * mul!
bufnum: bufnum in: in mul: mul add: add
    ^(Index bufnum: bufnum in: in) * mul + add!
!

PrimitiveUgen subclass: #IndexInBetween
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

IndexInBetween comment: 'Finds the (lowest) point in the Buffer at which the input signal lies in-between the two values'!

!IndexInBetween class methodsFor: 'instance creation'!
bufnum: bufnum in: in
    ^UgenBuilder name: 'IndexInBetween' numChan: 1 rateSpec: #(2) specialIndex: 0 inputArray: {bufnum. in}!
primaryFactoryMethod
    ^#bufnum:in:!
bufnum: bufnum in: in mul: mul
    ^(IndexInBetween bufnum: bufnum in: in) * mul!
bufnum: bufnum in: in mul: mul add: add
    ^(IndexInBetween bufnum: bufnum in: in) * mul + add!
!

PrimitiveUgen subclass: #InFeedback
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

InFeedback comment: 'Read signal from a bus with a current or one cycle old timestamp.'!

!InFeedback class methodsFor: 'instance creation'!
numChan: numChan bus: bus
    ^UgenBuilder name: 'InFeedback' numChan: numChan rateSpec: (Rate ar) specialIndex: 0 inputArray: {bus}!
primaryFactoryMethod
    ^#numChan:bus:!
numChan: numChan bus: bus mul: mul
    ^(InFeedback numChan: numChan bus: bus) * mul!
numChan: numChan bus: bus mul: mul add: add
    ^(InFeedback numChan: numChan bus: bus) * mul + add!
!

PrimitiveUgen subclass: #InRange
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

InRange comment: 'Tests if a signal is within a given range.'!

!InRange class methodsFor: 'instance creation'!
in: in lo: lo hi: hi
    ^UgenBuilder name: 'InRange' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. lo. hi}!
primaryFactoryMethod
    ^#in:lo:hi:!
in: in lo: lo hi: hi mul: mul
    ^(InRange in: in lo: lo hi: hi) * mul!
in: in lo: lo hi: hi mul: mul add: add
    ^(InRange in: in lo: lo hi: hi) * mul + add!
!

PrimitiveUgen subclass: #IRand
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

IRand comment: 'Single integer random number generator.'!

!IRand class methodsFor: 'instance creation'!
lo: lo hi: hi
    ^UgenBuilder name: 'IRand' numChan: 1 rateSpec: (Rate ir) specialIndex: 0 inputArray: {lo. hi}!
primaryFactoryMethod
    ^#lo:hi:!
lo: lo hi: hi mul: mul
    ^(IRand lo: lo hi: hi) * mul!
lo: lo hi: hi mul: mul add: add
    ^(IRand lo: lo hi: hi) * mul + add!
!

PrimitiveUgen subclass: #Integrator
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Integrator comment: 'A leaky integrator.'!

!Integrator class methodsFor: 'instance creation'!
in: in coef: coef
    ^UgenBuilder name: 'Integrator' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. coef}!
primaryFactoryMethod
    ^#in:coef:!
in: in coef: coef mul: mul
    ^(Integrator in: in coef: coef) * mul!
in: in coef: coef mul: mul add: add
    ^(Integrator in: in coef: coef) * mul + add!
!

PrimitiveUgen subclass: #K2A
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

K2A comment: 'Control to audio rate converter.'!

!K2A class methodsFor: 'instance creation'!
in: in
    ^UgenBuilder name: 'K2A' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {in}!
primaryFactoryMethod
    ^#in:!
in: in mul: mul
    ^(K2A in: in) * mul!
in: in mul: mul add: add
    ^(K2A in: in) * mul + add!
!

PrimitiveUgen subclass: #KeyState
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

KeyState comment: 'Respond to the state of a key'!

!KeyState class methodsFor: 'instance creation'!
keycode: keycode minval: minval maxval: maxval lag: lag
    ^UgenBuilder name: 'KeyState' numChan: 1 rateSpec: (Rate kr) specialIndex: 0 inputArray: {keycode. minval. maxval. lag}!
primaryFactoryMethod
    ^#keycode:minval:maxval:lag:!
keycode: keycode minval: minval maxval: maxval lag: lag mul: mul
    ^(KeyState keycode: keycode minval: minval maxval: maxval lag: lag) * mul!
keycode: keycode minval: minval maxval: maxval lag: lag mul: mul add: add
    ^(KeyState keycode: keycode minval: minval maxval: maxval lag: lag) * mul + add!
!

PrimitiveUgen subclass: #Klang
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Klang comment: 'Sine oscillator bank'!

!Klang class methodsFor: 'instance creation'!
freqscale: freqscale freqoffset: freqoffset specificationsArrayRef: specificationsArrayRef
    ^UgenBuilder name: 'Klang' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: ({freqscale. freqoffset} , (specificationsArrayRef asArray))!
primaryFactoryMethod
    ^#freqscale:freqoffset:specificationsArrayRef:!
freqscale: freqscale freqoffset: freqoffset specificationsArrayRef: specificationsArrayRef mul: mul
    ^(Klang freqscale: freqscale freqoffset: freqoffset specificationsArrayRef: specificationsArrayRef) * mul!
freqscale: freqscale freqoffset: freqoffset specificationsArrayRef: specificationsArrayRef mul: mul add: add
    ^(Klang freqscale: freqscale freqoffset: freqoffset specificationsArrayRef: specificationsArrayRef) * mul + add!
!

PrimitiveUgen subclass: #Klank
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Klank comment: 'Bank of resonators'!

!Klank class methodsFor: 'instance creation'!
input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef
    ^UgenBuilder name: 'Klank' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: ({input. freqscale. freqoffset. decayscale} , (specificationsArrayRef asArray))!
primaryFactoryMethod
    ^#input:freqscale:freqoffset:decayscale:specificationsArrayRef:!
input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef mul: mul
    ^(Klank input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef) * mul!
input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef mul: mul add: add
    ^(Klank input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef) * mul + add!
!

PrimitiveUgen subclass: #LFBrownNoise1
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LFBrownNoise1 comment: 'random walk linear interp'!

!LFBrownNoise1 class methodsFor: 'instance creation'!
freq: freq dev: dev dist: dist
    ^UgenBuilder name: 'LFBrownNoise1' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq. dev. dist}!
primaryFactoryMethod
    ^#freq:dev:dist:!
freq: freq dev: dev dist: dist mul: mul
    ^(LFBrownNoise1 freq: freq dev: dev dist: dist) * mul!
freq: freq dev: dev dist: dist mul: mul add: add
    ^(LFBrownNoise1 freq: freq dev: dev dist: dist) * mul + add!
!

PrimitiveUgen subclass: #LFClipNoise
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LFClipNoise comment: 'Clipped noise'!

!LFClipNoise class methodsFor: 'instance creation'!
freq: freq
    ^UgenBuilder name: 'LFClipNoise' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq}!
primaryFactoryMethod
    ^#freq:!
freq: freq mul: mul
    ^(LFClipNoise freq: freq) * mul!
freq: freq mul: mul add: add
    ^(LFClipNoise freq: freq) * mul + add!
!

PrimitiveUgen subclass: #LFCub
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LFCub comment: 'A sine like shape made of two cubic pieces'!

!LFCub class methodsFor: 'instance creation'!
freq: freq iphase: iphase
    ^UgenBuilder name: 'LFCub' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq. iphase}!
primaryFactoryMethod
    ^#freq:iphase:!
freq: freq iphase: iphase mul: mul
    ^(LFCub freq: freq iphase: iphase) * mul!
freq: freq iphase: iphase mul: mul add: add
    ^(LFCub freq: freq iphase: iphase) * mul + add!
!

PrimitiveUgen subclass: #LFDNoise1
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LFDNoise1 comment: 'Dynamic ramp noise'!

!LFDNoise1 class methodsFor: 'instance creation'!
freq: freq
    ^UgenBuilder name: 'LFDNoise1' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq}!
primaryFactoryMethod
    ^#freq:!
freq: freq mul: mul
    ^(LFDNoise1 freq: freq) * mul!
freq: freq mul: mul add: add
    ^(LFDNoise1 freq: freq) * mul + add!
!

PrimitiveUgen subclass: #LFDNoise3
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LFDNoise3 comment: 'Dynamic cubic noise'!

!LFDNoise3 class methodsFor: 'instance creation'!
freq: freq
    ^UgenBuilder name: 'LFDNoise3' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq}!
primaryFactoryMethod
    ^#freq:!
freq: freq mul: mul
    ^(LFDNoise3 freq: freq) * mul!
freq: freq mul: mul add: add
    ^(LFDNoise3 freq: freq) * mul + add!
!

PrimitiveUgen subclass: #LFGauss
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LFGauss comment: 'Gaussian function oscillator'!

!LFGauss class methodsFor: 'instance creation'!
duration: duration width: width iphase: iphase loop: loop doneAction: doneAction
    ^UgenBuilder name: 'LFGauss' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {duration. width. iphase. loop. doneAction}!
primaryFactoryMethod
    ^#duration:width:iphase:loop:doneAction:!
duration: duration width: width iphase: iphase loop: loop doneAction: doneAction mul: mul
    ^(LFGauss duration: duration width: width iphase: iphase loop: loop doneAction: doneAction) * mul!
duration: duration width: width iphase: iphase loop: loop doneAction: doneAction mul: mul add: add
    ^(LFGauss duration: duration width: width iphase: iphase loop: loop doneAction: doneAction) * mul + add!
!

PrimitiveUgen subclass: #LFNoise0
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LFNoise0 comment: 'Step noise'!

!LFNoise0 class methodsFor: 'instance creation'!
freq: freq
    ^UgenBuilder name: 'LFNoise0' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq}!
primaryFactoryMethod
    ^#freq:!
freq: freq mul: mul
    ^(LFNoise0 freq: freq) * mul!
freq: freq mul: mul add: add
    ^(LFNoise0 freq: freq) * mul + add!
!

PrimitiveUgen subclass: #LFNoise1
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LFNoise1 comment: 'Ramp noise'!

!LFNoise1 class methodsFor: 'instance creation'!
freq: freq
    ^UgenBuilder name: 'LFNoise1' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq}!
primaryFactoryMethod
    ^#freq:!
freq: freq mul: mul
    ^(LFNoise1 freq: freq) * mul!
freq: freq mul: mul add: add
    ^(LFNoise1 freq: freq) * mul + add!
!

PrimitiveUgen subclass: #LFNoise2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LFNoise2 comment: 'Quadratic noise.'!

!LFNoise2 class methodsFor: 'instance creation'!
freq: freq
    ^UgenBuilder name: 'LFNoise2' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq}!
primaryFactoryMethod
    ^#freq:!
freq: freq mul: mul
    ^(LFNoise2 freq: freq) * mul!
freq: freq mul: mul add: add
    ^(LFNoise2 freq: freq) * mul + add!
!

PrimitiveUgen subclass: #LFPar
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LFPar comment: 'Parabolic oscillator'!

!LFPar class methodsFor: 'instance creation'!
freq: freq iphase: iphase
    ^UgenBuilder name: 'LFPar' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq. iphase}!
primaryFactoryMethod
    ^#freq:iphase:!
freq: freq iphase: iphase mul: mul
    ^(LFPar freq: freq iphase: iphase) * mul!
freq: freq iphase: iphase mul: mul add: add
    ^(LFPar freq: freq iphase: iphase) * mul + add!
!

PrimitiveUgen subclass: #LFPulse
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LFPulse comment: 'pulse oscillator'!

!LFPulse class methodsFor: 'instance creation'!
freq: freq iphase: iphase width: width
    ^UgenBuilder name: 'LFPulse' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq. iphase. width}!
primaryFactoryMethod
    ^#freq:iphase:width:!
freq: freq iphase: iphase width: width mul: mul
    ^(LFPulse freq: freq iphase: iphase width: width) * mul!
freq: freq iphase: iphase width: width mul: mul add: add
    ^(LFPulse freq: freq iphase: iphase width: width) * mul + add!
!

PrimitiveUgen subclass: #LFSaw
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LFSaw comment: 'Sawtooth oscillator'!

!LFSaw class methodsFor: 'instance creation'!
freq: freq iphase: iphase
    ^UgenBuilder name: 'LFSaw' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq. iphase}!
primaryFactoryMethod
    ^#freq:iphase:!
freq: freq iphase: iphase mul: mul
    ^(LFSaw freq: freq iphase: iphase) * mul!
freq: freq iphase: iphase mul: mul add: add
    ^(LFSaw freq: freq iphase: iphase) * mul + add!
!

PrimitiveUgen subclass: #LFTri
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LFTri comment: 'Triangle oscillator'!

!LFTri class methodsFor: 'instance creation'!
freq: freq iphase: iphase
    ^UgenBuilder name: 'LFTri' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq. iphase}!
primaryFactoryMethod
    ^#freq:iphase:!
freq: freq iphase: iphase mul: mul
    ^(LFTri freq: freq iphase: iphase) * mul!
freq: freq iphase: iphase mul: mul add: add
    ^(LFTri freq: freq iphase: iphase) * mul + add!
!

PrimitiveUgen subclass: #LPF
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LPF comment: '2nd order Butterworth lowpass filter'!

!LPF class methodsFor: 'instance creation'!
in: in freq: freq
    ^UgenBuilder name: 'LPF' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. freq}!
primaryFactoryMethod
    ^#in:freq:!
in: in freq: freq mul: mul
    ^(LPF in: in freq: freq) * mul!
in: in freq: freq mul: mul add: add
    ^(LPF in: in freq: freq) * mul + add!
!

PrimitiveUgen subclass: #Lag
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Lag comment: 'Exponential lag'!

!Lag class methodsFor: 'instance creation'!
in: in lagTime: lagTime
    ^UgenBuilder name: 'Lag' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. lagTime}!
primaryFactoryMethod
    ^#in:lagTime:!
in: in lagTime: lagTime mul: mul
    ^(Lag in: in lagTime: lagTime) * mul!
in: in lagTime: lagTime mul: mul add: add
    ^(Lag in: in lagTime: lagTime) * mul + add!
!

PrimitiveUgen subclass: #LagUD
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LagUD comment: 'Exponential lag'!

!LagUD class methodsFor: 'instance creation'!
in: in lagTimeU: lagTimeU lagTimeD: lagTimeD
    ^UgenBuilder name: 'LagUD' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. lagTimeU. lagTimeD}!
primaryFactoryMethod
    ^#in:lagTimeU:lagTimeD:!
in: in lagTimeU: lagTimeU lagTimeD: lagTimeD mul: mul
    ^(LagUD in: in lagTimeU: lagTimeU lagTimeD: lagTimeD) * mul!
in: in lagTimeU: lagTimeU lagTimeD: lagTimeD mul: mul add: add
    ^(LagUD in: in lagTimeU: lagTimeU lagTimeD: lagTimeD) * mul + add!
!

PrimitiveUgen subclass: #Lag2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Lag2 comment: 'Exponential lag'!

!Lag2 class methodsFor: 'instance creation'!
in: in lagTime: lagTime
    ^UgenBuilder name: 'Lag2' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. lagTime}!
primaryFactoryMethod
    ^#in:lagTime:!
in: in lagTime: lagTime mul: mul
    ^(Lag2 in: in lagTime: lagTime) * mul!
in: in lagTime: lagTime mul: mul add: add
    ^(Lag2 in: in lagTime: lagTime) * mul + add!
!

PrimitiveUgen subclass: #Lag3
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Lag3 comment: 'Exponential lag'!

!Lag3 class methodsFor: 'instance creation'!
in: in lagTime: lagTime
    ^UgenBuilder name: 'Lag3' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. lagTime}!
primaryFactoryMethod
    ^#in:lagTime:!
in: in lagTime: lagTime mul: mul
    ^(Lag3 in: in lagTime: lagTime) * mul!
in: in lagTime: lagTime mul: mul add: add
    ^(Lag3 in: in lagTime: lagTime) * mul + add!
!

PrimitiveUgen subclass: #Lag3UD
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Lag3UD comment: 'Exponential lag'!

!Lag3UD class methodsFor: 'instance creation'!
in: in lagTimeU: lagTimeU lagTimeD: lagTimeD
    ^UgenBuilder name: 'Lag3UD' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. lagTimeU. lagTimeD}!
primaryFactoryMethod
    ^#in:lagTimeU:lagTimeD:!
in: in lagTimeU: lagTimeU lagTimeD: lagTimeD mul: mul
    ^(Lag3UD in: in lagTimeU: lagTimeU lagTimeD: lagTimeD) * mul!
in: in lagTimeU: lagTimeU lagTimeD: lagTimeD mul: mul add: add
    ^(Lag3UD in: in lagTimeU: lagTimeU lagTimeD: lagTimeD) * mul + add!
!

PrimitiveUgen subclass: #Latch
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Latch comment: 'Sample and hold'!

!Latch class methodsFor: 'instance creation'!
in: in trig: trig
    ^UgenBuilder name: 'Latch' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. trig}!
primaryFactoryMethod
    ^#in:trig:!
in: in trig: trig mul: mul
    ^(Latch in: in trig: trig) * mul!
in: in trig: trig mul: mul add: add
    ^(Latch in: in trig: trig) * mul + add!
!

PrimitiveUgen subclass: #LatoocarfianC
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LatoocarfianC comment: 'Latoocarfian chaotic generator'!

!LatoocarfianC class methodsFor: 'instance creation'!
freq: freq a: a b: b c: c d: d xi: xi yi: yi
    ^UgenBuilder name: 'LatoocarfianC' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq. a. b. c. d. xi. yi}!
primaryFactoryMethod
    ^#freq:a:b:c:d:xi:yi:!
freq: freq a: a b: b c: c d: d xi: xi yi: yi mul: mul
    ^(LatoocarfianC freq: freq a: a b: b c: c d: d xi: xi yi: yi) * mul!
freq: freq a: a b: b c: c d: d xi: xi yi: yi mul: mul add: add
    ^(LatoocarfianC freq: freq a: a b: b c: c d: d xi: xi yi: yi) * mul + add!
!

PrimitiveUgen subclass: #LeakDC
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LeakDC comment: 'Remove DC'!

!LeakDC class methodsFor: 'instance creation'!
in: in coef: coef
    ^UgenBuilder name: 'LeakDC' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. coef}!
primaryFactoryMethod
    ^#in:coef:!
in: in coef: coef mul: mul
    ^(LeakDC in: in coef: coef) * mul!
in: in coef: coef mul: mul add: add
    ^(LeakDC in: in coef: coef) * mul + add!
!

PrimitiveUgen subclass: #Limiter
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Limiter comment: 'Peak limiter'!

!Limiter class methodsFor: 'instance creation'!
in: in level: level dur: dur
    ^UgenBuilder name: 'Limiter' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. level. dur}!
primaryFactoryMethod
    ^#in:level:dur:!
in: in level: level dur: dur mul: mul
    ^(Limiter in: in level: level dur: dur) * mul!
in: in level: level dur: dur mul: mul add: add
    ^(Limiter in: in level: level dur: dur) * mul + add!
!

PrimitiveUgen subclass: #LinCongC
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LinCongC comment: 'Linear congruential chaotic generator'!

!LinCongC class methodsFor: 'instance creation'!
freq: freq a: a c: c m: m xi: xi
    ^UgenBuilder name: 'LinCongC' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq. a. c. m. xi}!
primaryFactoryMethod
    ^#freq:a:c:m:xi:!
freq: freq a: a c: c m: m xi: xi mul: mul
    ^(LinCongC freq: freq a: a c: c m: m xi: xi) * mul!
freq: freq a: a c: c m: m xi: xi mul: mul add: add
    ^(LinCongC freq: freq a: a c: c m: m xi: xi) * mul + add!
!

PrimitiveUgen subclass: #Line
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Line comment: 'Line generator.'!

!Line class methodsFor: 'instance creation'!
start: start end: end dur: dur doneAction: doneAction
    ^UgenBuilder name: 'Line' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {start. end. dur. doneAction}!
primaryFactoryMethod
    ^#start:end:dur:doneAction:!
start: start end: end dur: dur doneAction: doneAction mul: mul
    ^(Line start: start end: end dur: dur doneAction: doneAction) * mul!
start: start end: end dur: dur doneAction: doneAction mul: mul add: add
    ^(Line start: start end: end dur: dur doneAction: doneAction) * mul + add!
!

PrimitiveUgen subclass: #Linen
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Linen comment: 'Simple linear envelope generator.'!

!Linen class methodsFor: 'instance creation'!
gate: gate attackTime: attackTime susLevel: susLevel releaseTime: releaseTime doneAction: doneAction
    ^UgenBuilder name: 'Linen' numChan: 1 rateSpec: (Rate kr) specialIndex: 0 inputArray: {gate. attackTime. susLevel. releaseTime. doneAction}!
primaryFactoryMethod
    ^#gate:attackTime:susLevel:releaseTime:doneAction:!
gate: gate attackTime: attackTime susLevel: susLevel releaseTime: releaseTime doneAction: doneAction mul: mul
    ^(Linen gate: gate attackTime: attackTime susLevel: susLevel releaseTime: releaseTime doneAction: doneAction) * mul!
gate: gate attackTime: attackTime susLevel: susLevel releaseTime: releaseTime doneAction: doneAction mul: mul add: add
    ^(Linen gate: gate attackTime: attackTime susLevel: susLevel releaseTime: releaseTime doneAction: doneAction) * mul + add!
!

PrimitiveUgen subclass: #LinExp
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LinExp comment: 'Map a linear range to an exponential range'!

!LinExp class methodsFor: 'instance creation'!
in: in srclo: srclo srchi: srchi dstlo: dstlo dsthi: dsthi
    ^UgenBuilder name: 'LinExp' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. srclo. srchi. dstlo. dsthi}!
primaryFactoryMethod
    ^#in:srclo:srchi:dstlo:dsthi:!
in: in srclo: srclo srchi: srchi dstlo: dstlo dsthi: dsthi mul: mul
    ^(LinExp in: in srclo: srclo srchi: srchi dstlo: dstlo dsthi: dsthi) * mul!
in: in srclo: srclo srchi: srchi dstlo: dstlo dsthi: dsthi mul: mul add: add
    ^(LinExp in: in srclo: srclo srchi: srchi dstlo: dstlo dsthi: dsthi) * mul + add!
!

PrimitiveUgen subclass: #LinPan2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LinPan2 comment: 'Two channel linear pan.'!

!LinPan2 class methodsFor: 'instance creation'!
in: in pos: pos level: level
    ^UgenBuilder name: 'LinPan2' numChan: 2 rateSpec: #(1) specialIndex: 0 inputArray: {in. pos. level}!
primaryFactoryMethod
    ^#in:pos:level:!
in: in pos: pos level: level mul: mul
    ^(LinPan2 in: in pos: pos level: level) * mul!
in: in pos: pos level: level mul: mul add: add
    ^(LinPan2 in: in pos: pos level: level) * mul + add!
!

PrimitiveUgen subclass: #LinRand
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LinRand comment: 'Skewed random number generator.'!

!LinRand class methodsFor: 'instance creation'!
lo: lo hi: hi minmax: minmax
    ^UgenBuilder name: 'LinRand' numChan: 1 rateSpec: (Rate ir) specialIndex: 0 inputArray: {lo. hi. minmax}!
primaryFactoryMethod
    ^#lo:hi:minmax:!
lo: lo hi: hi minmax: minmax mul: mul
    ^(LinRand lo: lo hi: hi minmax: minmax) * mul!
lo: lo hi: hi minmax: minmax mul: mul add: add
    ^(LinRand lo: lo hi: hi minmax: minmax) * mul + add!
!

PrimitiveUgen subclass: #LinXFade2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LinXFade2 comment: 'Two channel linear crossfade.'!

!LinXFade2 class methodsFor: 'instance creation'!
inA: inA inB: inB pan: pan
    ^UgenBuilder name: 'LinXFade2' numChan: 1 rateSpec: #(1 2) specialIndex: 0 inputArray: {inA. inB. pan}!
primaryFactoryMethod
    ^#inA:inB:pan:!
inA: inA inB: inB pan: pan mul: mul
    ^(LinXFade2 inA: inA inB: inB pan: pan) * mul!
inA: inA inB: inB pan: pan mul: mul add: add
    ^(LinXFade2 inA: inA inB: inB pan: pan) * mul + add!
!

PrimitiveUgen subclass: #LocalBuf
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LocalBuf comment: 'Allocate a buffer local to the synth'!

!LocalBuf class methodsFor: 'instance creation'!
numChannels: numChannels numFrames: numFrames
    ^UgenBuilder name: 'LocalBuf' numChan: 1 rateSpec: (Rate ir) specialIndex: 0 inputArray: {numChannels. numFrames}!
primaryFactoryMethod
    ^#numChannels:numFrames:!
numChannels: numChannels numFrames: numFrames mul: mul
    ^(LocalBuf numChannels: numChannels numFrames: numFrames) * mul!
numChannels: numChannels numFrames: numFrames mul: mul add: add
    ^(LocalBuf numChannels: numChannels numFrames: numFrames) * mul + add!
!

PrimitiveUgen subclass: #LocalIn
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LocalIn comment: 'Define and read from buses local to a synth.'!

!LocalIn class methodsFor: 'instance creation'!
numChan: numChan default: default
    ^UgenBuilder name: 'LocalIn' numChan: numChan rateSpec: (Rate ar) specialIndex: 0 inputArray: ({} , (default asArray))!
primaryFactoryMethod
    ^#numChan:default:!
numChan: numChan default: default mul: mul
    ^(LocalIn numChan: numChan default: default) * mul!
numChan: numChan default: default mul: mul add: add
    ^(LocalIn numChan: numChan default: default) * mul + add!
!

PrimitiveUgen subclass: #LocalOut
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LocalOut comment: 'Write to buses local to a synth.'!

!LocalOut class methodsFor: 'instance creation'!
channelsArray: channelsArray
    ^UgenBuilder name: 'LocalOut' numChan: 0 rateSpec: #(1) specialIndex: 0 inputArray: ({} , (channelsArray asArray))!
primaryFactoryMethod
    ^#channelsArray:!
channelsArray: channelsArray mul: mul
    ^(LocalOut channelsArray: channelsArray) * mul!
channelsArray: channelsArray mul: mul add: add
    ^(LocalOut channelsArray: channelsArray) * mul + add!
!

PrimitiveUgen subclass: #LorenzL
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LorenzL comment: 'Lorenz chaotic generator'!

!LorenzL class methodsFor: 'instance creation'!
freq: freq s: s r: r b: b h: h xi: xi yi: yi zi: zi
    ^UgenBuilder name: 'LorenzL' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq. s. r. b. h. xi. yi. zi}!
primaryFactoryMethod
    ^#freq:s:r:b:h:xi:yi:zi:!
freq: freq s: s r: r b: b h: h xi: xi yi: yi zi: zi mul: mul
    ^(LorenzL freq: freq s: s r: r b: b h: h xi: xi yi: yi zi: zi) * mul!
freq: freq s: s r: r b: b h: h xi: xi yi: yi zi: zi mul: mul add: add
    ^(LorenzL freq: freq s: s r: r b: b h: h xi: xi yi: yi zi: zi) * mul + add!
!

PrimitiveUgen subclass: #LPZ1
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LPZ1 comment: 'Two point average filter'!

!LPZ1 class methodsFor: 'instance creation'!
in: in
    ^UgenBuilder name: 'LPZ1' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in}!
primaryFactoryMethod
    ^#in:!
in: in mul: mul
    ^(LPZ1 in: in) * mul!
in: in mul: mul add: add
    ^(LPZ1 in: in) * mul + add!
!

PrimitiveUgen subclass: #LPZ2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LPZ2 comment: 'Two zero fixed lowpass'!

!LPZ2 class methodsFor: 'instance creation'!
in: in
    ^UgenBuilder name: 'LPZ2' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in}!
primaryFactoryMethod
    ^#in:!
in: in mul: mul
    ^(LPZ2 in: in) * mul!
in: in mul: mul add: add
    ^(LPZ2 in: in) * mul + add!
!

PrimitiveUgen subclass: #MantissaMask
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

MantissaMask comment: 'Reduce precision.'!

!MantissaMask class methodsFor: 'instance creation'!
in: in bits: bits
    ^UgenBuilder name: 'MantissaMask' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. bits}!
primaryFactoryMethod
    ^#in:bits:!
in: in bits: bits mul: mul
    ^(MantissaMask in: in bits: bits) * mul!
in: in bits: bits mul: mul add: add
    ^(MantissaMask in: in bits: bits) * mul + add!
!

PrimitiveUgen subclass: #MaxLocalBufs
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

MaxLocalBufs comment: 'LocalBuf count'!

!MaxLocalBufs class methodsFor: 'instance creation'!
count: count
    ^UgenBuilder name: 'MaxLocalBufs' numChan: 1 rateSpec: (Rate ir) specialIndex: 0 inputArray: {count}!
primaryFactoryMethod
    ^#count:!
count: count mul: mul
    ^(MaxLocalBufs count: count) * mul!
count: count mul: mul add: add
    ^(MaxLocalBufs count: count) * mul + add!
!

PrimitiveUgen subclass: #Median
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Median comment: 'Median filter.'!

!Median class methodsFor: 'instance creation'!
length: length in: in
    ^UgenBuilder name: 'Median' numChan: 1 rateSpec: #(2) specialIndex: 0 inputArray: {length. in}!
primaryFactoryMethod
    ^#length:in:!
length: length in: in mul: mul
    ^(Median length: length in: in) * mul!
length: length in: in mul: mul add: add
    ^(Median length: length in: in) * mul + add!
!

PrimitiveUgen subclass: #ModDif
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

ModDif comment: 'Minimum difference of two values in modulo arithmetics'!

!ModDif class methodsFor: 'instance creation'!
x: x y: y mod: mod
    ^UgenBuilder name: 'ModDif' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {x. y. mod}!
primaryFactoryMethod
    ^#x:y:mod:!
x: x y: y mod: mod mul: mul
    ^(ModDif x: x y: y mod: mod) * mul!
x: x y: y mod: mod mul: mul add: add
    ^(ModDif x: x y: y mod: mod) * mul + add!
!

PrimitiveUgen subclass: #MoogFF
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

MoogFF comment: 'Moog VCF implementation, designed by Federico Fontana'!

!MoogFF class methodsFor: 'instance creation'!
in: in freq: freq gain: gain reset: reset
    ^UgenBuilder name: 'MoogFF' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. freq. gain. reset}!
primaryFactoryMethod
    ^#in:freq:gain:reset:!
in: in freq: freq gain: gain reset: reset mul: mul
    ^(MoogFF in: in freq: freq gain: gain reset: reset) * mul!
in: in freq: freq gain: gain reset: reset mul: mul add: add
    ^(MoogFF in: in freq: freq gain: gain reset: reset) * mul + add!
!

PrimitiveUgen subclass: #MouseButton
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

MouseButton comment: 'Mouse button UGen.'!

!MouseButton class methodsFor: 'instance creation'!
minval: minval maxval: maxval lag: lag
    ^UgenBuilder name: 'MouseButton' numChan: 1 rateSpec: (Rate kr) specialIndex: 0 inputArray: {minval. maxval. lag}!
primaryFactoryMethod
    ^#minval:maxval:lag:!
minval: minval maxval: maxval lag: lag mul: mul
    ^(MouseButton minval: minval maxval: maxval lag: lag) * mul!
minval: minval maxval: maxval lag: lag mul: mul add: add
    ^(MouseButton minval: minval maxval: maxval lag: lag) * mul + add!
!

PrimitiveUgen subclass: #MouseX
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

MouseX comment: 'Cursor tracking UGen.'!

!MouseX class methodsFor: 'instance creation'!
minval: minval maxval: maxval warp: warp lag: lag
    ^UgenBuilder name: 'MouseX' numChan: 1 rateSpec: (Rate kr) specialIndex: 0 inputArray: {minval. maxval. warp. lag}!
primaryFactoryMethod
    ^#minval:maxval:warp:lag:!
minval: minval maxval: maxval warp: warp lag: lag mul: mul
    ^(MouseX minval: minval maxval: maxval warp: warp lag: lag) * mul!
minval: minval maxval: maxval warp: warp lag: lag mul: mul add: add
    ^(MouseX minval: minval maxval: maxval warp: warp lag: lag) * mul + add!
!

PrimitiveUgen subclass: #MouseY
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

MouseY comment: 'Cursor tracking UGen.'!

!MouseY class methodsFor: 'instance creation'!
minval: minval maxval: maxval warp: warp lag: lag
    ^UgenBuilder name: 'MouseY' numChan: 1 rateSpec: (Rate kr) specialIndex: 0 inputArray: {minval. maxval. warp. lag}!
primaryFactoryMethod
    ^#minval:maxval:warp:lag:!
minval: minval maxval: maxval warp: warp lag: lag mul: mul
    ^(MouseY minval: minval maxval: maxval warp: warp lag: lag) * mul!
minval: minval maxval: maxval warp: warp lag: lag mul: mul add: add
    ^(MouseY minval: minval maxval: maxval warp: warp lag: lag) * mul + add!
!

PrimitiveUgen subclass: #MulAdd
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

MulAdd comment: 'Multiply add'!

!MulAdd class methodsFor: 'instance creation'!
in: in mul: mul add: add
    ^UgenBuilder name: 'MulAdd' numChan: 1 rateSpec: #(1 2 3) specialIndex: 0 inputArray: {in. mul. add}!
primaryFactoryMethod
    ^#in:mul:add:!
!

PrimitiveUgen subclass: #Normalizer
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Normalizer comment: 'Flattens dynamics.'!

!Normalizer class methodsFor: 'instance creation'!
in: in level: level dur: dur
    ^UgenBuilder name: 'Normalizer' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. level. dur}!
primaryFactoryMethod
    ^#in:level:dur:!
in: in level: level dur: dur mul: mul
    ^(Normalizer in: in level: level dur: dur) * mul!
in: in level: level dur: dur mul: mul add: add
    ^(Normalizer in: in level: level dur: dur) * mul + add!
!

PrimitiveUgen subclass: #NRand
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

NRand comment: 'Sum of uniform distributions.'!

!NRand class methodsFor: 'instance creation'!
lo: lo hi: hi n: n
    ^UgenBuilder name: 'NRand' numChan: 1 rateSpec: (Rate ir) specialIndex: 0 inputArray: {lo. hi. n}!
primaryFactoryMethod
    ^#lo:hi:n:!
lo: lo hi: hi n: n mul: mul
    ^(NRand lo: lo hi: hi n: n) * mul!
lo: lo hi: hi n: n mul: mul add: add
    ^(NRand lo: lo hi: hi n: n) * mul + add!
!

PrimitiveUgen subclass: #NumOutputBuses
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

NumOutputBuses comment: 'Number of output busses.'!

!NumOutputBuses class methodsFor: 'instance creation'!
new
    ^UgenBuilder name: 'NumOutputBuses' numChan: 1 rateSpec: (Rate ir) specialIndex: 0 inputArray: {}!
primaryFactoryMethod
    ^#new!
mul: mul
    ^(NumOutputBuses new) * mul!
mul: mul add: add
    ^(NumOutputBuses new) * mul + add!
!

PrimitiveUgen subclass: #OnePole
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

OnePole comment: 'One pole filter.'!

!OnePole class methodsFor: 'instance creation'!
in: in coef: coef
    ^UgenBuilder name: 'OnePole' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. coef}!
primaryFactoryMethod
    ^#in:coef:!
in: in coef: coef mul: mul
    ^(OnePole in: in coef: coef) * mul!
in: in coef: coef mul: mul add: add
    ^(OnePole in: in coef: coef) * mul + add!
!

PrimitiveUgen subclass: #OneZero
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

OneZero comment: 'One zero filter.'!

!OneZero class methodsFor: 'instance creation'!
in: in coef: coef
    ^UgenBuilder name: 'OneZero' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. coef}!
primaryFactoryMethod
    ^#in:coef:!
in: in coef: coef mul: mul
    ^(OneZero in: in coef: coef) * mul!
in: in coef: coef mul: mul add: add
    ^(OneZero in: in coef: coef) * mul + add!
!

PrimitiveUgen subclass: #Osc
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Osc comment: 'Interpolating wavetable oscillator.'!

!Osc class methodsFor: 'instance creation'!
bufnum: bufnum freq: freq phase: phase
    ^UgenBuilder name: 'Osc' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {bufnum. freq. phase}!
primaryFactoryMethod
    ^#bufnum:freq:phase:!
bufnum: bufnum freq: freq phase: phase mul: mul
    ^(Osc bufnum: bufnum freq: freq phase: phase) * mul!
bufnum: bufnum freq: freq phase: phase mul: mul add: add
    ^(Osc bufnum: bufnum freq: freq phase: phase) * mul + add!
!

PrimitiveUgen subclass: #Out
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Out comment: 'Write a signal to a bus.'!

!Out class methodsFor: 'instance creation'!
bus: bus channelsArray: channelsArray
    ^UgenBuilder name: 'Out' numChan: 0 rateSpec: #(2) specialIndex: 0 inputArray: ({bus} , (channelsArray asArray))!
primaryFactoryMethod
    ^#bus:channelsArray:!
bus: bus channelsArray: channelsArray mul: mul
    ^(Out bus: bus channelsArray: channelsArray) * mul!
bus: bus channelsArray: channelsArray mul: mul add: add
    ^(Out bus: bus channelsArray: channelsArray) * mul + add!
!

PrimitiveUgen subclass: #Pan2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Pan2 comment: 'Two channel equal power pan.'!

!Pan2 class methodsFor: 'instance creation'!
in: in pos: pos level: level
    ^UgenBuilder name: 'Pan2' numChan: 2 rateSpec: #(1) specialIndex: 0 inputArray: {in. pos. level}!
primaryFactoryMethod
    ^#in:pos:level:!
in: in pos: pos level: level mul: mul
    ^(Pan2 in: in pos: pos level: level) * mul!
in: in pos: pos level: level mul: mul add: add
    ^(Pan2 in: in pos: pos level: level) * mul + add!
!

PrimitiveUgen subclass: #PanAz
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PanAz comment: 'Azimuth panner'!

!PanAz class methodsFor: 'instance creation'!
numChan: numChan in: in pos: pos level: level width: width orientation: orientation
    ^UgenBuilder name: 'PanAz' numChan: numChan rateSpec: #(1) specialIndex: 0 inputArray: {in. pos. level. width. orientation}!
primaryFactoryMethod
    ^#numChan:in:pos:level:width:orientation:!
numChan: numChan in: in pos: pos level: level width: width orientation: orientation mul: mul
    ^(PanAz numChan: numChan in: in pos: pos level: level width: width orientation: orientation) * mul!
numChan: numChan in: in pos: pos level: level width: width orientation: orientation mul: mul add: add
    ^(PanAz numChan: numChan in: in pos: pos level: level width: width orientation: orientation) * mul + add!
!

PrimitiveUgen subclass: #PanB
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PanB comment: 'Ambisonic B-format panner.'!

!PanB class methodsFor: 'instance creation'!
in: in azimuth: azimuth elevation: elevation gain: gain
    ^UgenBuilder name: 'PanB' numChan: 4 rateSpec: (Rate ar) specialIndex: 0 inputArray: {in. azimuth. elevation. gain}!
primaryFactoryMethod
    ^#in:azimuth:elevation:gain:!
in: in azimuth: azimuth elevation: elevation gain: gain mul: mul
    ^(PanB in: in azimuth: azimuth elevation: elevation gain: gain) * mul!
in: in azimuth: azimuth elevation: elevation gain: gain mul: mul add: add
    ^(PanB in: in azimuth: azimuth elevation: elevation gain: gain) * mul + add!
!

PrimitiveUgen subclass: #PeakFollower
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PeakFollower comment: 'Track peak signal amplitude.'!

!PeakFollower class methodsFor: 'instance creation'!
in: in decay: decay
    ^UgenBuilder name: 'PeakFollower' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. decay}!
primaryFactoryMethod
    ^#in:decay:!
in: in decay: decay mul: mul
    ^(PeakFollower in: in decay: decay) * mul!
in: in decay: decay mul: mul add: add
    ^(PeakFollower in: in decay: decay) * mul + add!
!

PrimitiveUgen subclass: #Perlin3
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Perlin3 comment: '3D Perlin Noise'!

!Perlin3 class methodsFor: 'instance creation'!
x: x y: y z: z
    ^UgenBuilder name: 'Perlin3' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {x. y. z}!
primaryFactoryMethod
    ^#x:y:z:!
x: x y: y z: z mul: mul
    ^(Perlin3 x: x y: y z: z) * mul!
x: x y: y z: z mul: mul add: add
    ^(Perlin3 x: x y: y z: z) * mul + add!
!

PrimitiveUgen subclass: #Phasor
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Phasor comment: 'A resettable linear ramp between two levels.'!

!Phasor class methodsFor: 'instance creation'!
trig: trig rate: rate start: start end: end resetPos: resetPos
    ^UgenBuilder name: 'Phasor' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {trig. rate. start. end. resetPos}!
primaryFactoryMethod
    ^#trig:rate:start:end:resetPos:!
trig: trig rate: rate start: start end: end resetPos: resetPos mul: mul
    ^(Phasor trig: trig rate: rate start: start end: end resetPos: resetPos) * mul!
trig: trig rate: rate start: start end: end resetPos: resetPos mul: mul add: add
    ^(Phasor trig: trig rate: rate start: start end: end resetPos: resetPos) * mul + add!
!

PrimitiveUgen subclass: #PinkNoise
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PinkNoise comment: 'Pink Noise.'!

!PinkNoise class methodsFor: 'instance creation'!
new
    ^UgenBuilder name: 'PinkNoise' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {}!
primaryFactoryMethod
    ^#new!
mul: mul
    ^(PinkNoise new) * mul!
mul: mul add: add
    ^(PinkNoise new) * mul + add!
!

PrimitiveUgen subclass: #Pitch
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Pitch comment: 'Autocorrelation pitch follower'!

!Pitch class methodsFor: 'instance creation'!
in: in initFreq: initFreq minFreq: minFreq maxFreq: maxFreq execFreq: execFreq maxBinsPerOctave: maxBinsPerOctave median: median ampThreshold: ampThreshold peakThreshold: peakThreshold downSample: downSample clar: clar
    ^UgenBuilder name: 'Pitch' numChan: 2 rateSpec: (Rate kr) specialIndex: 0 inputArray: {in. initFreq. minFreq. maxFreq. execFreq. maxBinsPerOctave. median. ampThreshold. peakThreshold. downSample. clar}!
primaryFactoryMethod
    ^#in:initFreq:minFreq:maxFreq:execFreq:maxBinsPerOctave:median:ampThreshold:peakThreshold:downSample:clar:!
in: in initFreq: initFreq minFreq: minFreq maxFreq: maxFreq execFreq: execFreq maxBinsPerOctave: maxBinsPerOctave median: median ampThreshold: ampThreshold peakThreshold: peakThreshold downSample: downSample clar: clar mul: mul
    ^(Pitch in: in initFreq: initFreq minFreq: minFreq maxFreq: maxFreq execFreq: execFreq maxBinsPerOctave: maxBinsPerOctave median: median ampThreshold: ampThreshold peakThreshold: peakThreshold downSample: downSample clar: clar) * mul!
in: in initFreq: initFreq minFreq: minFreq maxFreq: maxFreq execFreq: execFreq maxBinsPerOctave: maxBinsPerOctave median: median ampThreshold: ampThreshold peakThreshold: peakThreshold downSample: downSample clar: clar mul: mul add: add
    ^(Pitch in: in initFreq: initFreq minFreq: minFreq maxFreq: maxFreq execFreq: execFreq maxBinsPerOctave: maxBinsPerOctave median: median ampThreshold: ampThreshold peakThreshold: peakThreshold downSample: downSample clar: clar) * mul + add!
!

PrimitiveUgen subclass: #PitchShift
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PitchShift comment: 'Time domain pitch shifter.'!

!PitchShift class methodsFor: 'instance creation'!
in: in windowSize: windowSize pitchRatio: pitchRatio pitchDispersion: pitchDispersion timeDispersion: timeDispersion
    ^UgenBuilder name: 'PitchShift' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. windowSize. pitchRatio. pitchDispersion. timeDispersion}!
primaryFactoryMethod
    ^#in:windowSize:pitchRatio:pitchDispersion:timeDispersion:!
in: in windowSize: windowSize pitchRatio: pitchRatio pitchDispersion: pitchDispersion timeDispersion: timeDispersion mul: mul
    ^(PitchShift in: in windowSize: windowSize pitchRatio: pitchRatio pitchDispersion: pitchDispersion timeDispersion: timeDispersion) * mul!
in: in windowSize: windowSize pitchRatio: pitchRatio pitchDispersion: pitchDispersion timeDispersion: timeDispersion mul: mul add: add
    ^(PitchShift in: in windowSize: windowSize pitchRatio: pitchRatio pitchDispersion: pitchDispersion timeDispersion: timeDispersion) * mul + add!
!

PrimitiveUgen subclass: #PlayBuf
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PlayBuf comment: 'Sample playback oscillator.'!

!PlayBuf class methodsFor: 'instance creation'!
numChan: numChan bufnum: bufnum rate: rate trigger: trigger startPos: startPos loop: loop doneAction: doneAction
    ^UgenBuilder name: 'PlayBuf' numChan: numChan rateSpec: (Rate ar) specialIndex: 0 inputArray: {bufnum. rate. trigger. startPos. loop. doneAction}!
primaryFactoryMethod
    ^#numChan:bufnum:rate:trigger:startPos:loop:doneAction:!
numChan: numChan bufnum: bufnum rate: rate trigger: trigger startPos: startPos loop: loop doneAction: doneAction mul: mul
    ^(PlayBuf numChan: numChan bufnum: bufnum rate: rate trigger: trigger startPos: startPos loop: loop doneAction: doneAction) * mul!
numChan: numChan bufnum: bufnum rate: rate trigger: trigger startPos: startPos loop: loop doneAction: doneAction mul: mul add: add
    ^(PlayBuf numChan: numChan bufnum: bufnum rate: rate trigger: trigger startPos: startPos loop: loop doneAction: doneAction) * mul + add!
!

PrimitiveUgen subclass: #Pluck
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Pluck comment: 'A Karplus-Strong UGen'!

!Pluck class methodsFor: 'instance creation'!
in: in trig: trig maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime coef: coef
    ^UgenBuilder name: 'Pluck' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. trig. maxdelaytime. delaytime. decaytime. coef}!
primaryFactoryMethod
    ^#in:trig:maxdelaytime:delaytime:decaytime:coef:!
in: in trig: trig maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime coef: coef mul: mul
    ^(Pluck in: in trig: trig maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime coef: coef) * mul!
in: in trig: trig maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime coef: coef mul: mul add: add
    ^(Pluck in: in trig: trig maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime coef: coef) * mul + add!
!

PrimitiveUgen subclass: #Pulse
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Pulse comment: 'Band limited pulse wave.'!

!Pulse class methodsFor: 'instance creation'!
freq: freq width: width
    ^UgenBuilder name: 'Pulse' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq. width}!
primaryFactoryMethod
    ^#freq:width:!
freq: freq width: width mul: mul
    ^(Pulse freq: freq width: width) * mul!
freq: freq width: width mul: mul add: add
    ^(Pulse freq: freq width: width) * mul + add!
!

PrimitiveUgen subclass: #PulseCount
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PulseCount comment: 'Pulse counter.'!

!PulseCount class methodsFor: 'instance creation'!
trig: trig reset: reset
    ^UgenBuilder name: 'PulseCount' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {trig. reset}!
primaryFactoryMethod
    ^#trig:reset:!
trig: trig reset: reset mul: mul
    ^(PulseCount trig: trig reset: reset) * mul!
trig: trig reset: reset mul: mul add: add
    ^(PulseCount trig: trig reset: reset) * mul + add!
!

PrimitiveUgen subclass: #PulseDivider
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PulseDivider comment: 'Pulse divider.'!

!PulseDivider class methodsFor: 'instance creation'!
trig: trig div: div start: start
    ^UgenBuilder name: 'PulseDivider' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {trig. div. start}!
primaryFactoryMethod
    ^#trig:div:start:!
trig: trig div: div start: start mul: mul
    ^(PulseDivider trig: trig div: div start: start) * mul!
trig: trig div: div start: start mul: mul add: add
    ^(PulseDivider trig: trig div: div start: start) * mul + add!
!

PrimitiveUgen subclass: #PV_RandComb
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PV_RandComb comment: 'Pass random bins.'!

!PV_RandComb class methodsFor: 'instance creation'!
buffer: buffer wipe: wipe trig: trig
    ^UgenBuilder name: 'PV_RandComb' numChan: 1 rateSpec: (Rate kr) specialIndex: 0 inputArray: {buffer. wipe. trig}!
primaryFactoryMethod
    ^#buffer:wipe:trig:!
buffer: buffer wipe: wipe trig: trig mul: mul
    ^(PV_RandComb buffer: buffer wipe: wipe trig: trig) * mul!
buffer: buffer wipe: wipe trig: trig mul: mul add: add
    ^(PV_RandComb buffer: buffer wipe: wipe trig: trig) * mul + add!
!

PrimitiveUgen subclass: #QuadL
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

QuadL comment: 'General quadratic map chaotic generator'!

!QuadL class methodsFor: 'instance creation'!
freq: freq a: a b: b c: c xi: xi
    ^UgenBuilder name: 'QuadL' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq. a. b. c. xi}!
primaryFactoryMethod
    ^#freq:a:b:c:xi:!
freq: freq a: a b: b c: c xi: xi mul: mul
    ^(QuadL freq: freq a: a b: b c: c xi: xi) * mul!
freq: freq a: a b: b c: c xi: xi mul: mul add: add
    ^(QuadL freq: freq a: a b: b c: c xi: xi) * mul + add!
!

PrimitiveUgen subclass: #QuadC
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

QuadC comment: 'General quadratic map chaotic generator'!

!QuadC class methodsFor: 'instance creation'!
freq: freq a: a b: b c: c xi: xi
    ^UgenBuilder name: 'QuadC' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq. a. b. c. xi}!
primaryFactoryMethod
    ^#freq:a:b:c:xi:!
freq: freq a: a b: b c: c xi: xi mul: mul
    ^(QuadC freq: freq a: a b: b c: c xi: xi) * mul!
freq: freq a: a b: b c: c xi: xi mul: mul add: add
    ^(QuadC freq: freq a: a b: b c: c xi: xi) * mul + add!
!

PrimitiveUgen subclass: #RHPF
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

RHPF comment: 'A resonant high pass filter.'!

!RHPF class methodsFor: 'instance creation'!
in: in freq: freq rq: rq
    ^UgenBuilder name: 'RHPF' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. freq. rq}!
primaryFactoryMethod
    ^#in:freq:rq:!
in: in freq: freq rq: rq mul: mul
    ^(RHPF in: in freq: freq rq: rq) * mul!
in: in freq: freq rq: rq mul: mul add: add
    ^(RHPF in: in freq: freq rq: rq) * mul + add!
!

PrimitiveUgen subclass: #RLPF
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

RLPF comment: 'A resonant low pass filter.'!

!RLPF class methodsFor: 'instance creation'!
in: in freq: freq rq: rq
    ^UgenBuilder name: 'RLPF' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. freq. rq}!
primaryFactoryMethod
    ^#in:freq:rq:!
in: in freq: freq rq: rq mul: mul
    ^(RLPF in: in freq: freq rq: rq) * mul!
in: in freq: freq rq: rq mul: mul add: add
    ^(RLPF in: in freq: freq rq: rq) * mul + add!
!

PrimitiveUgen subclass: #Rand
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Rand comment: 'Single random number generator.'!

!Rand class methodsFor: 'instance creation'!
lo: lo hi: hi
    ^UgenBuilder name: 'Rand' numChan: 1 rateSpec: (Rate ir) specialIndex: 0 inputArray: {lo. hi}!
primaryFactoryMethod
    ^#lo:hi:!
lo: lo hi: hi mul: mul
    ^(Rand lo: lo hi: hi) * mul!
lo: lo hi: hi mul: mul add: add
    ^(Rand lo: lo hi: hi) * mul + add!
!

PrimitiveUgen subclass: #RecordBuf
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

RecordBuf comment: 'Record or overdub into a Buffer.'!

!RecordBuf class methodsFor: 'instance creation'!
bufnum: bufnum offset: offset recLevel: recLevel preLevel: preLevel run: run loop: loop trigger: trigger doneAction: doneAction inputArray: inputArray
    ^UgenBuilder name: 'RecordBuf' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: ({bufnum. offset. recLevel. preLevel. run. loop. trigger. doneAction} , (inputArray asArray))!
primaryFactoryMethod
    ^#bufnum:offset:recLevel:preLevel:run:loop:trigger:doneAction:inputArray:!
bufnum: bufnum offset: offset recLevel: recLevel preLevel: preLevel run: run loop: loop trigger: trigger doneAction: doneAction inputArray: inputArray mul: mul
    ^(RecordBuf bufnum: bufnum offset: offset recLevel: recLevel preLevel: preLevel run: run loop: loop trigger: trigger doneAction: doneAction inputArray: inputArray) * mul!
bufnum: bufnum offset: offset recLevel: recLevel preLevel: preLevel run: run loop: loop trigger: trigger doneAction: doneAction inputArray: inputArray mul: mul add: add
    ^(RecordBuf bufnum: bufnum offset: offset recLevel: recLevel preLevel: preLevel run: run loop: loop trigger: trigger doneAction: doneAction inputArray: inputArray) * mul + add!
!

PrimitiveUgen subclass: #ReplaceOut
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

ReplaceOut comment: 'Send signal to a bus, overwriting previous contents.'!

!ReplaceOut class methodsFor: 'instance creation'!
bus: bus channelsArray: channelsArray
    ^UgenBuilder name: 'ReplaceOut' numChan: 0 rateSpec: #(2) specialIndex: 0 inputArray: ({bus} , (channelsArray asArray))!
primaryFactoryMethod
    ^#bus:channelsArray:!
bus: bus channelsArray: channelsArray mul: mul
    ^(ReplaceOut bus: bus channelsArray: channelsArray) * mul!
bus: bus channelsArray: channelsArray mul: mul add: add
    ^(ReplaceOut bus: bus channelsArray: channelsArray) * mul + add!
!

PrimitiveUgen subclass: #Resonz
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Resonz comment: 'Resonant filter.'!

!Resonz class methodsFor: 'instance creation'!
in: in freq: freq bwr: bwr
    ^UgenBuilder name: 'Resonz' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. freq. bwr}!
primaryFactoryMethod
    ^#in:freq:bwr:!
in: in freq: freq bwr: bwr mul: mul
    ^(Resonz in: in freq: freq bwr: bwr) * mul!
in: in freq: freq bwr: bwr mul: mul add: add
    ^(Resonz in: in freq: freq bwr: bwr) * mul + add!
!

PrimitiveUgen subclass: #Ringz
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Ringz comment: 'Ringing filter.'!

!Ringz class methodsFor: 'instance creation'!
in: in freq: freq decaytime: decaytime
    ^UgenBuilder name: 'Ringz' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. freq. decaytime}!
primaryFactoryMethod
    ^#in:freq:decaytime:!
in: in freq: freq decaytime: decaytime mul: mul
    ^(Ringz in: in freq: freq decaytime: decaytime) * mul!
in: in freq: freq decaytime: decaytime mul: mul add: add
    ^(Ringz in: in freq: freq decaytime: decaytime) * mul + add!
!

PrimitiveUgen subclass: #RunningMax
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

RunningMax comment: 'Track maximum level.'!

!RunningMax class methodsFor: 'instance creation'!
in: in trig: trig
    ^UgenBuilder name: 'RunningMax' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. trig}!
primaryFactoryMethod
    ^#in:trig:!
in: in trig: trig mul: mul
    ^(RunningMax in: in trig: trig) * mul!
in: in trig: trig mul: mul add: add
    ^(RunningMax in: in trig: trig) * mul + add!
!

PrimitiveUgen subclass: #RunningSum
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

RunningSum comment: 'Running sum over n frames'!

!RunningSum class methodsFor: 'instance creation'!
in: in numsamp: numsamp
    ^UgenBuilder name: 'RunningSum' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. numsamp}!
primaryFactoryMethod
    ^#in:numsamp:!
in: in numsamp: numsamp mul: mul
    ^(RunningSum in: in numsamp: numsamp) * mul!
in: in numsamp: numsamp mul: mul add: add
    ^(RunningSum in: in numsamp: numsamp) * mul + add!
!

PrimitiveUgen subclass: #Rotate2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Rotate2 comment: 'Rotate a sound field.'!

!Rotate2 class methodsFor: 'instance creation'!
x: x y: y pos: pos
    ^UgenBuilder name: 'Rotate2' numChan: 2 rateSpec: #(1 2) specialIndex: 0 inputArray: {x. y. pos}!
primaryFactoryMethod
    ^#x:y:pos:!
x: x y: y pos: pos mul: mul
    ^(Rotate2 x: x y: y pos: pos) * mul!
x: x y: y pos: pos mul: mul add: add
    ^(Rotate2 x: x y: y pos: pos) * mul + add!
!

PrimitiveUgen subclass: #SampleDur
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

SampleDur comment: 'Duration of one sample.'!

!SampleDur class methodsFor: 'instance creation'!
new
    ^UgenBuilder name: 'SampleDur' numChan: 1 rateSpec: (Rate ir) specialIndex: 0 inputArray: {}!
primaryFactoryMethod
    ^#new!
mul: mul
    ^(SampleDur new) * mul!
mul: mul add: add
    ^(SampleDur new) * mul + add!
!

PrimitiveUgen subclass: #SampleRate
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

SampleRate comment: 'Server sample rate.'!

!SampleRate class methodsFor: 'instance creation'!
new
    ^UgenBuilder name: 'SampleRate' numChan: 1 rateSpec: (Rate ir) specialIndex: 0 inputArray: {}!
primaryFactoryMethod
    ^#new!
mul: mul
    ^(SampleRate new) * mul!
mul: mul add: add
    ^(SampleRate new) * mul + add!
!

PrimitiveUgen subclass: #Saw
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Saw comment: 'Band limited sawtooth.'!

!Saw class methodsFor: 'instance creation'!
freq: freq
    ^UgenBuilder name: 'Saw' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq}!
primaryFactoryMethod
    ^#freq:!
freq: freq mul: mul
    ^(Saw freq: freq) * mul!
freq: freq mul: mul add: add
    ^(Saw freq: freq) * mul + add!
!

PrimitiveUgen subclass: #Schmidt
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Schmidt comment: 'Schmidt trigger.'!

!Schmidt class methodsFor: 'instance creation'!
in: in lo: lo hi: hi
    ^UgenBuilder name: 'Schmidt' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. lo. hi}!
primaryFactoryMethod
    ^#in:lo:hi:!
in: in lo: lo hi: hi mul: mul
    ^(Schmidt in: in lo: lo hi: hi) * mul!
in: in lo: lo hi: hi mul: mul add: add
    ^(Schmidt in: in lo: lo hi: hi) * mul + add!
!

PrimitiveUgen subclass: #Select
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Select comment: 'Select output from an array of inputs.'!

!Select class methodsFor: 'instance creation'!
which: which array: array
    ^UgenBuilder name: 'Select' numChan: 1 rateSpec: #(1 2) specialIndex: 0 inputArray: ({which} , (array asArray))!
primaryFactoryMethod
    ^#which:array:!
which: which array: array mul: mul
    ^(Select which: which array: array) * mul!
which: which array: array mul: mul add: add
    ^(Select which: which array: array) * mul + add!
!

PrimitiveUgen subclass: #SetBuf
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

SetBuf comment: 'Set local buffer'!

!SetBuf class methodsFor: 'instance creation'!
buf: buf offset: offset length: length array: array
    ^UgenBuilder name: 'SetBuf' numChan: 1 rateSpec: (Rate ir) specialIndex: 0 inputArray: ({buf. offset. length} , (array asArray))!
primaryFactoryMethod
    ^#buf:offset:length:array:!
buf: buf offset: offset length: length array: array mul: mul
    ^(SetBuf buf: buf offset: offset length: length array: array) * mul!
buf: buf offset: offset length: length array: array mul: mul add: add
    ^(SetBuf buf: buf offset: offset length: length array: array) * mul + add!
!

PrimitiveUgen subclass: #SetResetFF
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

SetResetFF comment: 'Set-reset flip flop.'!

!SetResetFF class methodsFor: 'instance creation'!
trig: trig reset: reset
    ^UgenBuilder name: 'SetResetFF' numChan: 1 rateSpec: #(1 2) specialIndex: 0 inputArray: {trig. reset}!
primaryFactoryMethod
    ^#trig:reset:!
trig: trig reset: reset mul: mul
    ^(SetResetFF trig: trig reset: reset) * mul!
trig: trig reset: reset mul: mul add: add
    ^(SetResetFF trig: trig reset: reset) * mul + add!
!

PrimitiveUgen subclass: #SinOsc
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

SinOsc comment: 'Interpolating sine wavetable oscillator.'!

!SinOsc class methodsFor: 'instance creation'!
freq: freq phase: phase
    ^UgenBuilder name: 'SinOsc' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq. phase}!
primaryFactoryMethod
    ^#freq:phase:!
freq: freq phase: phase mul: mul
    ^(SinOsc freq: freq phase: phase) * mul!
freq: freq phase: phase mul: mul add: add
    ^(SinOsc freq: freq phase: phase) * mul + add!
!

PrimitiveUgen subclass: #SinOscFB
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

SinOscFB comment: 'Feedback FM oscillator'!

!SinOscFB class methodsFor: 'instance creation'!
freq: freq feedback: feedback
    ^UgenBuilder name: 'SinOscFB' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq. feedback}!
primaryFactoryMethod
    ^#freq:feedback:!
freq: freq feedback: feedback mul: mul
    ^(SinOscFB freq: freq feedback: feedback) * mul!
freq: freq feedback: feedback mul: mul add: add
    ^(SinOscFB freq: freq feedback: feedback) * mul + add!
!

PrimitiveUgen subclass: #Slew
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Slew comment: 'Slew rate limiter.'!

!Slew class methodsFor: 'instance creation'!
in: in up: up dn: dn
    ^UgenBuilder name: 'Slew' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. up. dn}!
primaryFactoryMethod
    ^#in:up:dn:!
in: in up: up dn: dn mul: mul
    ^(Slew in: in up: up dn: dn) * mul!
in: in up: up dn: dn mul: mul add: add
    ^(Slew in: in up: up dn: dn) * mul + add!
!

PrimitiveUgen subclass: #Slope
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Slope comment: 'Slope of signal'!

!Slope class methodsFor: 'instance creation'!
in: in
    ^UgenBuilder name: 'Slope' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in}!
primaryFactoryMethod
    ^#in:!
in: in mul: mul
    ^(Slope in: in) * mul!
in: in mul: mul add: add
    ^(Slope in: in) * mul + add!
!

PrimitiveUgen subclass: #SOS
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

SOS comment: 'Second order filter section (biquad).'!

!SOS class methodsFor: 'instance creation'!
in: in a0: a0 a1: a1 a2: a2 b1: b1 b2: b2
    ^UgenBuilder name: 'SOS' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. a0. a1. a2. b1. b2}!
primaryFactoryMethod
    ^#in:a0:a1:a2:b1:b2:!
in: in a0: a0 a1: a1 a2: a2 b1: b1 b2: b2 mul: mul
    ^(SOS in: in a0: a0 a1: a1 a2: a2 b1: b1 b2: b2) * mul!
in: in a0: a0 a1: a1 a2: a2 b1: b1 b2: b2 mul: mul add: add
    ^(SOS in: in a0: a0 a1: a1 a2: a2 b1: b1 b2: b2) * mul + add!
!

PrimitiveUgen subclass: #Stepper
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Stepper comment: 'Pulse counter.'!

!Stepper class methodsFor: 'instance creation'!
trig: trig reset: reset min: min max: max step: step resetval: resetval
    ^UgenBuilder name: 'Stepper' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {trig. reset. min. max. step. resetval}!
primaryFactoryMethod
    ^#trig:reset:min:max:step:resetval:!
trig: trig reset: reset min: min max: max step: step resetval: resetval mul: mul
    ^(Stepper trig: trig reset: reset min: min max: max step: step resetval: resetval) * mul!
trig: trig reset: reset min: min max: max step: step resetval: resetval mul: mul add: add
    ^(Stepper trig: trig reset: reset min: min max: max step: step resetval: resetval) * mul + add!
!

PrimitiveUgen subclass: #Sweep
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Sweep comment: 'Triggered linear ramp'!

!Sweep class methodsFor: 'instance creation'!
trig: trig rate: rate
    ^UgenBuilder name: 'Sweep' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {trig. rate}!
primaryFactoryMethod
    ^#trig:rate:!
trig: trig rate: rate mul: mul
    ^(Sweep trig: trig rate: rate) * mul!
trig: trig rate: rate mul: mul add: add
    ^(Sweep trig: trig rate: rate) * mul + add!
!

PrimitiveUgen subclass: #SyncSaw
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

SyncSaw comment: 'Hard sync sawtooth wave.'!

!SyncSaw class methodsFor: 'instance creation'!
syncFreq: syncFreq sawFreq: sawFreq
    ^UgenBuilder name: 'SyncSaw' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {syncFreq. sawFreq}!
primaryFactoryMethod
    ^#syncFreq:sawFreq:!
syncFreq: syncFreq sawFreq: sawFreq mul: mul
    ^(SyncSaw syncFreq: syncFreq sawFreq: sawFreq) * mul!
syncFreq: syncFreq sawFreq: sawFreq mul: mul add: add
    ^(SyncSaw syncFreq: syncFreq sawFreq: sawFreq) * mul + add!
!

PrimitiveUgen subclass: #TDuty
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

TDuty comment: 'Demand results as trigger from demand rate UGens.'!

!TDuty class methodsFor: 'instance creation'!
dur: dur reset: reset doneAction: doneAction level: level gapFirst: gapFirst
    ^UgenBuilder name: 'TDuty' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {dur. reset. doneAction. level. gapFirst}!
primaryFactoryMethod
    ^#dur:reset:doneAction:level:gapFirst:!
dur: dur reset: reset doneAction: doneAction level: level gapFirst: gapFirst mul: mul
    ^(TDuty dur: dur reset: reset doneAction: doneAction level: level gapFirst: gapFirst) * mul!
dur: dur reset: reset doneAction: doneAction level: level gapFirst: gapFirst mul: mul add: add
    ^(TDuty dur: dur reset: reset doneAction: doneAction level: level gapFirst: gapFirst) * mul + add!
!

PrimitiveUgen subclass: #TExpRand
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

TExpRand comment: 'Triggered exponential random number generator.'!

!TExpRand class methodsFor: 'instance creation'!
lo: lo hi: hi trig: trig
    ^UgenBuilder name: 'TExpRand' numChan: 1 rateSpec: #(3) specialIndex: 0 inputArray: {lo. hi. trig}!
primaryFactoryMethod
    ^#lo:hi:trig:!
lo: lo hi: hi trig: trig mul: mul
    ^(TExpRand lo: lo hi: hi trig: trig) * mul!
lo: lo hi: hi trig: trig mul: mul add: add
    ^(TExpRand lo: lo hi: hi trig: trig) * mul + add!
!

PrimitiveUgen subclass: #TGrains
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

TGrains comment: 'Buffer granulator.'!

!TGrains class methodsFor: 'instance creation'!
numChan: numChan trigger: trigger bufnum: bufnum rate: rate centerPos: centerPos dur: dur pan: pan amp: amp interp: interp
    ^UgenBuilder name: 'TGrains' numChan: numChan rateSpec: (Rate ar) specialIndex: 0 inputArray: {trigger. bufnum. rate. centerPos. dur. pan. amp. interp}!
primaryFactoryMethod
    ^#numChan:trigger:bufnum:rate:centerPos:dur:pan:amp:interp:!
numChan: numChan trigger: trigger bufnum: bufnum rate: rate centerPos: centerPos dur: dur pan: pan amp: amp interp: interp mul: mul
    ^(TGrains numChan: numChan trigger: trigger bufnum: bufnum rate: rate centerPos: centerPos dur: dur pan: pan amp: amp interp: interp) * mul!
numChan: numChan trigger: trigger bufnum: bufnum rate: rate centerPos: centerPos dur: dur pan: pan amp: amp interp: interp mul: mul add: add
    ^(TGrains numChan: numChan trigger: trigger bufnum: bufnum rate: rate centerPos: centerPos dur: dur pan: pan amp: amp interp: interp) * mul + add!
!

PrimitiveUgen subclass: #Timer
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Timer comment: 'Returns time since last triggered.'!

!Timer class methodsFor: 'instance creation'!
trig: trig
    ^UgenBuilder name: 'Timer' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {trig}!
primaryFactoryMethod
    ^#trig:!
trig: trig mul: mul
    ^(Timer trig: trig) * mul!
trig: trig mul: mul add: add
    ^(Timer trig: trig) * mul + add!
!

PrimitiveUgen subclass: #TIRand
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

TIRand comment: 'Triggered integer random number generator.'!

!TIRand class methodsFor: 'instance creation'!
lo: lo hi: hi trig: trig
    ^UgenBuilder name: 'TIRand' numChan: 1 rateSpec: #(3) specialIndex: 0 inputArray: {lo. hi. trig}!
primaryFactoryMethod
    ^#lo:hi:trig:!
lo: lo hi: hi trig: trig mul: mul
    ^(TIRand lo: lo hi: hi trig: trig) * mul!
lo: lo hi: hi trig: trig mul: mul add: add
    ^(TIRand lo: lo hi: hi trig: trig) * mul + add!
!

PrimitiveUgen subclass: #ToggleFF
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

ToggleFF comment: 'Toggle flip flop.'!

!ToggleFF class methodsFor: 'instance creation'!
trig: trig
    ^UgenBuilder name: 'ToggleFF' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {trig}!
primaryFactoryMethod
    ^#trig:!
trig: trig mul: mul
    ^(ToggleFF trig: trig) * mul!
trig: trig mul: mul add: add
    ^(ToggleFF trig: trig) * mul + add!
!

PrimitiveUgen subclass: #TRand
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

TRand comment: 'Triggered random number generator.'!

!TRand class methodsFor: 'instance creation'!
lo: lo hi: hi trig: trig
    ^UgenBuilder name: 'TRand' numChan: 1 rateSpec: #(3) specialIndex: 0 inputArray: {lo. hi. trig}!
primaryFactoryMethod
    ^#lo:hi:trig:!
lo: lo hi: hi trig: trig mul: mul
    ^(TRand lo: lo hi: hi trig: trig) * mul!
lo: lo hi: hi trig: trig mul: mul add: add
    ^(TRand lo: lo hi: hi trig: trig) * mul + add!
!

PrimitiveUgen subclass: #Trig
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Trig comment: 'Timed trigger.'!

!Trig class methodsFor: 'instance creation'!
in: in dur: dur
    ^UgenBuilder name: 'Trig' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. dur}!
primaryFactoryMethod
    ^#in:dur:!
in: in dur: dur mul: mul
    ^(Trig in: in dur: dur) * mul!
in: in dur: dur mul: mul add: add
    ^(Trig in: in dur: dur) * mul + add!
!

PrimitiveUgen subclass: #Trig1
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Trig1 comment: 'Timed trigger.'!

!Trig1 class methodsFor: 'instance creation'!
in: in dur: dur
    ^UgenBuilder name: 'Trig1' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. dur}!
primaryFactoryMethod
    ^#in:dur:!
in: in dur: dur mul: mul
    ^(Trig1 in: in dur: dur) * mul!
in: in dur: dur mul: mul add: add
    ^(Trig1 in: in dur: dur) * mul + add!
!

PrimitiveUgen subclass: #TwoPole
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

TwoPole comment: 'Two pole filter.'!

!TwoPole class methodsFor: 'instance creation'!
in: in freq: freq radius: radius
    ^UgenBuilder name: 'TwoPole' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. freq. radius}!
primaryFactoryMethod
    ^#in:freq:radius:!
in: in freq: freq radius: radius mul: mul
    ^(TwoPole in: in freq: freq radius: radius) * mul!
in: in freq: freq radius: radius mul: mul add: add
    ^(TwoPole in: in freq: freq radius: radius) * mul + add!
!

PrimitiveUgen subclass: #TwoZero
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

TwoZero comment: 'Two zero filter.'!

!TwoZero class methodsFor: 'instance creation'!
in: in freq: freq radius: radius
    ^UgenBuilder name: 'TwoZero' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. freq. radius}!
primaryFactoryMethod
    ^#in:freq:radius:!
in: in freq: freq radius: radius mul: mul
    ^(TwoZero in: in freq: freq radius: radius) * mul!
in: in freq: freq radius: radius mul: mul add: add
    ^(TwoZero in: in freq: freq radius: radius) * mul + add!
!

PrimitiveUgen subclass: #VarSaw
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

VarSaw comment: 'Variable duty saw'!

!VarSaw class methodsFor: 'instance creation'!
freq: freq iphase: iphase width: width
    ^UgenBuilder name: 'VarSaw' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq. iphase. width}!
primaryFactoryMethod
    ^#freq:iphase:width:!
freq: freq iphase: iphase width: width mul: mul
    ^(VarSaw freq: freq iphase: iphase width: width) * mul!
freq: freq iphase: iphase width: width mul: mul add: add
    ^(VarSaw freq: freq iphase: iphase width: width) * mul + add!
!

PrimitiveUgen subclass: #VBJonVerb
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

VBJonVerb comment: 'artifical reverberator'!

!VBJonVerb class methodsFor: 'instance creation'!
in: in decay: decay damping: damping inputbw: inputbw erfl: erfl tail: tail
    ^UgenBuilder name: 'VBJonVerb' numChan: 2 rateSpec: #(1) specialIndex: 0 inputArray: {in. decay. damping. inputbw. erfl. tail}!
primaryFactoryMethod
    ^#in:decay:damping:inputbw:erfl:tail:!
in: in decay: decay damping: damping inputbw: inputbw erfl: erfl tail: tail mul: mul
    ^(VBJonVerb in: in decay: decay damping: damping inputbw: inputbw erfl: erfl tail: tail) * mul!
in: in decay: decay damping: damping inputbw: inputbw erfl: erfl tail: tail mul: mul add: add
    ^(VBJonVerb in: in decay: decay damping: damping inputbw: inputbw erfl: erfl tail: tail) * mul + add!
!

PrimitiveUgen subclass: #Vibrato
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Vibrato comment: 'The Vibrato oscillator models a slow frequency modulation.'!

!Vibrato class methodsFor: 'instance creation'!
freq: freq rate: rate depth: depth delay: delay onset: onset rateVariation: rateVariation depthVariation: depthVariation iphase: iphase trig: trig
    ^UgenBuilder name: 'Vibrato' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq. rate. depth. delay. onset. rateVariation. depthVariation. iphase. trig}!
primaryFactoryMethod
    ^#freq:rate:depth:delay:onset:rateVariation:depthVariation:iphase:trig:!
freq: freq rate: rate depth: depth delay: delay onset: onset rateVariation: rateVariation depthVariation: depthVariation iphase: iphase trig: trig mul: mul
    ^(Vibrato freq: freq rate: rate depth: depth delay: delay onset: onset rateVariation: rateVariation depthVariation: depthVariation iphase: iphase trig: trig) * mul!
freq: freq rate: rate depth: depth delay: delay onset: onset rateVariation: rateVariation depthVariation: depthVariation iphase: iphase trig: trig mul: mul add: add
    ^(Vibrato freq: freq rate: rate depth: depth delay: delay onset: onset rateVariation: rateVariation depthVariation: depthVariation iphase: iphase trig: trig) * mul + add!
!

PrimitiveUgen subclass: #WaveLoss
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

WaveLoss comment: 'Lose bits of your waves'!

!WaveLoss class methodsFor: 'instance creation'!
in: in drop: drop outof: outof mode: mode
    ^UgenBuilder name: 'WaveLoss' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {in. drop. outof. mode}!
primaryFactoryMethod
    ^#in:drop:outof:mode:!
in: in drop: drop outof: outof mode: mode mul: mul
    ^(WaveLoss in: in drop: drop outof: outof mode: mode) * mul!
in: in drop: drop outof: outof mode: mode mul: mul add: add
    ^(WaveLoss in: in drop: drop outof: outof mode: mode) * mul + add!
!

PrimitiveUgen subclass: #WhiteNoise
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

WhiteNoise comment: 'White noise.'!

!WhiteNoise class methodsFor: 'instance creation'!
new
    ^UgenBuilder name: 'WhiteNoise' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {}!
primaryFactoryMethod
    ^#new!
mul: mul
    ^(WhiteNoise new) * mul!
mul: mul add: add
    ^(WhiteNoise new) * mul + add!
!

PrimitiveUgen subclass: #Wrap
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Wrap comment: 'Wrap a signal outside given thresholds.'!

!Wrap class methodsFor: 'instance creation'!
in: in lo: lo hi: hi
    ^UgenBuilder name: 'Wrap' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. lo. hi}!
primaryFactoryMethod
    ^#in:lo:hi:!
in: in lo: lo hi: hi mul: mul
    ^(Wrap in: in lo: lo hi: hi) * mul!
in: in lo: lo hi: hi mul: mul add: add
    ^(Wrap in: in lo: lo hi: hi) * mul + add!
!

PrimitiveUgen subclass: #WrapIndex
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

WrapIndex comment: 'Index into a table with a signal.'!

!WrapIndex class methodsFor: 'instance creation'!
bufnum: bufnum in: in
    ^UgenBuilder name: 'WrapIndex' numChan: 1 rateSpec: #(2) specialIndex: 0 inputArray: {bufnum. in}!
primaryFactoryMethod
    ^#bufnum:in:!
bufnum: bufnum in: in mul: mul
    ^(WrapIndex bufnum: bufnum in: in) * mul!
bufnum: bufnum in: in mul: mul add: add
    ^(WrapIndex bufnum: bufnum in: in) * mul + add!
!

PrimitiveUgen subclass: #XFade2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

XFade2 comment: 'Equal power two channel cross fade.'!

!XFade2 class methodsFor: 'instance creation'!
inA: inA inB: inB pan: pan level: level
    ^UgenBuilder name: 'XFade2' numChan: 1 rateSpec: #(1 2) specialIndex: 0 inputArray: {inA. inB. pan. level}!
primaryFactoryMethod
    ^#inA:inB:pan:level:!
inA: inA inB: inB pan: pan level: level mul: mul
    ^(XFade2 inA: inA inB: inB pan: pan level: level) * mul!
inA: inA inB: inB pan: pan level: level mul: mul add: add
    ^(XFade2 inA: inA inB: inB pan: pan level: level) * mul + add!
!

PrimitiveUgen subclass: #XLine
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

XLine comment: 'Exponential line generator.'!

!XLine class methodsFor: 'instance creation'!
start: start end: end dur: dur doneAction: doneAction
    ^UgenBuilder name: 'XLine' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {start. end. dur. doneAction}!
primaryFactoryMethod
    ^#start:end:dur:doneAction:!
start: start end: end dur: dur doneAction: doneAction mul: mul
    ^(XLine start: start end: end dur: dur doneAction: doneAction) * mul!
start: start end: end dur: dur doneAction: doneAction mul: mul add: add
    ^(XLine start: start end: end dur: dur doneAction: doneAction) * mul + add!
!

PrimitiveUgen subclass: #ZeroCrossing
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

ZeroCrossing comment: 'Zero crossing frequency follower'!

!ZeroCrossing class methodsFor: 'instance creation'!
in: in
    ^UgenBuilder name: 'ZeroCrossing' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in}!
primaryFactoryMethod
    ^#in:!
in: in mul: mul
    ^(ZeroCrossing in: in) * mul!
in: in mul: mul add: add
    ^(ZeroCrossing in: in) * mul + add!
!

PrimitiveUgen subclass: #MoogLadder
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

MoogLadder comment: 'Moog Filter Emulation'!

!MoogLadder class methodsFor: 'instance creation'!
in: in ffreq: ffreq res: res
    ^UgenBuilder name: 'MoogLadder' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. ffreq. res}!
primaryFactoryMethod
    ^#in:ffreq:res:!
in: in ffreq: ffreq res: res mul: mul
    ^(MoogLadder in: in ffreq: ffreq res: res) * mul!
in: in ffreq: ffreq res: res mul: mul add: add
    ^(MoogLadder in: in ffreq: ffreq res: res) * mul + add!
!

PrimitiveUgen subclass: #GreyholeRaw
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

GreyholeRaw comment: 'algorithmic delay'!

!GreyholeRaw class methodsFor: 'instance creation'!
in1: in1 in2: in2 damping: damping delaytime: delaytime diffusion: diffusion feedback: feedback moddepth: moddepth modfreq: modfreq size: size
    ^UgenBuilder name: 'GreyholeRaw' numChan: 2 rateSpec: #(1 2) specialIndex: 0 inputArray: {in1. in2. damping. delaytime. diffusion. feedback. moddepth. modfreq. size}!
primaryFactoryMethod
    ^#in1:in2:damping:delaytime:diffusion:feedback:moddepth:modfreq:size:!
in1: in1 in2: in2 damping: damping delaytime: delaytime diffusion: diffusion feedback: feedback moddepth: moddepth modfreq: modfreq size: size mul: mul
    ^(GreyholeRaw in1: in1 in2: in2 damping: damping delaytime: delaytime diffusion: diffusion feedback: feedback moddepth: moddepth modfreq: modfreq size: size) * mul!
in1: in1 in2: in2 damping: damping delaytime: delaytime diffusion: diffusion feedback: feedback moddepth: moddepth modfreq: modfreq size: size mul: mul add: add
    ^(GreyholeRaw in1: in1 in2: in2 damping: damping delaytime: delaytime diffusion: diffusion feedback: feedback moddepth: moddepth modfreq: modfreq size: size) * mul + add!
!

PrimitiveUgen subclass: #CrossoverDistortion
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

CrossoverDistortion comment: 'class B/AB power amp distortion simulation'!

!CrossoverDistortion class methodsFor: 'instance creation'!
in: in amp: amp smooth: smooth
    ^UgenBuilder name: 'CrossoverDistortion' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. amp. smooth}!
primaryFactoryMethod
    ^#in:amp:smooth:!
in: in amp: amp smooth: smooth mul: mul
    ^(CrossoverDistortion in: in amp: amp smooth: smooth) * mul!
in: in amp: amp smooth: smooth mul: mul add: add
    ^(CrossoverDistortion in: in amp: amp smooth: smooth) * mul + add!
!

PrimitiveUgen subclass: #Friction
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Friction comment: 'A physical model of a system with dry-friction. A chaotic filter.'!

!Friction class methodsFor: 'instance creation'!
in: in friction: friction spring: spring damp: damp mass: mass beltmass: beltmass
    ^UgenBuilder name: 'Friction' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {in. friction. spring. damp. mass. beltmass}!
primaryFactoryMethod
    ^#in:friction:spring:damp:mass:beltmass:!
in: in friction: friction spring: spring damp: damp mass: mass beltmass: beltmass mul: mul
    ^(Friction in: in friction: friction spring: spring damp: damp mass: mass beltmass: beltmass) * mul!
in: in friction: friction spring: spring damp: damp mass: mass beltmass: beltmass mul: mul add: add
    ^(Friction in: in friction: friction spring: spring damp: damp mass: mass beltmass: beltmass) * mul + add!
!

PrimitiveUgen subclass: #MembraneCircle
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

MembraneCircle comment: 'Waveguide mesh physical models of drum membranes'!

!MembraneCircle class methodsFor: 'instance creation'!
excitation: excitation tension: tension loss: loss
    ^UgenBuilder name: 'MembraneCircle' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {excitation. tension. loss}!
primaryFactoryMethod
    ^#excitation:tension:loss:!
excitation: excitation tension: tension loss: loss mul: mul
    ^(MembraneCircle excitation: excitation tension: tension loss: loss) * mul!
excitation: excitation tension: tension loss: loss mul: mul add: add
    ^(MembraneCircle excitation: excitation tension: tension loss: loss) * mul + add!
!

PrimitiveUgen subclass: #VOSIM
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

VOSIM comment: 'vosim pulse generator'!

!VOSIM class methodsFor: 'instance creation'!
trig: trig freq: freq nCycles: nCycles decay: decay
    ^UgenBuilder name: 'VOSIM' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {trig. freq. nCycles. decay}!
primaryFactoryMethod
    ^#trig:freq:nCycles:decay:!
trig: trig freq: freq nCycles: nCycles decay: decay mul: mul
    ^(VOSIM trig: trig freq: freq nCycles: nCycles decay: decay) * mul!
trig: trig freq: freq nCycles: nCycles decay: decay mul: mul add: add
    ^(VOSIM trig: trig freq: freq nCycles: nCycles decay: decay) * mul + add!
!

PrimitiveUgen subclass: #MiRings
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

MiRings comment: 'a resonator'!

!MiRings class methodsFor: 'instance creation'!
in: in trig: trig pit: pit struct: struct bright: bright damp: damp pos: pos model: model poly: poly intern_exciter: intern_exciter easteregg: easteregg bypass: bypass
    ^UgenBuilder name: 'MiRings' numChan: 2 rateSpec: (Rate ar) specialIndex: 0 inputArray: {in. trig. pit. struct. bright. damp. pos. model. poly. intern_exciter. easteregg. bypass}!
primaryFactoryMethod
    ^#in:trig:pit:struct:bright:damp:pos:model:poly:intern_exciter:easteregg:bypass:!
in: in trig: trig pit: pit struct: struct bright: bright damp: damp pos: pos model: model poly: poly intern_exciter: intern_exciter easteregg: easteregg bypass: bypass mul: mul
    ^(MiRings in: in trig: trig pit: pit struct: struct bright: bright damp: damp pos: pos model: model poly: poly intern_exciter: intern_exciter easteregg: easteregg bypass: bypass) * mul!
in: in trig: trig pit: pit struct: struct bright: bright damp: damp pos: pos model: model poly: poly intern_exciter: intern_exciter easteregg: easteregg bypass: bypass mul: mul add: add
    ^(MiRings in: in trig: trig pit: pit struct: struct bright: bright damp: damp pos: pos model: model poly: poly intern_exciter: intern_exciter easteregg: easteregg bypass: bypass) * mul + add!
!

PrimitiveUgen subclass: #AnalogFoldOsc
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

AnalogFoldOsc comment: '(Undocumented class)'!

!AnalogFoldOsc class methodsFor: 'instance creation'!
freq: freq amp: amp
    ^UgenBuilder name: 'AnalogFoldOsc' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {freq. amp}!
primaryFactoryMethod
    ^#freq:amp:!
freq: freq amp: amp mul: mul
    ^(AnalogFoldOsc freq: freq amp: amp) * mul!
freq: freq amp: amp mul: mul add: add
    ^(AnalogFoldOsc freq: freq amp: amp) * mul + add!
!

PrimitiveUgen subclass: #RCD
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

RCD comment: 'rotating clock divider'!

!RCD class methodsFor: 'instance creation'!
clock: clock rotate: rotate reset: reset div: div spread: spread auto: auto len: len down: down gates: gates
    ^UgenBuilder name: 'RCD' numChan: 8 rateSpec: #(1) specialIndex: 0 inputArray: {clock. rotate. reset. div. spread. auto. len. down. gates}!
primaryFactoryMethod
    ^#clock:rotate:reset:div:spread:auto:len:down:gates:!
clock: clock rotate: rotate reset: reset div: div spread: spread auto: auto len: len down: down gates: gates mul: mul
    ^(RCD clock: clock rotate: rotate reset: reset div: div spread: spread auto: auto len: len down: down gates: gates) * mul!
clock: clock rotate: rotate reset: reset div: div spread: spread auto: auto len: len down: down gates: gates mul: mul add: add
    ^(RCD clock: clock rotate: rotate reset: reset div: div spread: spread auto: auto len: len down: down gates: gates) * mul + add!
!

PrimitiveUgen subclass: #SCM
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

SCM comment: 'shuffling clock multiplier'!

!SCM class methodsFor: 'instance creation'!
clock: clock bpm: bpm rotate: rotate slip: slip shuffle: shuffle skip: skip pw: pw
    ^UgenBuilder name: 'SCM' numChan: 8 rateSpec: (Rate ar) specialIndex: 0 inputArray: {clock. bpm. rotate. slip. shuffle. skip. pw}!
primaryFactoryMethod
    ^#clock:bpm:rotate:slip:shuffle:skip:pw:!
clock: clock bpm: bpm rotate: rotate slip: slip shuffle: shuffle skip: skip pw: pw mul: mul
    ^(SCM clock: clock bpm: bpm rotate: rotate slip: slip shuffle: shuffle skip: skip pw: pw) * mul!
clock: clock bpm: bpm rotate: rotate slip: slip shuffle: shuffle skip: skip pw: pw mul: mul add: add
    ^(SCM clock: clock bpm: bpm rotate: rotate slip: slip shuffle: shuffle skip: skip pw: pw) * mul + add!
!

PrimitiveUgen subclass: #DustRange
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

DustRange comment: '(Undocumented class)'!

!DustRange class methodsFor: 'instance creation'!
iotMin: iotMin iotMax: iotMax
    ^UgenBuilder name: 'DustRange' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {iotMin. iotMax}!
primaryFactoryMethod
    ^#iotMin:iotMax:!
iotMin: iotMin iotMax: iotMax mul: mul
    ^(DustRange iotMin: iotMin iotMax: iotMax) * mul!
iotMin: iotMin iotMax: iotMax mul: mul add: add
    ^(DustRange iotMin: iotMin iotMax: iotMax) * mul + add!
!

PrimitiveUgen subclass: #ExpRandN
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

ExpRandN comment: '(Undocumented class)'!

!ExpRandN class methodsFor: 'instance creation'!
numChan: numChan lo: lo hi: hi
    ^UgenBuilder name: 'ExpRandN' numChan: numChan rateSpec: (Rate ir) specialIndex: 0 inputArray: {lo. hi}!
primaryFactoryMethod
    ^#numChan:lo:hi:!
numChan: numChan lo: lo hi: hi mul: mul
    ^(ExpRandN numChan: numChan lo: lo hi: hi) * mul!
numChan: numChan lo: lo hi: hi mul: mul add: add
    ^(ExpRandN numChan: numChan lo: lo hi: hi) * mul + add!
!

PrimitiveUgen subclass: #LinRandN
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LinRandN comment: '(Undocumented class)'!

!LinRandN class methodsFor: 'instance creation'!
numChan: numChan lo: lo hi: hi minmax: minmax
    ^UgenBuilder name: 'LinRandN' numChan: numChan rateSpec: (Rate ir) specialIndex: 0 inputArray: {lo. hi. minmax}!
primaryFactoryMethod
    ^#numChan:lo:hi:minmax:!
numChan: numChan lo: lo hi: hi minmax: minmax mul: mul
    ^(LinRandN numChan: numChan lo: lo hi: hi minmax: minmax) * mul!
numChan: numChan lo: lo hi: hi minmax: minmax mul: mul add: add
    ^(LinRandN numChan: numChan lo: lo hi: hi minmax: minmax) * mul + add!
!

PrimitiveUgen subclass: #RandN
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

RandN comment: '(Undocumented class)'!

!RandN class methodsFor: 'instance creation'!
numChan: numChan lo: lo hi: hi
    ^UgenBuilder name: 'RandN' numChan: numChan rateSpec: (Rate ir) specialIndex: 0 inputArray: {lo. hi}!
primaryFactoryMethod
    ^#numChan:lo:hi:!
numChan: numChan lo: lo hi: hi mul: mul
    ^(RandN numChan: numChan lo: lo hi: hi) * mul!
numChan: numChan lo: lo hi: hi mul: mul add: add
    ^(RandN numChan: numChan lo: lo hi: hi) * mul + add!
!

PrimitiveUgen subclass: #TScramble
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

TScramble comment: '(Undocumented class)'!

!TScramble class methodsFor: 'instance creation'!
trigger: trigger inputs: inputs
    ^UgenBuilder name: 'TScramble' numChan: inputs asArray size rateSpec: #(1) specialIndex: 0 inputArray: ({trigger} , (inputs asArray))!
primaryFactoryMethod
    ^#trigger:inputs:!
trigger: trigger inputs: inputs mul: mul
    ^(TScramble trigger: trigger inputs: inputs) * mul!
trigger: trigger inputs: inputs mul: mul add: add
    ^(TScramble trigger: trigger inputs: inputs) * mul + add!
!

PrimitiveUgen subclass: #DX7
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

DX7 comment: '(Undocumented class)'!

!DX7 class methodsFor: 'instance creation'!
bufnum: bufnum on: on off: off data: data vc: vc mnn: mnn vel: vel pw: pw mw: mw bc: bc fc: fc
    ^UgenBuilder name: 'DX7' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {bufnum. on. off. data. vc. mnn. vel. pw. mw. bc. fc}!
primaryFactoryMethod
    ^#bufnum:on:off:data:vc:mnn:vel:pw:mw:bc:fc:!
bufnum: bufnum on: on off: off data: data vc: vc mnn: mnn vel: vel pw: pw mw: mw bc: bc fc: fc mul: mul
    ^(DX7 bufnum: bufnum on: on off: off data: data vc: vc mnn: mnn vel: vel pw: pw mw: mw bc: bc fc: fc) * mul!
bufnum: bufnum on: on off: off data: data vc: vc mnn: mnn vel: vel pw: pw mw: mw bc: bc fc: fc mul: mul add: add
    ^(DX7 bufnum: bufnum on: on off: off data: data vc: vc mnn: mnn vel: vel pw: pw mw: mw bc: bc fc: fc) * mul + add!
!

PrimitiveUgen subclass: #RDX7Env
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

RDX7Env comment: '(Undocumented class)'!

!RDX7Env class methodsFor: 'instance creation'!
gate: gate data: data r1: r1 r2: r2 r3: r3 r4: r4 l1: l1 l2: l2 l3: l3 l4: l4 ol: ol
    ^UgenBuilder name: 'RDX7Env' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {gate. data. r1. r2. r3. r4. l1. l2. l3. l4. ol}!
primaryFactoryMethod
    ^#gate:data:r1:r2:r3:r4:l1:l2:l3:l4:ol:!
gate: gate data: data r1: r1 r2: r2 r3: r3 r4: r4 l1: l1 l2: l2 l3: l3 l4: l4 ol: ol mul: mul
    ^(RDX7Env gate: gate data: data r1: r1 r2: r2 r3: r3 r4: r4 l1: l1 l2: l2 l3: l3 l4: l4 ol: ol) * mul!
gate: gate data: data r1: r1 r2: r2 r3: r3 r4: r4 l1: l1 l2: l2 l3: l3 l4: l4 ol: ol mul: mul add: add
    ^(RDX7Env gate: gate data: data r1: r1 r2: r2 r3: r3 r4: r4 l1: l1 l2: l2 l3: l3 l4: l4 ol: ol) * mul + add!
!

PrimitiveUgen subclass: #ObxdFilter
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

ObxdFilter comment: '(Undocumented class)'!

!ObxdFilter class methodsFor: 'instance creation'!
in: in cutoff: cutoff resonance: resonance multimode: multimode bandpass: bandpass fourpole: fourpole
    ^UgenBuilder name: 'ObxdFilter' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. cutoff. resonance. multimode. bandpass. fourpole}!
primaryFactoryMethod
    ^#in:cutoff:resonance:multimode:bandpass:fourpole:!
in: in cutoff: cutoff resonance: resonance multimode: multimode bandpass: bandpass fourpole: fourpole mul: mul
    ^(ObxdFilter in: in cutoff: cutoff resonance: resonance multimode: multimode bandpass: bandpass fourpole: fourpole) * mul!
in: in cutoff: cutoff resonance: resonance multimode: multimode bandpass: bandpass fourpole: fourpole mul: mul add: add
    ^(ObxdFilter in: in cutoff: cutoff resonance: resonance multimode: multimode bandpass: bandpass fourpole: fourpole) * mul + add!
!

PrimitiveUgen subclass: #SvfBp
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

SvfBp comment: '(Undocumented class)'!

!SvfBp class methodsFor: 'instance creation'!
in: in freq: freq q: q
    ^UgenBuilder name: 'SvfBp' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {in. freq. q}!
primaryFactoryMethod
    ^#in:freq:q:!
in: in freq: freq q: q mul: mul
    ^(SvfBp in: in freq: freq q: q) * mul!
in: in freq: freq q: q mul: mul add: add
    ^(SvfBp in: in freq: freq q: q) * mul + add!
!

PrimitiveUgen subclass: #SvfHp
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

SvfHp comment: '(Undocumented class)'!

!SvfHp class methodsFor: 'instance creation'!
in: in freq: freq q: q
    ^UgenBuilder name: 'SvfHp' numChan: 1 rateSpec: #(1) specialIndex: 0 inputArray: {in. freq. q}!
primaryFactoryMethod
    ^#in:freq:q:!
in: in freq: freq q: q mul: mul
    ^(SvfHp in: in freq: freq q: q) * mul!
in: in freq: freq q: q mul: mul add: add
    ^(SvfHp in: in freq: freq q: q) * mul + add!
!

PrimitiveUgen subclass: #SvfLp
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

SvfLp comment: '(Undocumented class)'!

!SvfLp class methodsFor: 'instance creation'!
in: in freq: freq q: q
    ^UgenBuilder name: 'SvfLp' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {in. freq. q}!
primaryFactoryMethod
    ^#in:freq:q:!
in: in freq: freq q: q mul: mul
    ^(SvfLp in: in freq: freq q: q) * mul!
in: in freq: freq q: q mul: mul add: add
    ^(SvfLp in: in freq: freq q: q) * mul + add!
!

PrimitiveUgen subclass: #Bezier
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Bezier comment: '(Undocumented class)'!

!Bezier class methodsFor: 'instance creation'!
haltAfter: haltAfter dx: dx freq: freq phase: phase param: param
    ^UgenBuilder name: 'Bezier' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: ({haltAfter. dx. freq. phase} , (param asArray))!
primaryFactoryMethod
    ^#haltAfter:dx:freq:phase:param:!
haltAfter: haltAfter dx: dx freq: freq phase: phase param: param mul: mul
    ^(Bezier haltAfter: haltAfter dx: dx freq: freq phase: phase param: param) * mul!
haltAfter: haltAfter dx: dx freq: freq phase: phase param: param mul: mul add: add
    ^(Bezier haltAfter: haltAfter dx: dx freq: freq phase: phase param: param) * mul + add!
!

PrimitiveUgen subclass: #Freezer
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Freezer comment: '(Undocumented class)'!

!Freezer class methodsFor: 'instance creation'!
bufnum: bufnum left: left right: right gain: gain increment: increment incrementOffset: incrementOffset incrementRandom: incrementRandom rightRandom: rightRandom syncPhaseTrigger: syncPhaseTrigger randomizePhaseTrigger: randomizePhaseTrigger numberOfLoops: numberOfLoops
    ^UgenBuilder name: 'Freezer' numChan: 1 rateSpec: (Rate ar) specialIndex: 0 inputArray: {bufnum. left. right. gain. increment. incrementOffset. incrementRandom. rightRandom. syncPhaseTrigger. randomizePhaseTrigger. numberOfLoops}!
primaryFactoryMethod
    ^#bufnum:left:right:gain:increment:incrementOffset:incrementRandom:rightRandom:syncPhaseTrigger:randomizePhaseTrigger:numberOfLoops:!
bufnum: bufnum left: left right: right gain: gain increment: increment incrementOffset: incrementOffset incrementRandom: incrementRandom rightRandom: rightRandom syncPhaseTrigger: syncPhaseTrigger randomizePhaseTrigger: randomizePhaseTrigger numberOfLoops: numberOfLoops mul: mul
    ^(Freezer bufnum: bufnum left: left right: right gain: gain increment: increment incrementOffset: incrementOffset incrementRandom: incrementRandom rightRandom: rightRandom syncPhaseTrigger: syncPhaseTrigger randomizePhaseTrigger: randomizePhaseTrigger numberOfLoops: numberOfLoops) * mul!
bufnum: bufnum left: left right: right gain: gain increment: increment incrementOffset: incrementOffset incrementRandom: incrementRandom rightRandom: rightRandom syncPhaseTrigger: syncPhaseTrigger randomizePhaseTrigger: randomizePhaseTrigger numberOfLoops: numberOfLoops mul: mul add: add
    ^(Freezer bufnum: bufnum left: left right: right gain: gain increment: increment incrementOffset: incrementOffset incrementRandom: incrementRandom rightRandom: rightRandom syncPhaseTrigger: syncPhaseTrigger randomizePhaseTrigger: randomizePhaseTrigger numberOfLoops: numberOfLoops) * mul + add!
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

