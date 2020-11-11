UGen subclass: #Amplitude
instanceVariableNames: 'in attackTime releaseTime' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!Amplitude commentStamp: '<historical>' prior: 0!
Amplitude follower !

!Amplitude methodsFor: 'accessing'!
in ^in !
attackTime ^attackTime !
releaseTime ^releaseTime !
!

!Amplitude methodsFor: 'private'!
setIn: x in := x !
setAttackTime: x attackTime := x !
setReleaseTime: x releaseTime := x !
!

!Amplitude methodsFor: 'private'!
initialize name := #Amplitude . paramNames := #(in attackTime releaseTime) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!Amplitude class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Amplitude class methodsFor: 'instance creation'!
in: in attackTime: attackTime releaseTime: releaseTime |u| u := self new . u setIn: in . u setAttackTime: attackTime . u setReleaseTime: releaseTime . ^ u ! !

UGen subclass: #BrownNoise
instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!BrownNoise commentStamp: '<historical>' prior: 0!
Brown Noise. !



!BrownNoise methodsFor: 'private'!
initialize name := #BrownNoise . paramNames := #() . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!BrownNoise class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !


UGen subclass: #BPZ2
instanceVariableNames: 'in' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!BPZ2 commentStamp: '<historical>' prior: 0!
Two zero fixed midpass. !

!BPZ2 methodsFor: 'accessing'!
in ^in !
!

!BPZ2 methodsFor: 'private'!
setIn: x in := x !
!

!BPZ2 methodsFor: 'private'!
initialize name := #BPZ2 . paramNames := #(in) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!BPZ2 class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!BPZ2 class methodsFor: 'instance creation'!
in: in |u| u := self new . u setIn: in . ^ u ! !

UGen subclass: #CombL
instanceVariableNames: 'in maxdelaytime delaytime decaytime' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!CombL commentStamp: '<historical>' prior: 0!
Comb delay line with linear interpolation. !

!CombL methodsFor: 'accessing'!
in ^in !
maxdelaytime ^maxdelaytime !
delaytime ^delaytime !
decaytime ^decaytime !
!

!CombL methodsFor: 'private'!
setIn: x in := x !
setMaxdelaytime: x maxdelaytime := x !
setDelaytime: x delaytime := x !
setDecaytime: x decaytime := x !
!

!CombL methodsFor: 'private'!
initialize name := #CombL . paramNames := #(in maxdelaytime delaytime decaytime) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!CombL class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!CombL class methodsFor: 'instance creation'!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime |u| u := self new . u setIn: in . u setMaxdelaytime: maxdelaytime . u setDelaytime: delaytime . u setDecaytime: decaytime . ^ u ! !

UGen subclass: #CombN
instanceVariableNames: 'in maxdelaytime delaytime decaytime' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!CombN commentStamp: '<historical>' prior: 0!
Comb delay line with no interpolation. !

!CombN methodsFor: 'accessing'!
in ^in !
maxdelaytime ^maxdelaytime !
delaytime ^delaytime !
decaytime ^decaytime !
!

!CombN methodsFor: 'private'!
setIn: x in := x !
setMaxdelaytime: x maxdelaytime := x !
setDelaytime: x delaytime := x !
setDecaytime: x decaytime := x !
!

!CombN methodsFor: 'private'!
initialize name := #CombN . paramNames := #(in maxdelaytime delaytime decaytime) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!CombN class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!CombN class methodsFor: 'instance creation'!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime |u| u := self new . u setIn: in . u setMaxdelaytime: maxdelaytime . u setDelaytime: delaytime . u setDecaytime: decaytime . ^ u ! !

UGen subclass: #Decay
instanceVariableNames: 'in decayTime' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!Decay commentStamp: '<historical>' prior: 0!
Exponential decay !

!Decay methodsFor: 'accessing'!
in ^in !
decayTime ^decayTime !
!

!Decay methodsFor: 'private'!
setIn: x in := x !
setDecayTime: x decayTime := x !
!

!Decay methodsFor: 'private'!
initialize name := #Decay . paramNames := #(in decayTime) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!Decay class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Decay class methodsFor: 'instance creation'!
in: in decayTime: decayTime |u| u := self new . u setIn: in . u setDecayTime: decayTime . ^ u ! !

UGen subclass: #HPZ1
instanceVariableNames: 'in' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!HPZ1 commentStamp: '<historical>' prior: 0!
Two point difference filter !

!HPZ1 methodsFor: 'accessing'!
in ^in !
!

!HPZ1 methodsFor: 'private'!
setIn: x in := x !
!

!HPZ1 methodsFor: 'private'!
initialize name := #HPZ1 . paramNames := #(in) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!HPZ1 class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!HPZ1 class methodsFor: 'instance creation'!
in: in |u| u := self new . u setIn: in . ^ u ! !

UGen subclass: #In
instanceVariableNames: 'bus' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!In commentStamp: '<historical>' prior: 0!
Read a signal from a bus. !

!In methodsFor: 'accessing'!
bus ^bus !
!

!In methodsFor: 'private'!
setBus: x bus := x !
!

!In methodsFor: 'private'!
initialize name := #In . paramNames := #(bus) . mceNames := #() . rate := #ar . numChan := -1 . specialIndex := 0 ! !

!In class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!In class methodsFor: 'instance creation'!
numChan: numChan bus: bus |u| u := self new . u setNumChan: numChan . u setBus: bus . ^ u ! !

UGen subclass: #LeakDC
instanceVariableNames: 'in coef' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!LeakDC commentStamp: '<historical>' prior: 0!
Remove DC !

!LeakDC methodsFor: 'accessing'!
in ^in !
coef ^coef !
!

!LeakDC methodsFor: 'private'!
setIn: x in := x !
setCoef: x coef := x !
!

!LeakDC methodsFor: 'private'!
initialize name := #LeakDC . paramNames := #(in coef) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!LeakDC class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!LeakDC class methodsFor: 'instance creation'!
in: in coef: coef |u| u := self new . u setIn: in . u setCoef: coef . ^ u ! !

UGen subclass: #LFPulse
instanceVariableNames: 'freq iphase width' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!LFPulse commentStamp: '<historical>' prior: 0!
pulse oscillator !

!LFPulse methodsFor: 'accessing'!
freq ^freq !
iphase ^iphase !
width ^width !
!

!LFPulse methodsFor: 'private'!
setFreq: x freq := x !
setIphase: x iphase := x !
setWidth: x width := x !
!

!LFPulse methodsFor: 'private'!
initialize name := #LFPulse . paramNames := #(freq iphase width) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!LFPulse class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!LFPulse class methodsFor: 'instance creation'!
freq: freq iphase: iphase width: width |u| u := self new . u setFreq: freq . u setIphase: iphase . u setWidth: width . ^ u ! !

UGen subclass: #LFSaw
instanceVariableNames: 'freq iphase' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!LFSaw commentStamp: '<historical>' prior: 0!
Sawtooth oscillator !

!LFSaw methodsFor: 'accessing'!
freq ^freq !
iphase ^iphase !
!

!LFSaw methodsFor: 'private'!
setFreq: x freq := x !
setIphase: x iphase := x !
!

!LFSaw methodsFor: 'private'!
initialize name := #LFSaw . paramNames := #(freq iphase) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!LFSaw class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!LFSaw class methodsFor: 'instance creation'!
freq: freq iphase: iphase |u| u := self new . u setFreq: freq . u setIphase: iphase . ^ u ! !

UGen subclass: #LPF
instanceVariableNames: 'in freq' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!LPF commentStamp: '<historical>' prior: 0!
2nd order Butterworth lowpass filter !

!LPF methodsFor: 'accessing'!
in ^in !
freq ^freq !
!

!LPF methodsFor: 'private'!
setIn: x in := x !
setFreq: x freq := x !
!

!LPF methodsFor: 'private'!
initialize name := #LPF . paramNames := #(in freq) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!LPF class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!LPF class methodsFor: 'instance creation'!
in: in freq: freq |u| u := self new . u setIn: in . u setFreq: freq . ^ u ! !

UGen subclass: #MouseX
instanceVariableNames: 'minval maxval warp lag' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!MouseX commentStamp: '<historical>' prior: 0!
Cursor tracking UGen. !

!MouseX methodsFor: 'accessing'!
minval ^minval !
maxval ^maxval !
warp ^warp !
lag ^lag !
!

!MouseX methodsFor: 'private'!
setMinval: x minval := x !
setMaxval: x maxval := x !
setWarp: x warp := x !
setLag: x lag := x !
!

!MouseX methodsFor: 'private'!
initialize name := #MouseX . paramNames := #(minval maxval warp lag) . mceNames := #() . rate := #kr . numChan := 1 . specialIndex := 0 ! !

!MouseX class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!MouseX class methodsFor: 'instance creation'!
minval: minval maxval: maxval warp: warp lag: lag |u| u := self new . u setMinval: minval . u setMaxval: maxval . u setWarp: warp . u setLag: lag . ^ u ! !

UGen subclass: #MouseY
instanceVariableNames: 'minval maxval warp lag' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!MouseY commentStamp: '<historical>' prior: 0!
Cursor tracking UGen. !

!MouseY methodsFor: 'accessing'!
minval ^minval !
maxval ^maxval !
warp ^warp !
lag ^lag !
!

!MouseY methodsFor: 'private'!
setMinval: x minval := x !
setMaxval: x maxval := x !
setWarp: x warp := x !
setLag: x lag := x !
!

!MouseY methodsFor: 'private'!
initialize name := #MouseY . paramNames := #(minval maxval warp lag) . mceNames := #() . rate := #kr . numChan := 1 . specialIndex := 0 ! !

!MouseY class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!MouseY class methodsFor: 'instance creation'!
minval: minval maxval: maxval warp: warp lag: lag |u| u := self new . u setMinval: minval . u setMaxval: maxval . u setWarp: warp . u setLag: lag . ^ u ! !

UGen subclass: #MulAdd
instanceVariableNames: 'in mul add' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!MulAdd commentStamp: '<historical>' prior: 0!
Multiply add !

!MulAdd methodsFor: 'accessing'!
in ^in !
mul ^mul !
add ^add !
!

!MulAdd methodsFor: 'private'!
setIn: x in := x !
setMul: x mul := x !
setAdd: x add := x !
!

!MulAdd methodsFor: 'private'!
initialize name := #MulAdd . paramNames := #(in mul add) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!MulAdd class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!MulAdd class methodsFor: 'instance creation'!
in: in mul: mul add: add |u| u := self new . u setIn: in . u setMul: mul . u setAdd: add . ^ u ! !

UGen subclass: #OnePole
instanceVariableNames: 'in coef' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!OnePole commentStamp: '<historical>' prior: 0!
One pole filter. !

!OnePole methodsFor: 'accessing'!
in ^in !
coef ^coef !
!

!OnePole methodsFor: 'private'!
setIn: x in := x !
setCoef: x coef := x !
!

!OnePole methodsFor: 'private'!
initialize name := #OnePole . paramNames := #(in coef) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!OnePole class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!OnePole class methodsFor: 'instance creation'!
in: in coef: coef |u| u := self new . u setIn: in . u setCoef: coef . ^ u ! !

UGen subclass: #Out
instanceVariableNames: 'bus channelsArray' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!Out commentStamp: '<historical>' prior: 0!
Write a signal to a bus. !

!Out methodsFor: 'accessing'!
bus ^bus !
channelsArray ^channelsArray !
!

!Out methodsFor: 'private'!
setBus: x bus := x !
setChannelsArray: x channelsArray := x !
!

!Out methodsFor: 'private'!
initialize name := #Out . paramNames := #(bus) . mceNames := #(channelsArray) . rate := #ar . numChan := 0 . specialIndex := 0 ! !

!Out class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Out class methodsFor: 'instance creation'!
bus: bus channelsArray: channelsArray |u| u := self new . u setBus: bus . u setChannelsArray: channelsArray . ^ u ! !

UGen subclass: #Pan2
instanceVariableNames: 'in pos level' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!Pan2 commentStamp: '<historical>' prior: 0!
Two channel equal power pan. !

!Pan2 methodsFor: 'accessing'!
in ^in !
pos ^pos !
level ^level !
!

!Pan2 methodsFor: 'private'!
setIn: x in := x !
setPos: x pos := x !
setLevel: x level := x !
!

!Pan2 methodsFor: 'private'!
initialize name := #Pan2 . paramNames := #(in pos level) . mceNames := #() . rate := #ar . numChan := 2 . specialIndex := 0 ! !

!Pan2 class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Pan2 class methodsFor: 'instance creation'!
in: in pos: pos level: level |u| u := self new . u setIn: in . u setPos: pos . u setLevel: level . ^ u ! !

UGen subclass: #PinkNoise
instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!PinkNoise commentStamp: '<historical>' prior: 0!
Pink Noise. !



!PinkNoise methodsFor: 'private'!
initialize name := #PinkNoise . paramNames := #() . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!PinkNoise class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !


UGen subclass: #RHPF
instanceVariableNames: 'in freq rq' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!RHPF commentStamp: '<historical>' prior: 0!
A resonant high pass filter. !

!RHPF methodsFor: 'accessing'!
in ^in !
freq ^freq !
rq ^rq !
!

!RHPF methodsFor: 'private'!
setIn: x in := x !
setFreq: x freq := x !
setRq: x rq := x !
!

!RHPF methodsFor: 'private'!
initialize name := #RHPF . paramNames := #(in freq rq) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!RHPF class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!RHPF class methodsFor: 'instance creation'!
in: in freq: freq rq: rq |u| u := self new . u setIn: in . u setFreq: freq . u setRq: rq . ^ u ! !

UGen subclass: #RLPF
instanceVariableNames: 'in freq rq' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!RLPF commentStamp: '<historical>' prior: 0!
A resonant low pass filter. !

!RLPF methodsFor: 'accessing'!
in ^in !
freq ^freq !
rq ^rq !
!

!RLPF methodsFor: 'private'!
setIn: x in := x !
setFreq: x freq := x !
setRq: x rq := x !
!

!RLPF methodsFor: 'private'!
initialize name := #RLPF . paramNames := #(in freq rq) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!RLPF class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!RLPF class methodsFor: 'instance creation'!
in: in freq: freq rq: rq |u| u := self new . u setIn: in . u setFreq: freq . u setRq: rq . ^ u ! !

UGen subclass: #SinOsc
instanceVariableNames: 'freq phase' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!SinOsc commentStamp: '<historical>' prior: 0!
Interpolating sine wavetable oscillator. !

!SinOsc methodsFor: 'accessing'!
freq ^freq !
phase ^phase !
!

!SinOsc methodsFor: 'private'!
setFreq: x freq := x !
setPhase: x phase := x !
!

!SinOsc methodsFor: 'private'!
initialize name := #SinOsc . paramNames := #(freq phase) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!SinOsc class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!SinOsc class methodsFor: 'instance creation'!
freq: freq phase: phase |u| u := self new . u setFreq: freq . u setPhase: phase . ^ u ! !

UGen subclass: #WhiteNoise
instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!WhiteNoise commentStamp: '<historical>' prior: 0!
White noise. !



!WhiteNoise methodsFor: 'private'!
initialize name := #WhiteNoise . paramNames := #() . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!WhiteNoise class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !



!UGen methodsFor: 'arithmetic'!
+ b ^ BinaryOpUGen specialIndex: 0 a: self b: b !
- b ^ BinaryOpUGen specialIndex: 1 a: self b: b !
* b ^ BinaryOpUGen specialIndex: 2 a: self b: b !
/ b ^ BinaryOpUGen specialIndex: 4 a: self b: b !
% b ^ BinaryOpUGen specialIndex: 5 a: self b: b !
/= b ^ BinaryOpUGen specialIndex: 7 a: self b: b !
< b ^ BinaryOpUGen specialIndex: 8 a: self b: b !
> b ^ BinaryOpUGen specialIndex: 9 a: self b: b !
<= b ^ BinaryOpUGen specialIndex: 10 a: self b: b !
>= b ^ BinaryOpUGen specialIndex: 11 a: self b: b !
min: b ^ BinaryOpUGen specialIndex: 12 a: self b: b !
max: b ^ BinaryOpUGen specialIndex: 13 a: self b: b !
** b ^ BinaryOpUGen specialIndex: 25 a: self b: b !
clip2: b ^ BinaryOpUGen specialIndex: 42 a: self b: b !
!

!UGen methodsFor: 'arithmetic'!
negated ^ UnaryOpUGen specialIndex: 0 a: self !
abs ^ UnaryOpUGen specialIndex: 5 a: self !
cubed ^ UnaryOpUGen specialIndex: 13 a: self !
reciprocal ^ UnaryOpUGen specialIndex: 16 a: self !
midicps ^ UnaryOpUGen specialIndex: 17 a: self !
!

