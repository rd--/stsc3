UGen subclass: #AllpassN
instanceVariableNames: 'in maxdelaytime delaytime decaytime' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!AllpassN commentStamp: '<historical>' prior: 0!
Schroeder allpass delay line with no interpolation. !

!AllpassN methodsFor: 'accessing'!
in ^in !
maxdelaytime ^maxdelaytime !
delaytime ^delaytime !
decaytime ^decaytime !
!

!AllpassN methodsFor: 'private'!
setIn: x in := x !
setMaxdelaytime: x maxdelaytime := x !
setDelaytime: x delaytime := x !
setDecaytime: x decaytime := x !
!

!AllpassN methodsFor: 'private'!
initialize name := #AllpassN . paramNames := #(in maxdelaytime delaytime decaytime) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!AllpassN class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!AllpassN class methodsFor: 'instance creation'!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime |u| u := self new . u setIn: in . u setMaxdelaytime: maxdelaytime . u setDelaytime: delaytime . u setDecaytime: decaytime . ^ u !
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul |u| u := self new . u setIn: in . u setMaxdelaytime: maxdelaytime . u setDelaytime: delaytime . u setDecaytime: decaytime . ^ u * mul !
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add |u| u := self new . u setIn: in . u setMaxdelaytime: maxdelaytime . u setDelaytime: delaytime . u setDecaytime: decaytime . ^ u * mul + add !
!

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
in: in attackTime: attackTime releaseTime: releaseTime |u| u := self new . u setIn: in . u setAttackTime: attackTime . u setReleaseTime: releaseTime . ^ u !
in: in attackTime: attackTime releaseTime: releaseTime mul: mul |u| u := self new . u setIn: in . u setAttackTime: attackTime . u setReleaseTime: releaseTime . ^ u * mul !
in: in attackTime: attackTime releaseTime: releaseTime mul: mul add: add |u| u := self new . u setIn: in . u setAttackTime: attackTime . u setReleaseTime: releaseTime . ^ u * mul + add !
!

UGen subclass: #BrownNoise
instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!BrownNoise commentStamp: '<historical>' prior: 0!
Brown Noise. !



!BrownNoise methodsFor: 'private'!
initialize name := #BrownNoise . paramNames := #() . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!BrownNoise class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!BrownNoise class methodsFor: 'instance creation'!
 mul: mul |u| u := self new .  ^ u * mul !
 mul: mul add: add |u| u := self new .  ^ u * mul + add !
!

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
in: in |u| u := self new . u setIn: in . ^ u !
in: in mul: mul |u| u := self new . u setIn: in . ^ u * mul !
in: in mul: mul add: add |u| u := self new . u setIn: in . ^ u * mul + add !
!

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
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime |u| u := self new . u setIn: in . u setMaxdelaytime: maxdelaytime . u setDelaytime: delaytime . u setDecaytime: decaytime . ^ u !
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul |u| u := self new . u setIn: in . u setMaxdelaytime: maxdelaytime . u setDelaytime: delaytime . u setDecaytime: decaytime . ^ u * mul !
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add |u| u := self new . u setIn: in . u setMaxdelaytime: maxdelaytime . u setDelaytime: delaytime . u setDecaytime: decaytime . ^ u * mul + add !
!

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
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime |u| u := self new . u setIn: in . u setMaxdelaytime: maxdelaytime . u setDelaytime: delaytime . u setDecaytime: decaytime . ^ u !
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul |u| u := self new . u setIn: in . u setMaxdelaytime: maxdelaytime . u setDelaytime: delaytime . u setDecaytime: decaytime . ^ u * mul !
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add |u| u := self new . u setIn: in . u setMaxdelaytime: maxdelaytime . u setDelaytime: delaytime . u setDecaytime: decaytime . ^ u * mul + add !
!

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
in: in decayTime: decayTime |u| u := self new . u setIn: in . u setDecayTime: decayTime . ^ u !
in: in decayTime: decayTime mul: mul |u| u := self new . u setIn: in . u setDecayTime: decayTime . ^ u * mul !
in: in decayTime: decayTime mul: mul add: add |u| u := self new . u setIn: in . u setDecayTime: decayTime . ^ u * mul + add !
!

UGen subclass: #Decay2
instanceVariableNames: 'in attackTime decayTime' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!Decay2 commentStamp: '<historical>' prior: 0!
Exponential decay !

!Decay2 methodsFor: 'accessing'!
in ^in !
attackTime ^attackTime !
decayTime ^decayTime !
!

!Decay2 methodsFor: 'private'!
setIn: x in := x !
setAttackTime: x attackTime := x !
setDecayTime: x decayTime := x !
!

!Decay2 methodsFor: 'private'!
initialize name := #Decay2 . paramNames := #(in attackTime decayTime) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!Decay2 class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Decay2 class methodsFor: 'instance creation'!
in: in attackTime: attackTime decayTime: decayTime |u| u := self new . u setIn: in . u setAttackTime: attackTime . u setDecayTime: decayTime . ^ u !
in: in attackTime: attackTime decayTime: decayTime mul: mul |u| u := self new . u setIn: in . u setAttackTime: attackTime . u setDecayTime: decayTime . ^ u * mul !
in: in attackTime: attackTime decayTime: decayTime mul: mul add: add |u| u := self new . u setIn: in . u setAttackTime: attackTime . u setDecayTime: decayTime . ^ u * mul + add !
!

UGen subclass: #DegreeToKey
instanceVariableNames: 'bufnum in octave' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!DegreeToKey commentStamp: '<historical>' prior: 0!
Convert signal to modal pitch. !

!DegreeToKey methodsFor: 'accessing'!
bufnum ^bufnum !
in ^in !
octave ^octave !
!

!DegreeToKey methodsFor: 'private'!
setBufnum: x bufnum := x !
setIn: x in := x !
setOctave: x octave := x !
!

!DegreeToKey methodsFor: 'private'!
initialize name := #DegreeToKey . paramNames := #(bufnum in octave) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!DegreeToKey class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!DegreeToKey class methodsFor: 'instance creation'!
bufnum: bufnum in: in octave: octave |u| u := self new . u setBufnum: bufnum . u setIn: in . u setOctave: octave . ^ u !
bufnum: bufnum in: in octave: octave mul: mul |u| u := self new . u setBufnum: bufnum . u setIn: in . u setOctave: octave . ^ u * mul !
bufnum: bufnum in: in octave: octave mul: mul add: add |u| u := self new . u setBufnum: bufnum . u setIn: in . u setOctave: octave . ^ u * mul + add !
!

UGen subclass: #DelayN
instanceVariableNames: 'in maxdelaytime delaytime' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!DelayN commentStamp: '<historical>' prior: 0!
Simple delay line with no interpolation. !

!DelayN methodsFor: 'accessing'!
in ^in !
maxdelaytime ^maxdelaytime !
delaytime ^delaytime !
!

!DelayN methodsFor: 'private'!
setIn: x in := x !
setMaxdelaytime: x maxdelaytime := x !
setDelaytime: x delaytime := x !
!

!DelayN methodsFor: 'private'!
initialize name := #DelayN . paramNames := #(in maxdelaytime delaytime) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!DelayN class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!DelayN class methodsFor: 'instance creation'!
in: in maxdelaytime: maxdelaytime delaytime: delaytime |u| u := self new . u setIn: in . u setMaxdelaytime: maxdelaytime . u setDelaytime: delaytime . ^ u !
in: in maxdelaytime: maxdelaytime delaytime: delaytime mul: mul |u| u := self new . u setIn: in . u setMaxdelaytime: maxdelaytime . u setDelaytime: delaytime . ^ u * mul !
in: in maxdelaytime: maxdelaytime delaytime: delaytime mul: mul add: add |u| u := self new . u setIn: in . u setMaxdelaytime: maxdelaytime . u setDelaytime: delaytime . ^ u * mul + add !
!

UGen subclass: #Demand
instanceVariableNames: 'trig reset demandUGens' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!Demand commentStamp: '<historical>' prior: 0!
Demand results from demand rate UGens. !

!Demand methodsFor: 'accessing'!
trig ^trig !
reset ^reset !
demandUGens ^demandUGens !
!

!Demand methodsFor: 'private'!
setTrig: x trig := x !
setReset: x reset := x !
setDemandUGens: x demandUGens := x !
!

!Demand methodsFor: 'private'!
initialize name := #Demand . paramNames := #(trig reset) . mceNames := #(demandUGens) . rate := #ar . numChan := nil . specialIndex := 0 ! !

!Demand class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Demand class methodsFor: 'instance creation'!
trig: trig reset: reset demandUGens: demandUGens |u| u := self new . u setNumChan: #mceInput .  u setTrig: trig . u setReset: reset . u setDemandUGens: demandUGens . ^ u !
trig: trig reset: reset demandUGens: demandUGens mul: mul |u| u := self new . u setNumChan: #mceInput .  u setTrig: trig . u setReset: reset . u setDemandUGens: demandUGens . ^ u * mul !
trig: trig reset: reset demandUGens: demandUGens mul: mul add: add |u| u := self new . u setNumChan: #mceInput .  u setTrig: trig . u setReset: reset . u setDemandUGens: demandUGens . ^ u * mul + add !
!

UGen subclass: #Drand
instanceVariableNames: 'repeats list' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!Drand commentStamp: '<historical>' prior: 0!
Demand rate random sequence generator. !

!Drand methodsFor: 'accessing'!
repeats ^repeats !
list ^list !
!

!Drand methodsFor: 'private'!
setRepeats: x repeats := x !
setList: x list := x !
!

!Drand methodsFor: 'private'!
initialize name := #Drand . paramNames := #(repeats) . mceNames := #(list) . rate := #dr . numChan := 1 . specialIndex := 0 ! !

!Drand class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Drand class methodsFor: 'instance creation'!
repeats: repeats list: list |u| u := self new . u setRepeats: repeats . u setList: list . ^ u !
repeats: repeats list: list mul: mul |u| u := self new . u setRepeats: repeats . u setList: list . ^ u * mul !
repeats: repeats list: list mul: mul add: add |u| u := self new . u setRepeats: repeats . u setList: list . ^ u * mul + add !
!

UGen subclass: #Dseq
instanceVariableNames: 'repeats list' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!Dseq commentStamp: '<historical>' prior: 0!
Demand rate sequence generator. !

!Dseq methodsFor: 'accessing'!
repeats ^repeats !
list ^list !
!

!Dseq methodsFor: 'private'!
setRepeats: x repeats := x !
setList: x list := x !
!

!Dseq methodsFor: 'private'!
initialize name := #Dseq . paramNames := #(repeats) . mceNames := #(list) . rate := #dr . numChan := 1 . specialIndex := 0 ! !

!Dseq class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Dseq class methodsFor: 'instance creation'!
repeats: repeats list: list |u| u := self new . u setRepeats: repeats . u setList: list . ^ u !
repeats: repeats list: list mul: mul |u| u := self new . u setRepeats: repeats . u setList: list . ^ u * mul !
repeats: repeats list: list mul: mul add: add |u| u := self new . u setRepeats: repeats . u setList: list . ^ u * mul + add !
!

UGen subclass: #Dust
instanceVariableNames: 'density' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!Dust commentStamp: '<historical>' prior: 0!
Random impulses. !

!Dust methodsFor: 'accessing'!
density ^density !
!

!Dust methodsFor: 'private'!
setDensity: x density := x !
!

!Dust methodsFor: 'private'!
initialize name := #Dust . paramNames := #(density) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!Dust class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Dust class methodsFor: 'instance creation'!
density: density |u| u := self new . u setDensity: density . ^ u !
density: density mul: mul |u| u := self new . u setDensity: density . ^ u * mul !
density: density mul: mul add: add |u| u := self new . u setDensity: density . ^ u * mul + add !
!

UGen subclass: #EnvGen
instanceVariableNames: 'gate levelScale levelBias timeScale doneAction envelope' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!EnvGen commentStamp: '<historical>' prior: 0!
Envelope generator !

!EnvGen methodsFor: 'accessing'!
gate ^gate !
levelScale ^levelScale !
levelBias ^levelBias !
timeScale ^timeScale !
doneAction ^doneAction !
envelope ^envelope !
!

!EnvGen methodsFor: 'private'!
setGate: x gate := x !
setLevelScale: x levelScale := x !
setLevelBias: x levelBias := x !
setTimeScale: x timeScale := x !
setDoneAction: x doneAction := x !
setEnvelope: x envelope := x !
!

!EnvGen methodsFor: 'private'!
initialize name := #EnvGen . paramNames := #(gate levelScale levelBias timeScale doneAction) . mceNames := #(envelope) . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!EnvGen class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!EnvGen class methodsFor: 'instance creation'!
gate: gate levelScale: levelScale levelBias: levelBias timeScale: timeScale doneAction: doneAction envelope: envelope |u| u := self new . u setGate: gate . u setLevelScale: levelScale . u setLevelBias: levelBias . u setTimeScale: timeScale . u setDoneAction: doneAction . u setEnvelope: envelope . ^ u !
gate: gate levelScale: levelScale levelBias: levelBias timeScale: timeScale doneAction: doneAction envelope: envelope mul: mul |u| u := self new . u setGate: gate . u setLevelScale: levelScale . u setLevelBias: levelBias . u setTimeScale: timeScale . u setDoneAction: doneAction . u setEnvelope: envelope . ^ u * mul !
gate: gate levelScale: levelScale levelBias: levelBias timeScale: timeScale doneAction: doneAction envelope: envelope mul: mul add: add |u| u := self new . u setGate: gate . u setLevelScale: levelScale . u setLevelBias: levelBias . u setTimeScale: timeScale . u setDoneAction: doneAction . u setEnvelope: envelope . ^ u * mul + add !
!

UGen subclass: #ExpRand
instanceVariableNames: 'lo hi' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!ExpRand commentStamp: '<historical>' prior: 0!
Exponential single random number generator. !

!ExpRand methodsFor: 'accessing'!
lo ^lo !
hi ^hi !
!

!ExpRand methodsFor: 'private'!
setLo: x lo := x !
setHi: x hi := x !
!

!ExpRand methodsFor: 'private'!
initialize name := #ExpRand . paramNames := #(lo hi) . mceNames := #() . rate := #ir . numChan := 1 . specialIndex := 0 ! !

!ExpRand class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!ExpRand class methodsFor: 'instance creation'!
lo: lo hi: hi |u| u := self new . u setLo: lo . u setHi: hi . ^ u !
lo: lo hi: hi mul: mul |u| u := self new . u setLo: lo . u setHi: hi . ^ u * mul !
lo: lo hi: hi mul: mul add: add |u| u := self new . u setLo: lo . u setHi: hi . ^ u * mul + add !
!

UGen subclass: #FSinOsc
instanceVariableNames: 'freq iphase' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!FSinOsc commentStamp: '<historical>' prior: 0!
Fast sine oscillator. !

!FSinOsc methodsFor: 'accessing'!
freq ^freq !
iphase ^iphase !
!

!FSinOsc methodsFor: 'private'!
setFreq: x freq := x !
setIphase: x iphase := x !
!

!FSinOsc methodsFor: 'private'!
initialize name := #FSinOsc . paramNames := #(freq iphase) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!FSinOsc class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!FSinOsc class methodsFor: 'instance creation'!
freq: freq iphase: iphase |u| u := self new . u setFreq: freq . u setIphase: iphase . ^ u !
freq: freq iphase: iphase mul: mul |u| u := self new . u setFreq: freq . u setIphase: iphase . ^ u * mul !
freq: freq iphase: iphase mul: mul add: add |u| u := self new . u setFreq: freq . u setIphase: iphase . ^ u * mul + add !
!

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
in: in |u| u := self new . u setIn: in . ^ u !
in: in mul: mul |u| u := self new . u setIn: in . ^ u * mul !
in: in mul: mul add: add |u| u := self new . u setIn: in . ^ u * mul + add !
!

UGen subclass: #Impulse
instanceVariableNames: 'freq phase' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!Impulse commentStamp: '<historical>' prior: 0!
Impulse oscillator. !

!Impulse methodsFor: 'accessing'!
freq ^freq !
phase ^phase !
!

!Impulse methodsFor: 'private'!
setFreq: x freq := x !
setPhase: x phase := x !
!

!Impulse methodsFor: 'private'!
initialize name := #Impulse . paramNames := #(freq phase) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!Impulse class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Impulse class methodsFor: 'instance creation'!
freq: freq phase: phase |u| u := self new . u setFreq: freq . u setPhase: phase . ^ u !
freq: freq phase: phase mul: mul |u| u := self new . u setFreq: freq . u setPhase: phase . ^ u * mul !
freq: freq phase: phase mul: mul add: add |u| u := self new . u setFreq: freq . u setPhase: phase . ^ u * mul + add !
!

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
initialize name := #In . paramNames := #(bus) . mceNames := #() . rate := #ar . numChan := nil . specialIndex := 0 ! !

!In class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!In class methodsFor: 'instance creation'!
numChan: numChan bus: bus |u| u := self new . u setNumChan: numChan . u setBus: bus . ^ u !
numChan: numChan bus: bus mul: mul |u| u := self new . u setNumChan: numChan . u setBus: bus . ^ u * mul !
numChan: numChan bus: bus mul: mul add: add |u| u := self new . u setNumChan: numChan . u setBus: bus . ^ u * mul + add !
!

UGen subclass: #IRand
instanceVariableNames: 'lo hi' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!IRand commentStamp: '<historical>' prior: 0!
Single integer random number generator. !

!IRand methodsFor: 'accessing'!
lo ^lo !
hi ^hi !
!

!IRand methodsFor: 'private'!
setLo: x lo := x !
setHi: x hi := x !
!

!IRand methodsFor: 'private'!
initialize name := #IRand . paramNames := #(lo hi) . mceNames := #() . rate := #ir . numChan := 1 . specialIndex := 0 ! !

!IRand class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!IRand class methodsFor: 'instance creation'!
lo: lo hi: hi |u| u := self new . u setLo: lo . u setHi: hi . ^ u !
lo: lo hi: hi mul: mul |u| u := self new . u setLo: lo . u setHi: hi . ^ u * mul !
lo: lo hi: hi mul: mul add: add |u| u := self new . u setLo: lo . u setHi: hi . ^ u * mul + add !
!

UGen subclass: #Klank
instanceVariableNames: 'input freqscale freqoffset decayscale specificationsArrayRef' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!Klank commentStamp: '<historical>' prior: 0!
Bank of resonators !

!Klank methodsFor: 'accessing'!
input ^input !
freqscale ^freqscale !
freqoffset ^freqoffset !
decayscale ^decayscale !
specificationsArrayRef ^specificationsArrayRef !
!

!Klank methodsFor: 'private'!
setInput: x input := x !
setFreqscale: x freqscale := x !
setFreqoffset: x freqoffset := x !
setDecayscale: x decayscale := x !
setSpecificationsArrayRef: x specificationsArrayRef := x !
!

!Klank methodsFor: 'private'!
initialize name := #Klank . paramNames := #(input freqscale freqoffset decayscale) . mceNames := #(specificationsArrayRef) . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!Klank class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Klank class methodsFor: 'instance creation'!
input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef |u| u := self new . u setInput: input . u setFreqscale: freqscale . u setFreqoffset: freqoffset . u setDecayscale: decayscale . u setSpecificationsArrayRef: specificationsArrayRef . ^ u !
input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef mul: mul |u| u := self new . u setInput: input . u setFreqscale: freqscale . u setFreqoffset: freqoffset . u setDecayscale: decayscale . u setSpecificationsArrayRef: specificationsArrayRef . ^ u * mul !
input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef mul: mul add: add |u| u := self new . u setInput: input . u setFreqscale: freqscale . u setFreqoffset: freqoffset . u setDecayscale: decayscale . u setSpecificationsArrayRef: specificationsArrayRef . ^ u * mul + add !
!

UGen subclass: #Lag
instanceVariableNames: 'in lagTime' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!Lag commentStamp: '<historical>' prior: 0!
Exponential lag !

!Lag methodsFor: 'accessing'!
in ^in !
lagTime ^lagTime !
!

!Lag methodsFor: 'private'!
setIn: x in := x !
setLagTime: x lagTime := x !
!

!Lag methodsFor: 'private'!
initialize name := #Lag . paramNames := #(in lagTime) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!Lag class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Lag class methodsFor: 'instance creation'!
in: in lagTime: lagTime |u| u := self new . u setIn: in . u setLagTime: lagTime . ^ u !
in: in lagTime: lagTime mul: mul |u| u := self new . u setIn: in . u setLagTime: lagTime . ^ u * mul !
in: in lagTime: lagTime mul: mul add: add |u| u := self new . u setIn: in . u setLagTime: lagTime . ^ u * mul + add !
!

UGen subclass: #Latch
instanceVariableNames: 'in trig' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!Latch commentStamp: '<historical>' prior: 0!
Sample and hold !

!Latch methodsFor: 'accessing'!
in ^in !
trig ^trig !
!

!Latch methodsFor: 'private'!
setIn: x in := x !
setTrig: x trig := x !
!

!Latch methodsFor: 'private'!
initialize name := #Latch . paramNames := #(in trig) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!Latch class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Latch class methodsFor: 'instance creation'!
in: in trig: trig |u| u := self new . u setIn: in . u setTrig: trig . ^ u !
in: in trig: trig mul: mul |u| u := self new . u setIn: in . u setTrig: trig . ^ u * mul !
in: in trig: trig mul: mul add: add |u| u := self new . u setIn: in . u setTrig: trig . ^ u * mul + add !
!

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
in: in coef: coef |u| u := self new . u setIn: in . u setCoef: coef . ^ u !
in: in coef: coef mul: mul |u| u := self new . u setIn: in . u setCoef: coef . ^ u * mul !
in: in coef: coef mul: mul add: add |u| u := self new . u setIn: in . u setCoef: coef . ^ u * mul + add !
!

UGen subclass: #LFNoise1
instanceVariableNames: 'freq' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!LFNoise1 commentStamp: '<historical>' prior: 0!
Ramp noise !

!LFNoise1 methodsFor: 'accessing'!
freq ^freq !
!

!LFNoise1 methodsFor: 'private'!
setFreq: x freq := x !
!

!LFNoise1 methodsFor: 'private'!
initialize name := #LFNoise1 . paramNames := #(freq) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!LFNoise1 class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!LFNoise1 class methodsFor: 'instance creation'!
freq: freq |u| u := self new . u setFreq: freq . ^ u !
freq: freq mul: mul |u| u := self new . u setFreq: freq . ^ u * mul !
freq: freq mul: mul add: add |u| u := self new . u setFreq: freq . ^ u * mul + add !
!

UGen subclass: #LFNoise2
instanceVariableNames: 'freq' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!LFNoise2 commentStamp: '<historical>' prior: 0!
Quadratic noise. !

!LFNoise2 methodsFor: 'accessing'!
freq ^freq !
!

!LFNoise2 methodsFor: 'private'!
setFreq: x freq := x !
!

!LFNoise2 methodsFor: 'private'!
initialize name := #LFNoise2 . paramNames := #(freq) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!LFNoise2 class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!LFNoise2 class methodsFor: 'instance creation'!
freq: freq |u| u := self new . u setFreq: freq . ^ u !
freq: freq mul: mul |u| u := self new . u setFreq: freq . ^ u * mul !
freq: freq mul: mul add: add |u| u := self new . u setFreq: freq . ^ u * mul + add !
!

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
freq: freq iphase: iphase width: width |u| u := self new . u setFreq: freq . u setIphase: iphase . u setWidth: width . ^ u !
freq: freq iphase: iphase width: width mul: mul |u| u := self new . u setFreq: freq . u setIphase: iphase . u setWidth: width . ^ u * mul !
freq: freq iphase: iphase width: width mul: mul add: add |u| u := self new . u setFreq: freq . u setIphase: iphase . u setWidth: width . ^ u * mul + add !
!

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
freq: freq iphase: iphase |u| u := self new . u setFreq: freq . u setIphase: iphase . ^ u !
freq: freq iphase: iphase mul: mul |u| u := self new . u setFreq: freq . u setIphase: iphase . ^ u * mul !
freq: freq iphase: iphase mul: mul add: add |u| u := self new . u setFreq: freq . u setIphase: iphase . ^ u * mul + add !
!

UGen subclass: #Line
instanceVariableNames: 'start end dur doneAction' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!Line commentStamp: '<historical>' prior: 0!
Line generator. !

!Line methodsFor: 'accessing'!
start ^start !
end ^end !
dur ^dur !
doneAction ^doneAction !
!

!Line methodsFor: 'private'!
setStart: x start := x !
setEnd: x end := x !
setDur: x dur := x !
setDoneAction: x doneAction := x !
!

!Line methodsFor: 'private'!
initialize name := #Line . paramNames := #(start end dur doneAction) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!Line class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Line class methodsFor: 'instance creation'!
start: start end: end dur: dur doneAction: doneAction |u| u := self new . u setStart: start . u setEnd: end . u setDur: dur . u setDoneAction: doneAction . ^ u !
start: start end: end dur: dur doneAction: doneAction mul: mul |u| u := self new . u setStart: start . u setEnd: end . u setDur: dur . u setDoneAction: doneAction . ^ u * mul !
start: start end: end dur: dur doneAction: doneAction mul: mul add: add |u| u := self new . u setStart: start . u setEnd: end . u setDur: dur . u setDoneAction: doneAction . ^ u * mul + add !
!

UGen subclass: #LocalBuf
instanceVariableNames: 'numChannels numFrames' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!LocalBuf commentStamp: '<historical>' prior: 0!
Allocate a buffer local to the synth !

!LocalBuf methodsFor: 'accessing'!
numChannels ^numChannels !
numFrames ^numFrames !
!

!LocalBuf methodsFor: 'private'!
setNumChannels: x numChannels := x !
setNumFrames: x numFrames := x !
!

!LocalBuf methodsFor: 'private'!
initialize name := #LocalBuf . paramNames := #(numChannels numFrames) . mceNames := #() . rate := #ir . numChan := 1 . specialIndex := 0 ! !

!LocalBuf class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!LocalBuf class methodsFor: 'instance creation'!
numChannels: numChannels numFrames: numFrames |u| u := self new . u setNumChannels: numChannels . u setNumFrames: numFrames . ^ u !
numChannels: numChannels numFrames: numFrames mul: mul |u| u := self new . u setNumChannels: numChannels . u setNumFrames: numFrames . ^ u * mul !
numChannels: numChannels numFrames: numFrames mul: mul add: add |u| u := self new . u setNumChannels: numChannels . u setNumFrames: numFrames . ^ u * mul + add !
!

UGen subclass: #LocalIn
instanceVariableNames: 'default' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!LocalIn commentStamp: '<historical>' prior: 0!
Define and read from buses local to a synth. !

!LocalIn methodsFor: 'accessing'!
default ^default !
!

!LocalIn methodsFor: 'private'!
setDefault: x default := x !
!

!LocalIn methodsFor: 'private'!
initialize name := #LocalIn . paramNames := #() . mceNames := #(default) . rate := #ar . numChan := nil . specialIndex := 0 ! !

!LocalIn class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!LocalIn class methodsFor: 'instance creation'!
numChan: numChan default: default |u| u := self new . u setNumChan: numChan . u setDefault: default . ^ u !
numChan: numChan default: default mul: mul |u| u := self new . u setNumChan: numChan . u setDefault: default . ^ u * mul !
numChan: numChan default: default mul: mul add: add |u| u := self new . u setNumChan: numChan . u setDefault: default . ^ u * mul + add !
!

UGen subclass: #LocalOut
instanceVariableNames: 'channelsArray' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!LocalOut commentStamp: '<historical>' prior: 0!
Write to buses local to a synth. !

!LocalOut methodsFor: 'accessing'!
channelsArray ^channelsArray !
!

!LocalOut methodsFor: 'private'!
setChannelsArray: x channelsArray := x !
!

!LocalOut methodsFor: 'private'!
initialize name := #LocalOut . paramNames := #() . mceNames := #(channelsArray) . rate := #ar . numChan := 0 . specialIndex := 0 ! !

!LocalOut class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!LocalOut class methodsFor: 'instance creation'!
channelsArray: channelsArray |u| u := self new . u setChannelsArray: channelsArray . ^ u !
channelsArray: channelsArray mul: mul |u| u := self new . u setChannelsArray: channelsArray . ^ u * mul !
channelsArray: channelsArray mul: mul add: add |u| u := self new . u setChannelsArray: channelsArray . ^ u * mul + add !
!

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
in: in freq: freq |u| u := self new . u setIn: in . u setFreq: freq . ^ u !
in: in freq: freq mul: mul |u| u := self new . u setIn: in . u setFreq: freq . ^ u * mul !
in: in freq: freq mul: mul add: add |u| u := self new . u setIn: in . u setFreq: freq . ^ u * mul + add !
!

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
minval: minval maxval: maxval warp: warp lag: lag |u| u := self new . u setMinval: minval . u setMaxval: maxval . u setWarp: warp . u setLag: lag . ^ u !
minval: minval maxval: maxval warp: warp lag: lag mul: mul |u| u := self new . u setMinval: minval . u setMaxval: maxval . u setWarp: warp . u setLag: lag . ^ u * mul !
minval: minval maxval: maxval warp: warp lag: lag mul: mul add: add |u| u := self new . u setMinval: minval . u setMaxval: maxval . u setWarp: warp . u setLag: lag . ^ u * mul + add !
!

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
minval: minval maxval: maxval warp: warp lag: lag |u| u := self new . u setMinval: minval . u setMaxval: maxval . u setWarp: warp . u setLag: lag . ^ u !
minval: minval maxval: maxval warp: warp lag: lag mul: mul |u| u := self new . u setMinval: minval . u setMaxval: maxval . u setWarp: warp . u setLag: lag . ^ u * mul !
minval: minval maxval: maxval warp: warp lag: lag mul: mul add: add |u| u := self new . u setMinval: minval . u setMaxval: maxval . u setWarp: warp . u setLag: lag . ^ u * mul + add !
!

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
in: in mul: mul add: add |u| u := self new . u setIn: in . u setMul: mul . u setAdd: add . ^ u !
!

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
in: in coef: coef |u| u := self new . u setIn: in . u setCoef: coef . ^ u !
in: in coef: coef mul: mul |u| u := self new . u setIn: in . u setCoef: coef . ^ u * mul !
in: in coef: coef mul: mul add: add |u| u := self new . u setIn: in . u setCoef: coef . ^ u * mul + add !
!

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
bus: bus channelsArray: channelsArray |u| u := self new . u setBus: bus . u setChannelsArray: channelsArray . ^ u !
bus: bus channelsArray: channelsArray mul: mul |u| u := self new . u setBus: bus . u setChannelsArray: channelsArray . ^ u * mul !
bus: bus channelsArray: channelsArray mul: mul add: add |u| u := self new . u setBus: bus . u setChannelsArray: channelsArray . ^ u * mul + add !
!

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
in: in pos: pos level: level |u| u := self new . u setIn: in . u setPos: pos . u setLevel: level . ^ u !
in: in pos: pos level: level mul: mul |u| u := self new . u setIn: in . u setPos: pos . u setLevel: level . ^ u * mul !
in: in pos: pos level: level mul: mul add: add |u| u := self new . u setIn: in . u setPos: pos . u setLevel: level . ^ u * mul + add !
!

UGen subclass: #PinkNoise
instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!PinkNoise commentStamp: '<historical>' prior: 0!
Pink Noise. !



!PinkNoise methodsFor: 'private'!
initialize name := #PinkNoise . paramNames := #() . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!PinkNoise class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!PinkNoise class methodsFor: 'instance creation'!
 mul: mul |u| u := self new .  ^ u * mul !
 mul: mul add: add |u| u := self new .  ^ u * mul + add !
!

UGen subclass: #Rand
instanceVariableNames: 'lo hi' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!Rand commentStamp: '<historical>' prior: 0!
Single random number generator. !

!Rand methodsFor: 'accessing'!
lo ^lo !
hi ^hi !
!

!Rand methodsFor: 'private'!
setLo: x lo := x !
setHi: x hi := x !
!

!Rand methodsFor: 'private'!
initialize name := #Rand . paramNames := #(lo hi) . mceNames := #() . rate := #ir . numChan := 1 . specialIndex := 0 ! !

!Rand class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Rand class methodsFor: 'instance creation'!
lo: lo hi: hi |u| u := self new . u setLo: lo . u setHi: hi . ^ u !
lo: lo hi: hi mul: mul |u| u := self new . u setLo: lo . u setHi: hi . ^ u * mul !
lo: lo hi: hi mul: mul add: add |u| u := self new . u setLo: lo . u setHi: hi . ^ u * mul + add !
!

UGen subclass: #Resonz
instanceVariableNames: 'in freq bwr' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!Resonz commentStamp: '<historical>' prior: 0!
Resonant filter. !

!Resonz methodsFor: 'accessing'!
in ^in !
freq ^freq !
bwr ^bwr !
!

!Resonz methodsFor: 'private'!
setIn: x in := x !
setFreq: x freq := x !
setBwr: x bwr := x !
!

!Resonz methodsFor: 'private'!
initialize name := #Resonz . paramNames := #(in freq bwr) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!Resonz class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Resonz class methodsFor: 'instance creation'!
in: in freq: freq bwr: bwr |u| u := self new . u setIn: in . u setFreq: freq . u setBwr: bwr . ^ u !
in: in freq: freq bwr: bwr mul: mul |u| u := self new . u setIn: in . u setFreq: freq . u setBwr: bwr . ^ u * mul !
in: in freq: freq bwr: bwr mul: mul add: add |u| u := self new . u setIn: in . u setFreq: freq . u setBwr: bwr . ^ u * mul + add !
!

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
in: in freq: freq rq: rq |u| u := self new . u setIn: in . u setFreq: freq . u setRq: rq . ^ u !
in: in freq: freq rq: rq mul: mul |u| u := self new . u setIn: in . u setFreq: freq . u setRq: rq . ^ u * mul !
in: in freq: freq rq: rq mul: mul add: add |u| u := self new . u setIn: in . u setFreq: freq . u setRq: rq . ^ u * mul + add !
!

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
in: in freq: freq rq: rq |u| u := self new . u setIn: in . u setFreq: freq . u setRq: rq . ^ u !
in: in freq: freq rq: rq mul: mul |u| u := self new . u setIn: in . u setFreq: freq . u setRq: rq . ^ u * mul !
in: in freq: freq rq: rq mul: mul add: add |u| u := self new . u setIn: in . u setFreq: freq . u setRq: rq . ^ u * mul + add !
!

UGen subclass: #Saw
instanceVariableNames: 'freq' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!Saw commentStamp: '<historical>' prior: 0!
Band limited sawtooth. !

!Saw methodsFor: 'accessing'!
freq ^freq !
!

!Saw methodsFor: 'private'!
setFreq: x freq := x !
!

!Saw methodsFor: 'private'!
initialize name := #Saw . paramNames := #(freq) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!Saw class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Saw class methodsFor: 'instance creation'!
freq: freq |u| u := self new . u setFreq: freq . ^ u !
freq: freq mul: mul |u| u := self new . u setFreq: freq . ^ u * mul !
freq: freq mul: mul add: add |u| u := self new . u setFreq: freq . ^ u * mul + add !
!

UGen subclass: #SetBuf
instanceVariableNames: 'buf offset length array' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!SetBuf commentStamp: '<historical>' prior: 0!
Set local buffer !

!SetBuf methodsFor: 'accessing'!
buf ^buf !
offset ^offset !
length ^length !
array ^array !
!

!SetBuf methodsFor: 'private'!
setBuf: x buf := x !
setOffset: x offset := x !
setLength: x length := x !
setArray: x array := x !
!

!SetBuf methodsFor: 'private'!
initialize name := #SetBuf . paramNames := #(buf offset length) . mceNames := #(array) . rate := #ir . numChan := 1 . specialIndex := 0 ! !

!SetBuf class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!SetBuf class methodsFor: 'instance creation'!
buf: buf offset: offset length: length array: array |u| u := self new . u setBuf: buf . u setOffset: offset . u setLength: length . u setArray: array . ^ u !
buf: buf offset: offset length: length array: array mul: mul |u| u := self new . u setBuf: buf . u setOffset: offset . u setLength: length . u setArray: array . ^ u * mul !
buf: buf offset: offset length: length array: array mul: mul add: add |u| u := self new . u setBuf: buf . u setOffset: offset . u setLength: length . u setArray: array . ^ u * mul + add !
!

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
freq: freq phase: phase |u| u := self new . u setFreq: freq . u setPhase: phase . ^ u !
freq: freq phase: phase mul: mul |u| u := self new . u setFreq: freq . u setPhase: phase . ^ u * mul !
freq: freq phase: phase mul: mul add: add |u| u := self new . u setFreq: freq . u setPhase: phase . ^ u * mul + add !
!

UGen subclass: #Trig
instanceVariableNames: 'in dur' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!Trig commentStamp: '<historical>' prior: 0!
Timed trigger. !

!Trig methodsFor: 'accessing'!
in ^in !
dur ^dur !
!

!Trig methodsFor: 'private'!
setIn: x in := x !
setDur: x dur := x !
!

!Trig methodsFor: 'private'!
initialize name := #Trig . paramNames := #(in dur) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!Trig class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Trig class methodsFor: 'instance creation'!
in: in dur: dur |u| u := self new . u setIn: in . u setDur: dur . ^ u !
in: in dur: dur mul: mul |u| u := self new . u setIn: in . u setDur: dur . ^ u * mul !
in: in dur: dur mul: mul add: add |u| u := self new . u setIn: in . u setDur: dur . ^ u * mul + add !
!

UGen subclass: #XLine
instanceVariableNames: 'start end dur doneAction' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!XLine commentStamp: '<historical>' prior: 0!
Exponential line generator. !

!XLine methodsFor: 'accessing'!
start ^start !
end ^end !
dur ^dur !
doneAction ^doneAction !
!

!XLine methodsFor: 'private'!
setStart: x start := x !
setEnd: x end := x !
setDur: x dur := x !
setDoneAction: x doneAction := x !
!

!XLine methodsFor: 'private'!
initialize name := #XLine . paramNames := #(start end dur doneAction) . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!XLine class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!XLine class methodsFor: 'instance creation'!
start: start end: end dur: dur doneAction: doneAction |u| u := self new . u setStart: start . u setEnd: end . u setDur: dur . u setDoneAction: doneAction . ^ u !
start: start end: end dur: dur doneAction: doneAction mul: mul |u| u := self new . u setStart: start . u setEnd: end . u setDur: dur . u setDoneAction: doneAction . ^ u * mul !
start: start end: end dur: dur doneAction: doneAction mul: mul add: add |u| u := self new . u setStart: start . u setEnd: end . u setDur: dur . u setDoneAction: doneAction . ^ u * mul + add !
!

UGen subclass: #WhiteNoise
instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!WhiteNoise commentStamp: '<historical>' prior: 0!
White noise. !



!WhiteNoise methodsFor: 'private'!
initialize name := #WhiteNoise . paramNames := #() . mceNames := #() . rate := #ar . numChan := 1 . specialIndex := 0 ! !

!WhiteNoise class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!WhiteNoise class methodsFor: 'instance creation'!
 mul: mul |u| u := self new .  ^ u * mul !
 mul: mul add: add |u| u := self new .  ^ u * mul + add !
!


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
log ^ UnaryOpUGen specialIndex: 25 a: self !
distort ^ UnaryOpUGen specialIndex: 42 a: self !
!

