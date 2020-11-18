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
in: x in := x !
maxdelaytime: x maxdelaytime := x !
delaytime: x delaytime := x !
decaytime: x decaytime := x !
!

!AllpassN methodsFor: 'private'!
initialize ugenName := #AllpassN . paramNames := #(in maxdelaytime delaytime decaytime) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!AllpassN class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!AllpassN class methodsFor: 'instance creation'!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime |u| u := self new . u in: in . u maxdelaytime: maxdelaytime . u delaytime: delaytime . u decaytime: decaytime . ^ u !
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul |u| u := self new . u in: in . u maxdelaytime: maxdelaytime . u delaytime: delaytime . u decaytime: decaytime . ^ u * mul !
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add |u| u := self new . u in: in . u maxdelaytime: maxdelaytime . u delaytime: delaytime . u decaytime: decaytime . ^ u * mul + add !
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
in: x in := x !
attackTime: x attackTime := x !
releaseTime: x releaseTime := x !
!

!Amplitude methodsFor: 'private'!
initialize ugenName := #Amplitude . paramNames := #(in attackTime releaseTime) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!Amplitude class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Amplitude class methodsFor: 'instance creation'!
in: in attackTime: attackTime releaseTime: releaseTime |u| u := self new . u in: in . u attackTime: attackTime . u releaseTime: releaseTime . ^ u !
in: in attackTime: attackTime releaseTime: releaseTime mul: mul |u| u := self new . u in: in . u attackTime: attackTime . u releaseTime: releaseTime . ^ u * mul !
in: in attackTime: attackTime releaseTime: releaseTime mul: mul add: add |u| u := self new . u in: in . u attackTime: attackTime . u releaseTime: releaseTime . ^ u * mul + add !
!

UGen subclass: #BrownNoise
instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!BrownNoise commentStamp: '<historical>' prior: 0!
Brown Noise. !



!BrownNoise methodsFor: 'private'!
initialize ugenName := #BrownNoise . paramNames := #() . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

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
in: x in := x !
!

!BPZ2 methodsFor: 'private'!
initialize ugenName := #BPZ2 . paramNames := #(in) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!BPZ2 class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!BPZ2 class methodsFor: 'instance creation'!
in: in |u| u := self new . u in: in . ^ u !
in: in mul: mul |u| u := self new . u in: in . ^ u * mul !
in: in mul: mul add: add |u| u := self new . u in: in . ^ u * mul + add !
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
in: x in := x !
maxdelaytime: x maxdelaytime := x !
delaytime: x delaytime := x !
decaytime: x decaytime := x !
!

!CombL methodsFor: 'private'!
initialize ugenName := #CombL . paramNames := #(in maxdelaytime delaytime decaytime) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!CombL class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!CombL class methodsFor: 'instance creation'!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime |u| u := self new . u in: in . u maxdelaytime: maxdelaytime . u delaytime: delaytime . u decaytime: decaytime . ^ u !
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul |u| u := self new . u in: in . u maxdelaytime: maxdelaytime . u delaytime: delaytime . u decaytime: decaytime . ^ u * mul !
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add |u| u := self new . u in: in . u maxdelaytime: maxdelaytime . u delaytime: delaytime . u decaytime: decaytime . ^ u * mul + add !
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
in: x in := x !
maxdelaytime: x maxdelaytime := x !
delaytime: x delaytime := x !
decaytime: x decaytime := x !
!

!CombN methodsFor: 'private'!
initialize ugenName := #CombN . paramNames := #(in maxdelaytime delaytime decaytime) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!CombN class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!CombN class methodsFor: 'instance creation'!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime |u| u := self new . u in: in . u maxdelaytime: maxdelaytime . u delaytime: delaytime . u decaytime: decaytime . ^ u !
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul |u| u := self new . u in: in . u maxdelaytime: maxdelaytime . u delaytime: delaytime . u decaytime: decaytime . ^ u * mul !
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add |u| u := self new . u in: in . u maxdelaytime: maxdelaytime . u delaytime: delaytime . u decaytime: decaytime . ^ u * mul + add !
!

UGen subclass: #Crackle
instanceVariableNames: 'chaosParam' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!Crackle commentStamp: '<historical>' prior: 0!
Chaotic noise function. !

!Crackle methodsFor: 'accessing'!
chaosParam ^chaosParam !
!

!Crackle methodsFor: 'private'!
chaosParam: x chaosParam := x !
!

!Crackle methodsFor: 'private'!
initialize ugenName := #Crackle . paramNames := #(chaosParam) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!Crackle class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Crackle class methodsFor: 'instance creation'!
chaosParam: chaosParam |u| u := self new . u chaosParam: chaosParam . ^ u !
chaosParam: chaosParam mul: mul |u| u := self new . u chaosParam: chaosParam . ^ u * mul !
chaosParam: chaosParam mul: mul add: add |u| u := self new . u chaosParam: chaosParam . ^ u * mul + add !
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
in: x in := x !
decayTime: x decayTime := x !
!

!Decay methodsFor: 'private'!
initialize ugenName := #Decay . paramNames := #(in decayTime) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!Decay class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Decay class methodsFor: 'instance creation'!
in: in decayTime: decayTime |u| u := self new . u in: in . u decayTime: decayTime . ^ u !
in: in decayTime: decayTime mul: mul |u| u := self new . u in: in . u decayTime: decayTime . ^ u * mul !
in: in decayTime: decayTime mul: mul add: add |u| u := self new . u in: in . u decayTime: decayTime . ^ u * mul + add !
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
in: x in := x !
attackTime: x attackTime := x !
decayTime: x decayTime := x !
!

!Decay2 methodsFor: 'private'!
initialize ugenName := #Decay2 . paramNames := #(in attackTime decayTime) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!Decay2 class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Decay2 class methodsFor: 'instance creation'!
in: in attackTime: attackTime decayTime: decayTime |u| u := self new . u in: in . u attackTime: attackTime . u decayTime: decayTime . ^ u !
in: in attackTime: attackTime decayTime: decayTime mul: mul |u| u := self new . u in: in . u attackTime: attackTime . u decayTime: decayTime . ^ u * mul !
in: in attackTime: attackTime decayTime: decayTime mul: mul add: add |u| u := self new . u in: in . u attackTime: attackTime . u decayTime: decayTime . ^ u * mul + add !
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
bufnum: x bufnum := x !
in: x in := x !
octave: x octave := x !
!

!DegreeToKey methodsFor: 'private'!
initialize ugenName := #DegreeToKey . paramNames := #(bufnum in octave) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!DegreeToKey class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!DegreeToKey class methodsFor: 'instance creation'!
bufnum: bufnum in: in octave: octave |u| u := self new . u bufnum: bufnum . u in: in . u octave: octave . ^ u !
bufnum: bufnum in: in octave: octave mul: mul |u| u := self new . u bufnum: bufnum . u in: in . u octave: octave . ^ u * mul !
bufnum: bufnum in: in octave: octave mul: mul add: add |u| u := self new . u bufnum: bufnum . u in: in . u octave: octave . ^ u * mul + add !
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
in: x in := x !
maxdelaytime: x maxdelaytime := x !
delaytime: x delaytime := x !
!

!DelayN methodsFor: 'private'!
initialize ugenName := #DelayN . paramNames := #(in maxdelaytime delaytime) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!DelayN class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!DelayN class methodsFor: 'instance creation'!
in: in maxdelaytime: maxdelaytime delaytime: delaytime |u| u := self new . u in: in . u maxdelaytime: maxdelaytime . u delaytime: delaytime . ^ u !
in: in maxdelaytime: maxdelaytime delaytime: delaytime mul: mul |u| u := self new . u in: in . u maxdelaytime: maxdelaytime . u delaytime: delaytime . ^ u * mul !
in: in maxdelaytime: maxdelaytime delaytime: delaytime mul: mul add: add |u| u := self new . u in: in . u maxdelaytime: maxdelaytime . u delaytime: delaytime . ^ u * mul + add !
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
trig: x trig := x !
reset: x reset := x !
demandUGens: x demandUGens := x !
!

!Demand methodsFor: 'private'!
initialize ugenName := #Demand . paramNames := #(trig reset) . mceNames := #(demandUGens) . ugenRate := #ar . numChan := nil . specialIndex := 0 ! !

!Demand class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Demand class methodsFor: 'instance creation'!
trig: trig reset: reset demandUGens: demandUGens |u| u := self new . u numChan: #mceInput .  u trig: trig . u reset: reset . u demandUGens: demandUGens . ^ u !
trig: trig reset: reset demandUGens: demandUGens mul: mul |u| u := self new . u numChan: #mceInput .  u trig: trig . u reset: reset . u demandUGens: demandUGens . ^ u * mul !
trig: trig reset: reset demandUGens: demandUGens mul: mul add: add |u| u := self new . u numChan: #mceInput .  u trig: trig . u reset: reset . u demandUGens: demandUGens . ^ u * mul + add !
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
repeats: x repeats := x !
list: x list := x !
!

!Drand methodsFor: 'private'!
initialize ugenName := #Drand . paramNames := #(repeats) . mceNames := #(list) . ugenRate := #dr . numChan := 1 . specialIndex := 0 ! !

!Drand class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Drand class methodsFor: 'instance creation'!
repeats: repeats list: list |u| u := self new . u repeats: repeats . u list: list . ^ u !
repeats: repeats list: list mul: mul |u| u := self new . u repeats: repeats . u list: list . ^ u * mul !
repeats: repeats list: list mul: mul add: add |u| u := self new . u repeats: repeats . u list: list . ^ u * mul + add !
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
repeats: x repeats := x !
list: x list := x !
!

!Dseq methodsFor: 'private'!
initialize ugenName := #Dseq . paramNames := #(repeats) . mceNames := #(list) . ugenRate := #dr . numChan := 1 . specialIndex := 0 ! !

!Dseq class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Dseq class methodsFor: 'instance creation'!
repeats: repeats list: list |u| u := self new . u repeats: repeats . u list: list . ^ u !
repeats: repeats list: list mul: mul |u| u := self new . u repeats: repeats . u list: list . ^ u * mul !
repeats: repeats list: list mul: mul add: add |u| u := self new . u repeats: repeats . u list: list . ^ u * mul + add !
!

UGen subclass: #Dust
instanceVariableNames: 'density' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!Dust commentStamp: '<historical>' prior: 0!
Random impulses. !

!Dust methodsFor: 'accessing'!
density ^density !
!

!Dust methodsFor: 'private'!
density: x density := x !
!

!Dust methodsFor: 'private'!
initialize ugenName := #Dust . paramNames := #(density) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!Dust class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Dust class methodsFor: 'instance creation'!
density: density |u| u := self new . u density: density . ^ u !
density: density mul: mul |u| u := self new . u density: density . ^ u * mul !
density: density mul: mul add: add |u| u := self new . u density: density . ^ u * mul + add !
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
gate: x gate := x !
levelScale: x levelScale := x !
levelBias: x levelBias := x !
timeScale: x timeScale := x !
doneAction: x doneAction := x !
envelope: x envelope := x !
!

!EnvGen methodsFor: 'private'!
initialize ugenName := #EnvGen . paramNames := #(gate levelScale levelBias timeScale doneAction) . mceNames := #(envelope) . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!EnvGen class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!EnvGen class methodsFor: 'instance creation'!
gate: gate levelScale: levelScale levelBias: levelBias timeScale: timeScale doneAction: doneAction envelope: envelope |u| u := self new . u gate: gate . u levelScale: levelScale . u levelBias: levelBias . u timeScale: timeScale . u doneAction: doneAction . u envelope: envelope . ^ u !
gate: gate levelScale: levelScale levelBias: levelBias timeScale: timeScale doneAction: doneAction envelope: envelope mul: mul |u| u := self new . u gate: gate . u levelScale: levelScale . u levelBias: levelBias . u timeScale: timeScale . u doneAction: doneAction . u envelope: envelope . ^ u * mul !
gate: gate levelScale: levelScale levelBias: levelBias timeScale: timeScale doneAction: doneAction envelope: envelope mul: mul add: add |u| u := self new . u gate: gate . u levelScale: levelScale . u levelBias: levelBias . u timeScale: timeScale . u doneAction: doneAction . u envelope: envelope . ^ u * mul + add !
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
lo: x lo := x !
hi: x hi := x !
!

!ExpRand methodsFor: 'private'!
initialize ugenName := #ExpRand . paramNames := #(lo hi) . mceNames := #() . ugenRate := #ir . numChan := 1 . specialIndex := 0 ! !

!ExpRand class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!ExpRand class methodsFor: 'instance creation'!
lo: lo hi: hi |u| u := self new . u lo: lo . u hi: hi . ^ u !
lo: lo hi: hi mul: mul |u| u := self new . u lo: lo . u hi: hi . ^ u * mul !
lo: lo hi: hi mul: mul add: add |u| u := self new . u lo: lo . u hi: hi . ^ u * mul + add !
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
freq: x freq := x !
iphase: x iphase := x !
!

!FSinOsc methodsFor: 'private'!
initialize ugenName := #FSinOsc . paramNames := #(freq iphase) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!FSinOsc class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!FSinOsc class methodsFor: 'instance creation'!
freq: freq iphase: iphase |u| u := self new . u freq: freq . u iphase: iphase . ^ u !
freq: freq iphase: iphase mul: mul |u| u := self new . u freq: freq . u iphase: iphase . ^ u * mul !
freq: freq iphase: iphase mul: mul add: add |u| u := self new . u freq: freq . u iphase: iphase . ^ u * mul + add !
!

UGen subclass: #HPZ1
instanceVariableNames: 'in' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!HPZ1 commentStamp: '<historical>' prior: 0!
Two point difference filter !

!HPZ1 methodsFor: 'accessing'!
in ^in !
!

!HPZ1 methodsFor: 'private'!
in: x in := x !
!

!HPZ1 methodsFor: 'private'!
initialize ugenName := #HPZ1 . paramNames := #(in) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!HPZ1 class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!HPZ1 class methodsFor: 'instance creation'!
in: in |u| u := self new . u in: in . ^ u !
in: in mul: mul |u| u := self new . u in: in . ^ u * mul !
in: in mul: mul add: add |u| u := self new . u in: in . ^ u * mul + add !
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
freq: x freq := x !
phase: x phase := x !
!

!Impulse methodsFor: 'private'!
initialize ugenName := #Impulse . paramNames := #(freq phase) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!Impulse class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Impulse class methodsFor: 'instance creation'!
freq: freq phase: phase |u| u := self new . u freq: freq . u phase: phase . ^ u !
freq: freq phase: phase mul: mul |u| u := self new . u freq: freq . u phase: phase . ^ u * mul !
freq: freq phase: phase mul: mul add: add |u| u := self new . u freq: freq . u phase: phase . ^ u * mul + add !
!

UGen subclass: #In
instanceVariableNames: 'bus' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!In commentStamp: '<historical>' prior: 0!
Read a signal from a bus. !

!In methodsFor: 'accessing'!
bus ^bus !
!

!In methodsFor: 'private'!
bus: x bus := x !
!

!In methodsFor: 'private'!
initialize ugenName := #In . paramNames := #(bus) . mceNames := #() . ugenRate := #ar . numChan := nil . specialIndex := 0 ! !

!In class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!In class methodsFor: 'instance creation'!
numChan: numChan bus: bus |u| u := self new . u numChan: numChan . u bus: bus . ^ u !
numChan: numChan bus: bus mul: mul |u| u := self new . u numChan: numChan . u bus: bus . ^ u * mul !
numChan: numChan bus: bus mul: mul add: add |u| u := self new . u numChan: numChan . u bus: bus . ^ u * mul + add !
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
lo: x lo := x !
hi: x hi := x !
!

!IRand methodsFor: 'private'!
initialize ugenName := #IRand . paramNames := #(lo hi) . mceNames := #() . ugenRate := #ir . numChan := 1 . specialIndex := 0 ! !

!IRand class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!IRand class methodsFor: 'instance creation'!
lo: lo hi: hi |u| u := self new . u lo: lo . u hi: hi . ^ u !
lo: lo hi: hi mul: mul |u| u := self new . u lo: lo . u hi: hi . ^ u * mul !
lo: lo hi: hi mul: mul add: add |u| u := self new . u lo: lo . u hi: hi . ^ u * mul + add !
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
input: x input := x !
freqscale: x freqscale := x !
freqoffset: x freqoffset := x !
decayscale: x decayscale := x !
specificationsArrayRef: x specificationsArrayRef := x !
!

!Klank methodsFor: 'private'!
initialize ugenName := #Klank . paramNames := #(input freqscale freqoffset decayscale) . mceNames := #(specificationsArrayRef) . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!Klank class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Klank class methodsFor: 'instance creation'!
input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef |u| u := self new . u input: input . u freqscale: freqscale . u freqoffset: freqoffset . u decayscale: decayscale . u specificationsArrayRef: specificationsArrayRef . ^ u !
input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef mul: mul |u| u := self new . u input: input . u freqscale: freqscale . u freqoffset: freqoffset . u decayscale: decayscale . u specificationsArrayRef: specificationsArrayRef . ^ u * mul !
input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef mul: mul add: add |u| u := self new . u input: input . u freqscale: freqscale . u freqoffset: freqoffset . u decayscale: decayscale . u specificationsArrayRef: specificationsArrayRef . ^ u * mul + add !
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
in: x in := x !
lagTime: x lagTime := x !
!

!Lag methodsFor: 'private'!
initialize ugenName := #Lag . paramNames := #(in lagTime) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!Lag class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Lag class methodsFor: 'instance creation'!
in: in lagTime: lagTime |u| u := self new . u in: in . u lagTime: lagTime . ^ u !
in: in lagTime: lagTime mul: mul |u| u := self new . u in: in . u lagTime: lagTime . ^ u * mul !
in: in lagTime: lagTime mul: mul add: add |u| u := self new . u in: in . u lagTime: lagTime . ^ u * mul + add !
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
in: x in := x !
trig: x trig := x !
!

!Latch methodsFor: 'private'!
initialize ugenName := #Latch . paramNames := #(in trig) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!Latch class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Latch class methodsFor: 'instance creation'!
in: in trig: trig |u| u := self new . u in: in . u trig: trig . ^ u !
in: in trig: trig mul: mul |u| u := self new . u in: in . u trig: trig . ^ u * mul !
in: in trig: trig mul: mul add: add |u| u := self new . u in: in . u trig: trig . ^ u * mul + add !
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
in: x in := x !
coef: x coef := x !
!

!LeakDC methodsFor: 'private'!
initialize ugenName := #LeakDC . paramNames := #(in coef) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!LeakDC class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!LeakDC class methodsFor: 'instance creation'!
in: in coef: coef |u| u := self new . u in: in . u coef: coef . ^ u !
in: in coef: coef mul: mul |u| u := self new . u in: in . u coef: coef . ^ u * mul !
in: in coef: coef mul: mul add: add |u| u := self new . u in: in . u coef: coef . ^ u * mul + add !
!

UGen subclass: #LFNoise1
instanceVariableNames: 'freq' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!LFNoise1 commentStamp: '<historical>' prior: 0!
Ramp noise !

!LFNoise1 methodsFor: 'accessing'!
freq ^freq !
!

!LFNoise1 methodsFor: 'private'!
freq: x freq := x !
!

!LFNoise1 methodsFor: 'private'!
initialize ugenName := #LFNoise1 . paramNames := #(freq) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!LFNoise1 class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!LFNoise1 class methodsFor: 'instance creation'!
freq: freq |u| u := self new . u freq: freq . ^ u !
freq: freq mul: mul |u| u := self new . u freq: freq . ^ u * mul !
freq: freq mul: mul add: add |u| u := self new . u freq: freq . ^ u * mul + add !
!

UGen subclass: #LFNoise2
instanceVariableNames: 'freq' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!LFNoise2 commentStamp: '<historical>' prior: 0!
Quadratic noise. !

!LFNoise2 methodsFor: 'accessing'!
freq ^freq !
!

!LFNoise2 methodsFor: 'private'!
freq: x freq := x !
!

!LFNoise2 methodsFor: 'private'!
initialize ugenName := #LFNoise2 . paramNames := #(freq) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!LFNoise2 class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!LFNoise2 class methodsFor: 'instance creation'!
freq: freq |u| u := self new . u freq: freq . ^ u !
freq: freq mul: mul |u| u := self new . u freq: freq . ^ u * mul !
freq: freq mul: mul add: add |u| u := self new . u freq: freq . ^ u * mul + add !
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
freq: x freq := x !
iphase: x iphase := x !
width: x width := x !
!

!LFPulse methodsFor: 'private'!
initialize ugenName := #LFPulse . paramNames := #(freq iphase width) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!LFPulse class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!LFPulse class methodsFor: 'instance creation'!
freq: freq iphase: iphase width: width |u| u := self new . u freq: freq . u iphase: iphase . u width: width . ^ u !
freq: freq iphase: iphase width: width mul: mul |u| u := self new . u freq: freq . u iphase: iphase . u width: width . ^ u * mul !
freq: freq iphase: iphase width: width mul: mul add: add |u| u := self new . u freq: freq . u iphase: iphase . u width: width . ^ u * mul + add !
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
freq: x freq := x !
iphase: x iphase := x !
!

!LFSaw methodsFor: 'private'!
initialize ugenName := #LFSaw . paramNames := #(freq iphase) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!LFSaw class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!LFSaw class methodsFor: 'instance creation'!
freq: freq iphase: iphase |u| u := self new . u freq: freq . u iphase: iphase . ^ u !
freq: freq iphase: iphase mul: mul |u| u := self new . u freq: freq . u iphase: iphase . ^ u * mul !
freq: freq iphase: iphase mul: mul add: add |u| u := self new . u freq: freq . u iphase: iphase . ^ u * mul + add !
!

UGen subclass: #LFTri
instanceVariableNames: 'freq iphase' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!LFTri commentStamp: '<historical>' prior: 0!
Triangle oscillator !

!LFTri methodsFor: 'accessing'!
freq ^freq !
iphase ^iphase !
!

!LFTri methodsFor: 'private'!
freq: x freq := x !
iphase: x iphase := x !
!

!LFTri methodsFor: 'private'!
initialize ugenName := #LFTri . paramNames := #(freq iphase) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!LFTri class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!LFTri class methodsFor: 'instance creation'!
freq: freq iphase: iphase |u| u := self new . u freq: freq . u iphase: iphase . ^ u !
freq: freq iphase: iphase mul: mul |u| u := self new . u freq: freq . u iphase: iphase . ^ u * mul !
freq: freq iphase: iphase mul: mul add: add |u| u := self new . u freq: freq . u iphase: iphase . ^ u * mul + add !
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
start: x start := x !
end: x end := x !
dur: x dur := x !
doneAction: x doneAction := x !
!

!Line methodsFor: 'private'!
initialize ugenName := #Line . paramNames := #(start end dur doneAction) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!Line class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Line class methodsFor: 'instance creation'!
start: start end: end dur: dur doneAction: doneAction |u| u := self new . u start: start . u end: end . u dur: dur . u doneAction: doneAction . ^ u !
start: start end: end dur: dur doneAction: doneAction mul: mul |u| u := self new . u start: start . u end: end . u dur: dur . u doneAction: doneAction . ^ u * mul !
start: start end: end dur: dur doneAction: doneAction mul: mul add: add |u| u := self new . u start: start . u end: end . u dur: dur . u doneAction: doneAction . ^ u * mul + add !
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
numChannels: x numChannels := x !
numFrames: x numFrames := x !
!

!LocalBuf methodsFor: 'private'!
initialize ugenName := #LocalBuf . paramNames := #(numChannels numFrames) . mceNames := #() . ugenRate := #ir . numChan := 1 . specialIndex := 0 ! !

!LocalBuf class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!LocalBuf class methodsFor: 'instance creation'!
numChannels: numChannels numFrames: numFrames |u| u := self new . u numChannels: numChannels . u numFrames: numFrames . ^ u !
numChannels: numChannels numFrames: numFrames mul: mul |u| u := self new . u numChannels: numChannels . u numFrames: numFrames . ^ u * mul !
numChannels: numChannels numFrames: numFrames mul: mul add: add |u| u := self new . u numChannels: numChannels . u numFrames: numFrames . ^ u * mul + add !
!

UGen subclass: #LocalIn
instanceVariableNames: 'default' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!LocalIn commentStamp: '<historical>' prior: 0!
Define and read from buses local to a synth. !

!LocalIn methodsFor: 'accessing'!
default ^default !
!

!LocalIn methodsFor: 'private'!
default: x default := x !
!

!LocalIn methodsFor: 'private'!
initialize ugenName := #LocalIn . paramNames := #() . mceNames := #(default) . ugenRate := #ar . numChan := nil . specialIndex := 0 ! !

!LocalIn class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!LocalIn class methodsFor: 'instance creation'!
numChan: numChan default: default |u| u := self new . u numChan: numChan . u default: default . ^ u !
numChan: numChan default: default mul: mul |u| u := self new . u numChan: numChan . u default: default . ^ u * mul !
numChan: numChan default: default mul: mul add: add |u| u := self new . u numChan: numChan . u default: default . ^ u * mul + add !
!

UGen subclass: #LocalOut
instanceVariableNames: 'channelsArray' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!LocalOut commentStamp: '<historical>' prior: 0!
Write to buses local to a synth. !

!LocalOut methodsFor: 'accessing'!
channelsArray ^channelsArray !
!

!LocalOut methodsFor: 'private'!
channelsArray: x channelsArray := x !
!

!LocalOut methodsFor: 'private'!
initialize ugenName := #LocalOut . paramNames := #() . mceNames := #(channelsArray) . ugenRate := #ar . numChan := 0 . specialIndex := 0 ! !

!LocalOut class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!LocalOut class methodsFor: 'instance creation'!
channelsArray: channelsArray |u| u := self new . u channelsArray: channelsArray . ^ u !
channelsArray: channelsArray mul: mul |u| u := self new . u channelsArray: channelsArray . ^ u * mul !
channelsArray: channelsArray mul: mul add: add |u| u := self new . u channelsArray: channelsArray . ^ u * mul + add !
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
in: x in := x !
freq: x freq := x !
!

!LPF methodsFor: 'private'!
initialize ugenName := #LPF . paramNames := #(in freq) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!LPF class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!LPF class methodsFor: 'instance creation'!
in: in freq: freq |u| u := self new . u in: in . u freq: freq . ^ u !
in: in freq: freq mul: mul |u| u := self new . u in: in . u freq: freq . ^ u * mul !
in: in freq: freq mul: mul add: add |u| u := self new . u in: in . u freq: freq . ^ u * mul + add !
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
minval: x minval := x !
maxval: x maxval := x !
warp: x warp := x !
lag: x lag := x !
!

!MouseX methodsFor: 'private'!
initialize ugenName := #MouseX . paramNames := #(minval maxval warp lag) . mceNames := #() . ugenRate := #kr . numChan := 1 . specialIndex := 0 ! !

!MouseX class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!MouseX class methodsFor: 'instance creation'!
minval: minval maxval: maxval warp: warp lag: lag |u| u := self new . u minval: minval . u maxval: maxval . u warp: warp . u lag: lag . ^ u !
minval: minval maxval: maxval warp: warp lag: lag mul: mul |u| u := self new . u minval: minval . u maxval: maxval . u warp: warp . u lag: lag . ^ u * mul !
minval: minval maxval: maxval warp: warp lag: lag mul: mul add: add |u| u := self new . u minval: minval . u maxval: maxval . u warp: warp . u lag: lag . ^ u * mul + add !
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
minval: x minval := x !
maxval: x maxval := x !
warp: x warp := x !
lag: x lag := x !
!

!MouseY methodsFor: 'private'!
initialize ugenName := #MouseY . paramNames := #(minval maxval warp lag) . mceNames := #() . ugenRate := #kr . numChan := 1 . specialIndex := 0 ! !

!MouseY class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!MouseY class methodsFor: 'instance creation'!
minval: minval maxval: maxval warp: warp lag: lag |u| u := self new . u minval: minval . u maxval: maxval . u warp: warp . u lag: lag . ^ u !
minval: minval maxval: maxval warp: warp lag: lag mul: mul |u| u := self new . u minval: minval . u maxval: maxval . u warp: warp . u lag: lag . ^ u * mul !
minval: minval maxval: maxval warp: warp lag: lag mul: mul add: add |u| u := self new . u minval: minval . u maxval: maxval . u warp: warp . u lag: lag . ^ u * mul + add !
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
in: x in := x !
mul: x mul := x !
add: x add := x !
!

!MulAdd methodsFor: 'private'!
initialize ugenName := #MulAdd . paramNames := #(in mul add) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!MulAdd class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!MulAdd class methodsFor: 'instance creation'!
in: in mul: mul add: add |u| u := self new . u in: in . u mul: mul . u add: add . ^ u !
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
in: x in := x !
coef: x coef := x !
!

!OnePole methodsFor: 'private'!
initialize ugenName := #OnePole . paramNames := #(in coef) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!OnePole class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!OnePole class methodsFor: 'instance creation'!
in: in coef: coef |u| u := self new . u in: in . u coef: coef . ^ u !
in: in coef: coef mul: mul |u| u := self new . u in: in . u coef: coef . ^ u * mul !
in: in coef: coef mul: mul add: add |u| u := self new . u in: in . u coef: coef . ^ u * mul + add !
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
bus: x bus := x !
channelsArray: x channelsArray := x !
!

!Out methodsFor: 'private'!
initialize ugenName := #Out . paramNames := #(bus) . mceNames := #(channelsArray) . ugenRate := #ar . numChan := 0 . specialIndex := 0 ! !

!Out class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Out class methodsFor: 'instance creation'!
bus: bus channelsArray: channelsArray |u| u := self new . u bus: bus . u channelsArray: channelsArray . ^ u !
bus: bus channelsArray: channelsArray mul: mul |u| u := self new . u bus: bus . u channelsArray: channelsArray . ^ u * mul !
bus: bus channelsArray: channelsArray mul: mul add: add |u| u := self new . u bus: bus . u channelsArray: channelsArray . ^ u * mul + add !
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
in: x in := x !
pos: x pos := x !
level: x level := x !
!

!Pan2 methodsFor: 'private'!
initialize ugenName := #Pan2 . paramNames := #(in pos level) . mceNames := #() . ugenRate := #ar . numChan := 2 . specialIndex := 0 ! !

!Pan2 class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Pan2 class methodsFor: 'instance creation'!
in: in pos: pos level: level |u| u := self new . u in: in . u pos: pos . u level: level . ^ u !
in: in pos: pos level: level mul: mul |u| u := self new . u in: in . u pos: pos . u level: level . ^ u * mul !
in: in pos: pos level: level mul: mul add: add |u| u := self new . u in: in . u pos: pos . u level: level . ^ u * mul + add !
!

UGen subclass: #Phasor
instanceVariableNames: 'trig rate start end resetPos' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!Phasor commentStamp: '<historical>' prior: 0!
A resettable linear ramp between two levels. !

!Phasor methodsFor: 'accessing'!
trig ^trig !
rate ^rate !
start ^start !
end ^end !
resetPos ^resetPos !
!

!Phasor methodsFor: 'private'!
trig: x trig := x !
rate: x rate := x !
start: x start := x !
end: x end := x !
resetPos: x resetPos := x !
!

!Phasor methodsFor: 'private'!
initialize ugenName := #Phasor . paramNames := #(trig rate start end resetPos) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!Phasor class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Phasor class methodsFor: 'instance creation'!
trig: trig rate: rate start: start end: end resetPos: resetPos |u| u := self new . u trig: trig . u rate: rate . u start: start . u end: end . u resetPos: resetPos . ^ u !
trig: trig rate: rate start: start end: end resetPos: resetPos mul: mul |u| u := self new . u trig: trig . u rate: rate . u start: start . u end: end . u resetPos: resetPos . ^ u * mul !
trig: trig rate: rate start: start end: end resetPos: resetPos mul: mul add: add |u| u := self new . u trig: trig . u rate: rate . u start: start . u end: end . u resetPos: resetPos . ^ u * mul + add !
!

UGen subclass: #PinkNoise
instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!PinkNoise commentStamp: '<historical>' prior: 0!
Pink Noise. !



!PinkNoise methodsFor: 'private'!
initialize ugenName := #PinkNoise . paramNames := #() . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

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
lo: x lo := x !
hi: x hi := x !
!

!Rand methodsFor: 'private'!
initialize ugenName := #Rand . paramNames := #(lo hi) . mceNames := #() . ugenRate := #ir . numChan := 1 . specialIndex := 0 ! !

!Rand class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Rand class methodsFor: 'instance creation'!
lo: lo hi: hi |u| u := self new . u lo: lo . u hi: hi . ^ u !
lo: lo hi: hi mul: mul |u| u := self new . u lo: lo . u hi: hi . ^ u * mul !
lo: lo hi: hi mul: mul add: add |u| u := self new . u lo: lo . u hi: hi . ^ u * mul + add !
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
in: x in := x !
freq: x freq := x !
bwr: x bwr := x !
!

!Resonz methodsFor: 'private'!
initialize ugenName := #Resonz . paramNames := #(in freq bwr) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!Resonz class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Resonz class methodsFor: 'instance creation'!
in: in freq: freq bwr: bwr |u| u := self new . u in: in . u freq: freq . u bwr: bwr . ^ u !
in: in freq: freq bwr: bwr mul: mul |u| u := self new . u in: in . u freq: freq . u bwr: bwr . ^ u * mul !
in: in freq: freq bwr: bwr mul: mul add: add |u| u := self new . u in: in . u freq: freq . u bwr: bwr . ^ u * mul + add !
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
in: x in := x !
freq: x freq := x !
rq: x rq := x !
!

!RHPF methodsFor: 'private'!
initialize ugenName := #RHPF . paramNames := #(in freq rq) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!RHPF class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!RHPF class methodsFor: 'instance creation'!
in: in freq: freq rq: rq |u| u := self new . u in: in . u freq: freq . u rq: rq . ^ u !
in: in freq: freq rq: rq mul: mul |u| u := self new . u in: in . u freq: freq . u rq: rq . ^ u * mul !
in: in freq: freq rq: rq mul: mul add: add |u| u := self new . u in: in . u freq: freq . u rq: rq . ^ u * mul + add !
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
in: x in := x !
freq: x freq := x !
rq: x rq := x !
!

!RLPF methodsFor: 'private'!
initialize ugenName := #RLPF . paramNames := #(in freq rq) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!RLPF class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!RLPF class methodsFor: 'instance creation'!
in: in freq: freq rq: rq |u| u := self new . u in: in . u freq: freq . u rq: rq . ^ u !
in: in freq: freq rq: rq mul: mul |u| u := self new . u in: in . u freq: freq . u rq: rq . ^ u * mul !
in: in freq: freq rq: rq mul: mul add: add |u| u := self new . u in: in . u freq: freq . u rq: rq . ^ u * mul + add !
!

UGen subclass: #SampleDur
instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!SampleDur commentStamp: '<historical>' prior: 0!
Duration of one sample. !



!SampleDur methodsFor: 'private'!
initialize ugenName := #SampleDur . paramNames := #() . mceNames := #() . ugenRate := #ir . numChan := 1 . specialIndex := 0 ! !

!SampleDur class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!SampleDur class methodsFor: 'instance creation'!
 mul: mul |u| u := self new .  ^ u * mul !
 mul: mul add: add |u| u := self new .  ^ u * mul + add !
!

UGen subclass: #Saw
instanceVariableNames: 'freq' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!Saw commentStamp: '<historical>' prior: 0!
Band limited sawtooth. !

!Saw methodsFor: 'accessing'!
freq ^freq !
!

!Saw methodsFor: 'private'!
freq: x freq := x !
!

!Saw methodsFor: 'private'!
initialize ugenName := #Saw . paramNames := #(freq) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!Saw class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Saw class methodsFor: 'instance creation'!
freq: freq |u| u := self new . u freq: freq . ^ u !
freq: freq mul: mul |u| u := self new . u freq: freq . ^ u * mul !
freq: freq mul: mul add: add |u| u := self new . u freq: freq . ^ u * mul + add !
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
buf: x buf := x !
offset: x offset := x !
length: x length := x !
array: x array := x !
!

!SetBuf methodsFor: 'private'!
initialize ugenName := #SetBuf . paramNames := #(buf offset length) . mceNames := #(array) . ugenRate := #ir . numChan := 1 . specialIndex := 0 ! !

!SetBuf class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!SetBuf class methodsFor: 'instance creation'!
buf: buf offset: offset length: length array: array |u| u := self new . u buf: buf . u offset: offset . u length: length . u array: array . ^ u !
buf: buf offset: offset length: length array: array mul: mul |u| u := self new . u buf: buf . u offset: offset . u length: length . u array: array . ^ u * mul !
buf: buf offset: offset length: length array: array mul: mul add: add |u| u := self new . u buf: buf . u offset: offset . u length: length . u array: array . ^ u * mul + add !
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
freq: x freq := x !
phase: x phase := x !
!

!SinOsc methodsFor: 'private'!
initialize ugenName := #SinOsc . paramNames := #(freq phase) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!SinOsc class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!SinOsc class methodsFor: 'instance creation'!
freq: freq phase: phase |u| u := self new . u freq: freq . u phase: phase . ^ u !
freq: freq phase: phase mul: mul |u| u := self new . u freq: freq . u phase: phase . ^ u * mul !
freq: freq phase: phase mul: mul add: add |u| u := self new . u freq: freq . u phase: phase . ^ u * mul + add !
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
in: x in := x !
dur: x dur := x !
!

!Trig methodsFor: 'private'!
initialize ugenName := #Trig . paramNames := #(in dur) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!Trig class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!Trig class methodsFor: 'instance creation'!
in: in dur: dur |u| u := self new . u in: in . u dur: dur . ^ u !
in: in dur: dur mul: mul |u| u := self new . u in: in . u dur: dur . ^ u * mul !
in: in dur: dur mul: mul add: add |u| u := self new . u in: in . u dur: dur . ^ u * mul + add !
!

UGen subclass: #XFade2
instanceVariableNames: 'inA inB pan level' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!XFade2 commentStamp: '<historical>' prior: 0!
Equal power two channel cross fade. !

!XFade2 methodsFor: 'accessing'!
inA ^inA !
inB ^inB !
pan ^pan !
level ^level !
!

!XFade2 methodsFor: 'private'!
inA: x inA := x !
inB: x inB := x !
pan: x pan := x !
level: x level := x !
!

!XFade2 methodsFor: 'private'!
initialize ugenName := #XFade2 . paramNames := #(inA inB pan level) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!XFade2 class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!XFade2 class methodsFor: 'instance creation'!
inA: inA inB: inB pan: pan level: level |u| u := self new . u inA: inA . u inB: inB . u pan: pan . u level: level . ^ u !
inA: inA inB: inB pan: pan level: level mul: mul |u| u := self new . u inA: inA . u inB: inB . u pan: pan . u level: level . ^ u * mul !
inA: inA inB: inB pan: pan level: level mul: mul add: add |u| u := self new . u inA: inA . u inB: inB . u pan: pan . u level: level . ^ u * mul + add !
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
start: x start := x !
end: x end := x !
dur: x dur := x !
doneAction: x doneAction := x !
!

!XLine methodsFor: 'private'!
initialize ugenName := #XLine . paramNames := #(start end dur doneAction) . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

!XLine class methodsFor: 'instance creation'!
new ^ self basicNew initialize ! !

!XLine class methodsFor: 'instance creation'!
start: start end: end dur: dur doneAction: doneAction |u| u := self new . u start: start . u end: end . u dur: dur . u doneAction: doneAction . ^ u !
start: start end: end dur: dur doneAction: doneAction mul: mul |u| u := self new . u start: start . u end: end . u dur: dur . u doneAction: doneAction . ^ u * mul !
start: start end: end dur: dur doneAction: doneAction mul: mul add: add |u| u := self new . u start: start . u end: end . u dur: dur . u doneAction: doneAction . ^ u * mul + add !
!

UGen subclass: #WhiteNoise
instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!WhiteNoise commentStamp: '<historical>' prior: 0!
White noise. !



!WhiteNoise methodsFor: 'private'!
initialize ugenName := #WhiteNoise . paramNames := #() . mceNames := #() . ugenRate := #ar . numChan := 1 . specialIndex := 0 ! !

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
round: b ^ BinaryOpUGen specialIndex: 19 a: self b: b !
** b ^ BinaryOpUGen specialIndex: 25 a: self b: b !
amclip: b ^ BinaryOpUGen specialIndex: 40 a: self b: b !
clip2: b ^ BinaryOpUGen specialIndex: 42 a: self b: b !
!

!UGen methodsFor: 'arithmetic'!
negated ^ UnaryOpUGen specialIndex: 0 a: self !
abs ^ UnaryOpUGen specialIndex: 5 a: self !
cubed ^ UnaryOpUGen specialIndex: 13 a: self !
reciprocal ^ UnaryOpUGen specialIndex: 16 a: self !
midicps ^ UnaryOpUGen specialIndex: 17 a: self !
log ^ UnaryOpUGen specialIndex: 25 a: self !
sin ^ UnaryOpUGen specialIndex: 28 a: self !
distort ^ UnaryOpUGen specialIndex: 42 a: self !
!

