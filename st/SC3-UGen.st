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

!CombN class methodsFor: 'instance creation'!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime |u| u := self new . u setName: #CombN . u setParamNames: #(in maxdelaytime delaytime decaytime) . u setMceNames: #() . u setRate: #ar . u setNumChan: 1 . u setIn: in . u setMaxdelaytime: maxdelaytime . u setDelaytime: delaytime . u setDecaytime: decaytime . ^u ! !

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

!LFSaw class methodsFor: 'instance creation'!
freq: freq iphase: iphase |u| u := self new . u setName: #LFSaw . u setParamNames: #(freq iphase) . u setMceNames: #() . u setRate: #ar . u setNumChan: 1 . u setFreq: freq . u setIphase: iphase . ^u ! !

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

!SinOsc class methodsFor: 'instance creation'!
freq: freq phase: phase |u| u := self new . u setName: #SinOsc . u setParamNames: #(freq phase) . u setMceNames: #() . u setRate: #ar . u setNumChan: 1 . u setFreq: freq . u setPhase: phase . ^u ! !

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

!Pan2 class methodsFor: 'instance creation'!
in: in pos: pos level: level |u| u := self new . u setName: #Pan2 . u setParamNames: #(in pos level) . u setMceNames: #() . u setRate: #ar . u setNumChan: 2 . u setIn: in . u setPos: pos . u setLevel: level . ^u ! !

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

!Out class methodsFor: 'instance creation'!
bus: bus channelsArray: channelsArray |u| u := self new . u setName: #Out . u setParamNames: #(bus) . u setMceNames: #(channelsArray) . u setRate: #ar . u setNumChan: 0 . u setBus: bus . u setChannelsArray: channelsArray . ^u ! !


UGen subclass: #BinaryOpUGen
instanceVariableNames: 'specialIndex a b' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!BinaryOpUGen commentStamp: '<historical>' prior: 0!
Binary Operators !

!BinaryOpUGen methodsFor: 'accessing'!
specialIndex ^specialIndex !
a ^a !
b ^b !
!

!BinaryOpUGen methodsFor: 'private'!
setSpecialIndex: x specialIndex := x !
setA: x a := x !
setB: x b := x !
!

!BinaryOpUGen class methodsFor: 'instance creation'!
specialIndex: specialIndex a: a b: b |u| u := self new . u setName: #BinaryOpUGen . u setParamNames: #(specialIndex a b) . u setMceNames: #() . u setRate: #ar . u setNumChan: 1 . u setSpecialIndex: specialIndex . u setA: a . u setB: b . ^u ! !


UGen subclass: #UnaryOpUGen
instanceVariableNames: 'specialIndex a' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!UnaryOpUGen commentStamp: '<historical>' prior: 0!
Unary Operators !

!UnaryOpUGen methodsFor: 'accessing'!
specialIndex ^specialIndex !
a ^a !
!

!UnaryOpUGen methodsFor: 'private'!
setSpecialIndex: x specialIndex := x !
setA: x a := x !
!

!UnaryOpUGen class methodsFor: 'instance creation'!
specialIndex: specialIndex a: a |u| u := self new . u setName: #UnaryOpUGen . u setParamNames: #(specialIndex a) . u setMceNames: #() . u setRate: #ar . u setNumChan: 1 . u setSpecialIndex: specialIndex . u setA: a . ^u ! !


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
** b ^ BinaryOpUGen specialIndex: 25 a: self b: b !
!

!UGen methodsFor: 'arithmetic'!
abs ^ UnaryOpUGen specialIndex: 5 a: self !
cubed ^ UnaryOpUGen specialIndex: 13 a: self !
midicps ^ UnaryOpUGen specialIndex: 17 a: self !
neg ^ UnaryOpUGen specialIndex: 0 a: self !
!

