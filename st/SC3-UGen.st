SystemOrganization addCategory: #'SC3'!

UGen subclass: #CombN
instanceVariableNames: 'in maxdelaytime delaytime decaytime' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!CombN commentStamp: '<historical>' prior: 0!
Comb delay line with no interpolation. !

!CombN methodsFor: 'private'!
setRate: r setIn: p1 setMaxdelaytime: p2 setDelaytime: p3 setDecaytime: p4 numChan := 1 . rate := r . in := p1 . maxdelaytime := p2 . delaytime := p3 . decaytime := p4 ! !

!CombN class methodsFor: 'instance creation'!
in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime ^self basicNew setRate: 'ar' setIn: in setMaxdelaytime: maxdelaytime setDelaytime: delaytime setDecaytime: decaytime ! !


UGen subclass: #LFSaw
instanceVariableNames: 'freq iphase' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!LFSaw commentStamp: '<historical>' prior: 0!
Sawtooth oscillator !

!LFSaw methodsFor: 'private'!
setRate: r setFreq: p1 setIphase: p2 numChan := 1 . rate := r . freq := p1 . iphase := p2 ! !

!LFSaw class methodsFor: 'instance creation'!
freq: freq iphase: iphase ^self basicNew setRate: 'ar' setFreq: freq setIphase: iphase ! !


UGen subclass: #SinOsc
instanceVariableNames: 'freq phase' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!SinOsc commentStamp: '<historical>' prior: 0!
Interpolating sine wavetable oscillator. !

!SinOsc methodsFor: 'private'!
setRate: r setFreq: p1 setPhase: p2 numChan := 1 . rate := r . freq := p1 . phase := p2 ! !

!SinOsc class methodsFor: 'instance creation'!
freq: freq phase: phase ^self basicNew setRate: 'ar' setFreq: freq setPhase: phase ! !


UGen subclass: #Pan2
instanceVariableNames: 'in pos level' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!Pan2 commentStamp: '<historical>' prior: 0!
Two channel equal power pan. !

!Pan2 methodsFor: 'private'!
setRate: r setIn: p1 setPos: p2 setLevel: p3 numChan := 2 . rate := r . in := p1 . pos := p2 . level := p3 ! !

!Pan2 class methodsFor: 'instance creation'!
in: in pos: pos level: level |u| u := self basicNew setRate: 'ar' setIn: in setPos: pos setLevel: level . ^{OutputProxy source: u outputIndex: 1 . OutputProxy source: u outputIndex: 2} ! !


UGen subclass: #Out
instanceVariableNames: 'bus channelsArray' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!Out commentStamp: '<historical>' prior: 0!
Write a signal to a bus. !

!Out methodsFor: 'private'!
setRate: r setBus: p1 setChannelsArray: p2 numChan := 0 . rate := r . bus := p1 . channelsArray := p2 ! !

!Out class methodsFor: 'instance creation'!
bus: bus channelsArray: channelsArray ^self basicNew setRate: 'ar' setBus: bus setChannelsArray: channelsArray ! !


UGen subclass: #BinaryOpUGen
instanceVariableNames: 'specialIndex a b' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!BinaryOpUGen commentStamp: '<historical>' prior: 0!
Binary Operators !

!BinaryOpUGen methodsFor: 'private'!
setRate: r setSpecialIndex: p1 setA: p2 setB: p3 numChan := 1 . rate := r . specialIndex := p1 . a := p2 . b := p3 ! !

!BinaryOpUGen class methodsFor: 'instance creation'!
specialIndex: specialIndex a: a b: b ^self basicNew setRate: 'ar' setSpecialIndex: specialIndex setA: a setB: b ! !


UGen subclass: #UnaryOpUGen
instanceVariableNames: 'specialIndex a' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!UnaryOpUGen commentStamp: '<historical>' prior: 0!
Unary Operators !

!UnaryOpUGen methodsFor: 'private'!
setRate: r setSpecialIndex: p1 setA: p2 numChan := 1 . rate := r . specialIndex := p1 . a := p2 ! !

!UnaryOpUGen class methodsFor: 'instance creation'!
specialIndex: specialIndex a: a ^self basicNew setRate: 'ar' setSpecialIndex: specialIndex setA: a ! !


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
