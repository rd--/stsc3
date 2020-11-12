SystemOrganization addCategory: #'SC3'!

Object subclass: #UGen
 instanceVariableNames: 'name paramNames mceNames numChan rate specialIndex'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!UGen methodsFor: 'private'!
 setName: x name := x !
 setParamNames: x paramNames := x !
 setMceNames: x mceNames := x !
 setNumChan: x numChan := x !
 setRate: x rate := x !
 setSpecialIndex: x specialIndex := x ! !

!UGen methodsFor: 'accessing'!
 name ^name !
 paramNames ^paramNames !
 mceNames ^mceNames !
 numChan ^numChan !
 specialIndex ^specialIndex !
 rate ^rate ! !

UGen subclass: #BinaryOpUGen
instanceVariableNames: 'a b' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!BinaryOpUGen commentStamp: '<historical>' prior: 0!
Binary Operators !

!BinaryOpUGen methodsFor: 'accessing'!
a ^a !
b ^b !
!

!BinaryOpUGen methodsFor: 'private'!
setA: x a := x !
setB: x b := x !
!

!BinaryOpUGen class methodsFor: 'instance creation'!
specialIndex: specialIndex a: a b: b |u| u := self new . u setName: #BinaryOpUGen . u setParamNames: #(a b) . u setMceNames: #() . u setRate: #ar . u setNumChan: 1 . u setSpecialIndex: specialIndex . u setA: a . u setB: b . ^u ! !


UGen subclass: #UnaryOpUGen
instanceVariableNames: 'a' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!UnaryOpUGen commentStamp: '<historical>' prior: 0!
Unary Operators !

!UnaryOpUGen methodsFor: 'accessing'!
a ^a !
!

!UnaryOpUGen methodsFor: 'private'!
setA: x a := x !
!

!UnaryOpUGen class methodsFor: 'instance creation'!
specialIndex: specialIndex a: a |u| u := self new . u setName: #UnaryOpUGen . u setParamNames: #(a) . u setMceNames: #() . u setRate: #ar . u setNumChan: 1 . u setSpecialIndex: specialIndex . u setA: a . ^u ! !

Object subclass: #OutputProxy
 instanceVariableNames: 'source outputIndex'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!OutputProxy methodsFor: 'private'!
 setSource: x source := x !
 setOutputIndex: x outputIndex := x ! !

!OutputProxy class methodsFor: 'instance creation'!
 source: source outputIndex: outputIndex ^self new setSource: source setOutputIndex: outputIndex ! !

UGen subclass: #KR
 instanceVariableNames: 'source'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!KR methodsFor: 'private'!
 setSource: x source := x ! !

!KR class methodsFor: 'instance creation'!
 source: x ^self new setSource: x ! !

!UGen methodsFor: 'control rate'!
 kr ^KR source: self ! !

UGen subclass: #MCE
 instanceVariableNames: 'source'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!MCE methodsFor: 'private'!
 setSource: x source := x ! !

!MCE class methodsFor: 'instance creation'!
 source: x ^ self new setSource: x ! !

!Number methodsFor: 'enumerating'!
 to: n collect: f ^ ((self to: n) asArray collect: [:i| f value: i]) !
 mceFill: f ^ MCE source: (1 to: self collect: [:i| f value: i]) !
 mixFill: f ^ (1 to: self collect: [:i| f value: i]) sum !
!

!Number methodsFor: 'units'!
 midicps ^ 440 * (2 ** ((self - 69) * (1 / 12))) ! !

!Float methodsFor: 'randomness'!
 rand ^ (100000000 atRandom / 100000000.0) * self !
 rand: n ^ (100000000 atRandom / 100000000.0) * (n - self) + self !
 rand2 ^ (self negated) rand: self ! !

UGen subclass: #Mix
 instanceVariableNames: 'source'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!Mix methodsFor: 'private'!
 setSource: x source := x ! !

!Mix class methodsFor: 'instance creation'!
 source: x ^ self new setSource: x ! !
