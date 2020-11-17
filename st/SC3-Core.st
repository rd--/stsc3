SystemOrganization addCategory: #'SC3'!

Object subclass: #UGen
 instanceVariableNames: 'ugenName paramNames mceNames numChan ugenRate specialIndex'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!UGen methodsFor: 'private'!
 ugenName: x ugenName := x !
 paramNames: x paramNames := x !
 mceNames: x mceNames := x !
 numChan: x numChan := x !
 ugenRate: x ugenRate := x !
 specialIndex: x specialIndex := x ! !

!UGen methodsFor: 'accessing'!
 ugenName ^ugenName !
 paramNames ^paramNames !
 mceNames ^mceNames !
 numChan ^numChan !
 specialIndex ^specialIndex !
 ugenRate ^ugenRate ! !

UGen subclass: #BinaryOpUGen
instanceVariableNames: 'a b' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!BinaryOpUGen commentStamp: '<historical>' prior: 0!
Binary Operators !

!BinaryOpUGen methodsFor: 'accessing'!
a ^a !
b ^b !
!

!BinaryOpUGen methodsFor: 'private'!
a: x a := x !
b: x b := x !
!

!BinaryOpUGen class methodsFor: 'instance creation'!
specialIndex: specialIndex a: a b: b |u| u := self new . u ugenName: #BinaryOpUGen . u paramNames: #(a b) . u mceNames: #() . u ugenRate: #ar . u numChan: 1 . u specialIndex: specialIndex . u a: a . u b: b . ^u ! !

UGen subclass: #UnaryOpUGen
instanceVariableNames: 'a' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!UnaryOpUGen commentStamp: '<historical>' prior: 0!
Unary Operators !

!UnaryOpUGen methodsFor: 'accessing'!
a ^a !
!

!UnaryOpUGen methodsFor: 'private'!
a: x a := x !
!

!UnaryOpUGen class methodsFor: 'instance creation'!
specialIndex: specialIndex a: a |u| u := self new . u ugenName: #UnaryOpUGen . u paramNames: #(a) . u mceNames: #() . u ugenRate: #ar . u numChan: 1 . u specialIndex: specialIndex . u a: a . ^u ! !

Object subclass: #OutputProxy
 instanceVariableNames: 'source outputIndex'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!OutputProxy methodsFor: 'private'!
 source: x source := x !
 outputIndex: x outputIndex := x ! !

!OutputProxy class methodsFor: 'instance creation'!
 source: source outputIndex: outputIndex ^self new source: source outputIndex: outputIndex ! !

UGen subclass: #AR
 instanceVariableNames: 'source'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!AR methodsFor: 'private'!
 source: x source := x ! !

!AR class methodsFor: 'instance creation'!
 source: x ^self new source: x ! !

!UGen methodsFor: 'rate control'!
 ar ^AR source: self ! !

UGen subclass: #KR
 instanceVariableNames: 'source'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!KR methodsFor: 'private'!
 source: x source := x ! !

!KR class methodsFor: 'instance creation'!
 source: x ^self new source: x ! !

!UGen methodsFor: 'rate control'!
 kr ^KR source: self ! !

UGen subclass: #DR
 instanceVariableNames: 'source'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!DR methodsFor: 'private'!
 source: x source := x ! !

!DR class methodsFor: 'instance creation'!
 source: x ^self new source: x ! !

!UGen methodsFor: 'rate control'!
 dr ^DR source: self ! !

UGen subclass: #IR
 instanceVariableNames: 'source'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!IR methodsFor: 'private'!
 source: x source := x ! !

!IR class methodsFor: 'instance creation'!
 source: x ^self new source: x ! !

!UGen methodsFor: 'rate control'!
 ir ^IR source: self ! !

UGen subclass: #MCE
 instanceVariableNames: 'source'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!MCE methodsFor: 'private'!
 source: x source := x ! !

!MCE methodsFor: 'accessing'!
 source ^ source !
 size ^ source size ! !

!MCE class methodsFor: 'instance creation'!
 source: x ^ self new source: x ! !

!Array methodsFor: 'multi-channel expansion'!
 mce ^ MCE source: self ! !

!Array methodsFor: 'rearranging'!
 transpose ^ (1 to: self first size) collect: [:c | self collect: [:r | r at: c]] ! !

!Number methodsFor: 'enumerating'!
 to: n collect: f ^ ((self to: n) asArray) collect: [:i| f value: i] !
 arrayFill: f ^ 1 to: self collect: [:i| f value: i] !
 replicate: x ^ self arrayFill: [:_i| x] !
 mceFill: f ^ (self arrayFill: f) mce !
 mixFill: f ^ (self arrayFill: f) sum !
!

!Number methodsFor: 'units'!
 midicps ^ 440 * (2 ** ((self - 69) * (1 / 12))) ! !

!Integer methodsFor: 'randomness'!
 rand ^ self atRandom !
 rand: n ^ (n - self + 1) atRandom + self - 1 !
 rand2 ^ (self negated) rand: self ! !

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
 source: x source := x ! !

!Mix class methodsFor: 'instance creation'!
 source: x ^ self new source: x ! !

!UGen methodsFor: 'arithmetic'!
 mul: mul add: add ^ MulAdd in: self mul: mul add: add !
 mix ^ Mix source: self !
 round ^ self round: 1.0 ! !

Object subclass: #SC3
 instanceVariableNames: ''
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!SC3 class methodsFor: 'server control'!
 reset OSProcess command: 'hsc3-scsynth reset' ! !

UGen subclass: #MRG
 instanceVariableNames: 'lhs rhs'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!MRG methodsFor: 'private'!
 lhs: x lhs := x !
 rhs: x rhs := x ! !

!MRG class methodsFor: 'instance creation'!
 lhs: x rhs: y |u| u := self new . u lhs: x . u rhs: y . ^ u ! !

!LocalBuf class methodsFor: 'instance creation'!
 newFrom: x
 |p q |
 p := LocalBuf numChannels: 1 numFrames: x size .
 q := SetBuf buf: p offset: 0 length: x size array: x .
 ^ MRG lhs: p rhs: q ! !

UGen subclass: #Splay
 instanceVariableNames: 'input spread level center levelComp'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !
!Splay commentStamp: '<historical>' prior: 0!
Pan a set of channels across the stereo field. !

!Splay methodsFor: 'accessing'!
spread ^spread !
level ^level !
center ^center !
levelComp ^levelComp !
input ^input !
!

!Splay methodsFor: 'private'!
spread: x spread := x !
level: x level := x !
center: x center := x !
levelComp: x levelComp := x !
input: x input := x !
!

!Splay class methodsFor: 'instance creation'!
spread: spread level: level center: center levelComp: levelComp input: input |u| u := self new . u spread: spread . u level: level . u center: center . u levelComp: levelComp . u input: input . ^ u ! !

"RotateStereo is a composite UGen at hsc3 - it cannot be used with the current haskell printer"

UGen subclass: #RotateStereo
 instanceVariableNames: 'in pos'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!RotateStereo methodsFor: 'private'!
 in: x in := x !
 pos: x pos := x ! !

!RotateStereo class methodsFor: 'instance creation'!
 in: p1 pos: p2 |u| u := self new . u in: p1 . u pos: p2 . ^ u ! !

"Control - UGen has instance variable name"

UGen subclass: #Control
 instanceVariableNames: 'key init'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!Control methodsFor: 'private'!
 name: x name := x !
 init: x init := x ! !

!Control methodsFor: 'accessing'!
 key ^key !
 init ^init ! !

!Control class methodsFor: 'instance creation'!
 key: p1 init: p2 |u| u := self new . u name: p1 . u init: p2 . ^ u ! !
