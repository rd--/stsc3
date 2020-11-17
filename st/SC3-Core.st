SystemOrganization addCategory: #'SC3'!

Object subclass: #UGen
 instanceVariableNames: 'ugenName paramNames mceNames numChan ugenRate specialIndex'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!UGen methodsFor: 'private'!
 setUGenName: x ugenName := x !
 setParamNames: x paramNames := x !
 setMceNames: x mceNames := x !
 setNumChan: x numChan := x !
 setUGenRate: x ugenRate := x !
 setSpecialIndex: x specialIndex := x ! !

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
setA: x a := x !
setB: x b := x !
!

!BinaryOpUGen class methodsFor: 'instance creation'!
specialIndex: specialIndex a: a b: b |u| u := self new . u setUGenName: #BinaryOpUGen . u setParamNames: #(a b) . u setMceNames: #() . u setUGenRate: #ar . u setNumChan: 1 . u setSpecialIndex: specialIndex . u setA: a . u setB: b . ^u ! !

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
specialIndex: specialIndex a: a |u| u := self new . u setUGenName: #UnaryOpUGen . u setParamNames: #(a) . u setMceNames: #() . u setUGenRate: #ar . u setNumChan: 1 . u setSpecialIndex: specialIndex . u setA: a . ^u ! !

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

UGen subclass: #AR
 instanceVariableNames: 'source'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!AR methodsFor: 'private'!
 setSource: x source := x ! !

!AR class methodsFor: 'instance creation'!
 source: x ^self new setSource: x ! !

!UGen methodsFor: 'rate control'!
 ar ^AR source: self ! !

UGen subclass: #KR
 instanceVariableNames: 'source'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!KR methodsFor: 'private'!
 setSource: x source := x ! !

!KR class methodsFor: 'instance creation'!
 source: x ^self new setSource: x ! !

!UGen methodsFor: 'rate control'!
 kr ^KR source: self ! !

UGen subclass: #DR
 instanceVariableNames: 'source'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!DR methodsFor: 'private'!
 setSource: x source := x ! !

!DR class methodsFor: 'instance creation'!
 source: x ^self new setSource: x ! !

!UGen methodsFor: 'rate control'!
 dr ^DR source: self ! !

UGen subclass: #IR
 instanceVariableNames: 'source'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!IR methodsFor: 'private'!
 setSource: x source := x ! !

!IR class methodsFor: 'instance creation'!
 source: x ^self new setSource: x ! !

!UGen methodsFor: 'rate control'!
 ir ^IR source: self ! !

UGen subclass: #MCE
 instanceVariableNames: 'source'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!MCE methodsFor: 'private'!
 setSource: x source := x ! !

!MCE methodsFor: 'accessing'!
 source ^ source !
 size ^ source size ! !

!MCE class methodsFor: 'instance creation'!
 source: x ^ self new setSource: x ! !

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
 setSource: x source := x ! !

!Mix class methodsFor: 'instance creation'!
 source: x ^ self new setSource: x ! !

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
 setLhs: x lhs := x !
 setRhs: x rhs := x ! !

!MRG class methodsFor: 'instance creation'!
 lhs: x rhs: y |u| u := self new . u setLhs: x . u setRhs: y . ^ u ! !

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
setSpread: x spread := x !
setLevel: x level := x !
setCenter: x center := x !
setLevelComp: x levelComp := x !
setInput: x input := x !
!

!Splay class methodsFor: 'instance creation'!
spread: spread level: level center: center levelComp: levelComp input: input |u| u := self new . u setSpread: spread . u setLevel: level . u setCenter: center . u setLevelComp: levelComp . u setInput: input . ^ u ! !

"RotateStereo is a composite UGen at hsc3 - it cannot be used with the current haskell printer"

UGen subclass: #RotateStereo
 instanceVariableNames: 'in pos'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!RotateStereo methodsFor: 'private'!
 setIn: x in := x !
 setPos: x pos := x ! !

!RotateStereo class methodsFor: 'instance creation'!
 in: p1 pos: p2 |u| u := self new . u setIn: p1 . u setPos: p2 . ^ u ! !

"Control - UGen has instance variable name"

UGen subclass: #Control
 instanceVariableNames: 'key init'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!Control methodsFor: 'private'!
 setKey: x key := x !
 setInit: x init := x ! !

!Control methodsFor: 'accessing'!
 key ^key !
 init ^init ! !

!Control class methodsFor: 'instance creation'!
 key: p1 init: p2 |u| u := self new . u setKey: p1 . u setInit: p2 . ^ u ! !
