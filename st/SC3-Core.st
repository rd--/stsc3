Object subclass: #AbstractUGen
 instanceVariableNames: ''
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'
!

AbstractUGen comment: 'Unit Generator (Abstract)'!

!AbstractUGen methodsFor: 'collecting'!
 addToUGenSet: c
  "Add self any any inputs to UGen set.  Only actual instances of UGen are collected."
 !
 ugenSet
  | c |
  c := IdentitySet new.
  self addToUGenSet: c.
  ^c
 !
!

AbstractUGen subclass: #UGen
 instanceVariableNames: 'ugenName paramNames mceNames numChan ugenRate specialIndex'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'
!

UGen comment: 'Unit Generator'!

!UGen methodsFor: 'accessing'!
 mceNames ^mceNames
 !
 mceNames: x mceNames := x
 !
 numChan ^numChan
 !
 numChan: x numChan := x
 !
 paramNames ^paramNames
 !
 paramNames: x paramNames := x
 !
 specialIndex ^specialIndex
 !
 specialIndex: x specialIndex := x
 !
 ugenName ^ugenName
 !
 ugenName: x ugenName := x
 !
 ugenRate ^ugenRate
 !
 ugenRate: x ugenRate := x
 !
!

!UGen methodsFor: 'collecting'!
 addToUGenSet: c
  paramNames do: [:x |
   | p |
   c add: self.
   p := self perform: x.
   (p isKindOf: UGen) ifTrue: [c add: p. p addToUGenSet: c] ifFalse: [(p isKindOf: AbstractUGen) ifTrue: [p addToUGenSet: c]]]
 !
!

UGen subclass: #BinaryOpUGen
 instanceVariableNames: 'a b'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'
!

BinaryOpUGen comment: 'Binary Operators'!

!BinaryOpUGen methodsFor: 'accessing'!
 a ^a
 !
 a: x a := x
 !
 b ^b
 !
 b: x b := x
 !
!

!BinaryOpUGen class methodsFor: 'instance creation'!
 specialIndex: specialIndex a: a b: b
  "Create binary operator UGen"
  | u |
  u := self new.
  u ugenName: #BinaryOpUGen.
  u paramNames: #(#a #b).
  u mceNames: #().
  u ugenRate: #(0 1).
  u numChan: 1.
  u specialIndex: specialIndex.
  u a: a.
  u b: b.
  ^u
 !
!

UGen subclass: #UnaryOpUGen
 instanceVariableNames: 'a'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'
!

UnaryOpUGen comment: 'Unary Operators'!

!UnaryOpUGen methodsFor: 'accessing'!
 a ^a
 !
 a: x a := x
 !
!

!UnaryOpUGen class methodsFor: 'instance creation'!
 specialIndex: specialIndex a: a
  "Create unary operator UGen"
  | u |
  u := self new.
  u ugenName: #UnaryOpUGen.
  u paramNames: #(#a).
  u mceNames: #().
  u ugenRate: #(0).
  u numChan: 1.
  u specialIndex: specialIndex.
  u a: a.
  ^u
 !
!

AbstractUGen subclass: #Rate
 instanceVariableNames: 'source rate'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'
!

Rate comment: 'UGen Processing Rate'!

!Rate methodsFor: 'collecting'!
 addToUGenSet: c
  (source isKindOf: AbstractUGen) ifTrue: [source addToUGenSet: c]
 !
!

!Rate methodsFor: 'accessing'!
 rate ^rate
 !
 rate: x rate := x
 !
 source ^source
 !
 source: x source := x
 !
!

!Rate class methodsFor: 'instance creation'!
 source: p1 rate: p2
  | r |
  r := self new.
  r source: p1.
  r rate: p2.
  ^r
 !
!

!AbstractUGen methodsFor: 'rate control'!
 ar
  ^Rate source: self rate: #ar
 !
 kr
  ^Rate source: self rate: #kr
 !
 ir
  ^Rate source: self rate: #ir
 !
 dr
  ^Rate source: self rate: #dr
 !
!

AbstractUGen subclass: #MCE
 instanceVariableNames: 'source transform'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'
!

MCE comment: 'Multiple Channel Expansion'!

!MCE methodsFor: 'collecting'!
 addToUGenSet: c
  (source isKindOf: AbstractUGen) ifTrue: [source addToUGenSet: c]
 !
!

!MCE methodsFor: 'accessing'!
 source: x source := x
 !
 source ^source
 !
 transform: x transform := x
 !
 transform ^transform
 !
 size ^source size
 !
!

!MCE methodsFor: 'initialization'!
 initialize
  source := nil.
  transform := nil
 !
!

!MCE class methodsFor: 'instance creation'!
 new
  ^self basicNew initialize
 !
 source: x
  ^self new source: x
 !
!

!MCE methodsFor: 'transforms'!
 transpose
  ^self copy transform: {#mceTranspose}
 !
 at: x
  ^self copy transform: {#mceChannel. x - 1}
 !
!

!Array methodsFor: 'multi-channel expansion'!
 mce
  ^MCE source: self
 !
!

!AbstractUGen methodsFor: 'multi-channel expansion'!
 mce
  ^MCE source: self
 !
!

!Number methodsFor: 'enumerating'!
 mceFill: f
  ^(self arrayFill: f) mce
 !
!

AbstractUGen subclass: #Mix
 instanceVariableNames: 'source'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'
!

Mix comment: 'UGen Mixer'!

!Mix methodsFor: 'collecting'!
 addToUGenSet: c
  (source isKindOf: AbstractUGen) ifTrue: [source addToUGenSet: c]
 !
!

!Mix methodsFor: 'accessing'!
 source ^source
 !
 source: x source := x
 !
!

!Mix class methodsFor: 'instance creation'!
 source: x
  ^self new source: x
 !
!

!AbstractUGen methodsFor: 'arithmetic'!
 mul: mul add: add
  ^MulAdd in: self mul: mul add: add
 !
 mix
  ^Mix source: self
 !
 round
  ^self round: 1.0
 !
!

Object subclass: #SC3
 instanceVariableNames: ''
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'
!

SC3 comment: 'SuperCollider3'!

!SC3 class methodsFor: 'server control'!
 reset
  'hsc3-scsynth reset' systemCommand
 !
!

AbstractUGen subclass: #MRG
 instanceVariableNames: 'lhs rhs'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'
!

MRG comment: 'Multiple Root Graph'!

!MRG methodsFor: 'collecting'!
 addToUGenSet: c
  (lhs isKindOf: AbstractUGen) ifTrue: [lhs addToUGenSet: c].
  (rhs isKindOf: AbstractUGen) ifTrue: [rhs addToUGenSet: c]
 !
!

!MRG methodsFor: 'accessing'!
 lhs ^lhs
 !
 lhs: x lhs := x
 !
 rhs ^rhs
 !
 rhs: x rhs := x
 !
!

!MRG class methodsFor: 'instance creation'!
 lhs: x rhs: y
  | u |
  u := self new.
  u lhs: x.
  u rhs: y.
  ^u
 !
!

AbstractUGen subclass: #Control
 instanceVariableNames: 'name init'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'
!

Control comment: 'Control Input'!

!Control methodsFor: 'accessing'!
 name ^name
 !
 name: x name := x
 !
 init ^init
 !
 init: x init := x
 !
!

!Control class methodsFor: 'instance creation'!
 name: p1 init: p2
  | u |
  u := self new.
  u name: p1.
  u init: p2.
  ^u
 !
!

AbstractUGen subclass: #Constant
 instanceVariableNames: 'k'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'
!

Constant comment: 'Constant Numerical Value'!

!Constant methodsFor: 'accessing'!
 k ^k
 !
 k: x k := x
 !
!

!Constant class methodsFor: 'instance creation'!
 k: k
  | u |
  u := self new.
  u k: k.
  ^u
 !
!

!Number methodsFor: 'UGen constants'!
 constant
  ^Constant k: self
 !
!
