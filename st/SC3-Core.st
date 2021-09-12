Object subclass: #AbstractUGen
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

AbstractUGen comment: 'Unit Generator (Abstract)'!

!AbstractUGen class methodsFor: 'instance creation'!
  ar: d
    "
    Support for SuperCollider type class instantiation.
    Note there are also ar and kr instance methods, which are quite different.
    There are no unary methods, SC messages always allow arguments.
    "
    ^self new ugenRate: #ar ; setParamsFromOrderedDictionary: d
  !
  kr: d
    ^self new ugenRate: #kr ; setParamsFromOrderedDictionary: d
  !
  ir: d
    ^self new ugenRate: #ir ; setParamsFromOrderedDictionary: d
  !
  dr: d
    ^self new ugenRate: #dr ; setParamsFromOrderedDictionary: d
  !
!

!AbstractUGen methodsFor: 'accessing'!
  paramNameFromIndex: i
    "Get parameter name given index.
     Parameter names are separated into paramNames and mceNames.
     This does not allow mul: and add: inputs."
    | n |
    n := self paramNames size.
    i > n ifTrue: [^self mceNames at: i - n] ifFalse: [^self paramNames at: i]
  !
  setParamsFromOrderedDictionary: d
    "Set UGen parameters from an array of either ugen inputs or (name,input) associations."
    d withIndexDo: [:e :i |
      (e class = Association)
        ifTrue: [self perform: e key with: e value]
        ifFalse: [self instVarNamed: (self paramNameFromIndex: i) put: e]]
  !
!

!AbstractUGen methodsFor: 'converting'!
  adaptToInteger: rcvr andSend: selector
    "If I am involved in arithmetic with an Integer, convert it to a UGen."
    ^rcvr asUGen perform: selector with: self
  !
  adaptToFraction: rcvr andSend: selector
    "If I am involved in arithmetic with a Fraction, convert it to a UGen."
    ^rcvr asUGen perform: selector with: self
  !
  adaptToCollection: rcvr andSend: selector
    "If I am involved in arithmetic with a Collection, convert it to a UGen."
    ^rcvr asUGen perform: selector with: self
  !
!

!AbstractUGen methodsFor: 'testing'!
  isZero
    "In the general case false, c.f. Constant.
     Required for general Number / UGen math."
    ^false
  !
!

!AbstractUGen methodsFor: 'math'!
  ** aNumber
    ^self raisedTo: aNumber
  !
!

!AbstractUGen methodsFor: 'collecting'!
  addToUGenSet: c
    "Add inputs to UGen set.  Only actual instances of UGen are collected."
    self subclassResponsibility
  !
  ugenSet
    | c |
    c := IdentitySet new.
    self addToUGenSet: c.
    ^c
  !
!

!AbstractUGen methodsFor: 'multi-channel expansion'!
  mce
    ^Mce source: self
  !
  transpose
    ^self mce transpose
  !
  at: x
    ^self mce at: x
  !
  clump: x
    ^self mce clump: x
  !
!

AbstractUGen subclass: #UGen
  instanceVariableNames: 'ugenName paramNames mceNames numChan ugenRate specialIndex ugenId'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

UGen comment: 'Unit Generator.  ugenId is assigned during graph serialisation since identityHash is not unique'!

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
  ugenId ^ugenId
  !
  ugenId: x ugenId := x
  !
!

!UGen methodsFor: 'collecting'!
  addToUGenSet: c
    | addFunc |
    addFunc := [:x | (self perform: x) addToUGenSet: c].
    c add: self.
    paramNames do: addFunc.
    mceNames do: addFunc
  !
!

UGen subclass: #BinaryOpUGen
  instanceVariableNames: 'a b'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
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
    u ugenId: nil.
    u a: a.
    u b: b.
    ^u
  !
!

UGen subclass: #UnaryOpUGen
  instanceVariableNames: 'a'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
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
    u ugenId: nil.
    u a: a.
    ^u
  !
!

AbstractUGen subclass: #Rate
  instanceVariableNames: 'source rate'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

Rate comment: 'UGen Processing Rate'!

!Rate methodsFor: 'collecting'!
  addToUGenSet: c
    source addToUGenSet: c
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

AbstractUGen subclass: #Mce
  instanceVariableNames: 'source transform'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

Mce comment: 'Multiple Channel Expansion'!

!Array methodsFor: 'collecting'!
  addToUGenSet: c
    self do: [:x | x addToUGenSet: c]
  !
!

!Interval methodsFor: 'collecting'!
  addToUGenSet: c
    self asArray addToUGenSet: c
  !
!

!Mce methodsFor: 'collecting'!
  addToUGenSet: c
    source addToUGenSet: c
  !
!

!Mce methodsFor: 'accessing'!
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

!Mce methodsFor: 'initialization'!
  initialize
    source := nil.
    transform := nil
  !
!

!Mce class methodsFor: 'instance creation'!
  source: x
    ^self new source: x
  !
!

!Mce methodsFor: 'transforms'!
  reverse
    ^self copy transform: #(#mceReverse)
  !
  transpose
    ^self copy transform: #(#mceTranspose)
  !
  at: x
    ^self copy transform: (Array with: #mceChannel with: x - 1)
  !
  clump: x
    ^self copy transform: (Array with: #mceClump with: x)
  !
!

!AbstractUGen methodsFor: 'transforms'!
  reverse
    "Mce reverse"
    ^self mce reverse
  !
  transpose
    "Mce transpose"
    ^self mce transpose
  !
!

!Interval methodsFor: 'multi-channel expansion'!
  asUGen
    ^self asArray asUGen
  !
!

!Array methodsFor: 'multi-channel expansion'!
  asUGen
    "Array as UGen (ie. Mce)"
    ^Mce source: self
  !
!

!Number methodsFor: 'enumerating'!
  mceFill: f
    ^(self arrayFill: f) mce
  !
  mceFillZeroIndexed: f
    ^(self arrayFillZeroIndexed: f) mce
  !
!

AbstractUGen subclass: #Mix
  instanceVariableNames: 'source'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

Mix comment: 'UGen Mixer'!

!Mix methodsFor: 'collecting'!
  addToUGenSet: c
    source addToUGenSet: c
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
  category: 'Sound-SC3'
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
  category: 'Sound-SC3'
!

MRG comment: 'Multiple Root Graph'!

!MRG methodsFor: 'collecting'!
  addToUGenSet: c
    lhs addToUGenSet: c.
    rhs addToUGenSet: c
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
  category: 'Sound-SC3'
!

Control comment: 'Control Input'!

!Control methodsFor: 'collecting'!
  addToUGenSet: c
  !
!

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
  category: 'Sound-SC3'
!

Constant comment: 'Constant Numerical Value'!

!Constant methodsFor: 'testing'!
  isZero
    ^k isZero
  !
!

!Constant methodsFor: 'collecting'!
  addToUGenSet: c
  !
!

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
  asUGen
    ^Constant k: self
  !
!

!Number methodsFor: 'collecting'!
  addToUGenSet: c
  !
!

!Symbol methodsFor: 'collecting'!
  addToUGenSet: c
  !
!

!Boolean methodsFor: 'collecting'!
  addToUGenSet: c
  !
!

Object subclass: #OverlapTexture
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

OverlapTexture comment: 'Texture Generator'!

!OverlapTexture class methodsFor: 'texture generation'!
  graphFunc: graphFunc sustainTime: sustainTime transitionTime: transitionTime overlap: overlap
    ^overlap mixFill: [:i |
      | t s e |
      t := Impulse freq: (1 / (sustainTime + (transitionTime * 2))) phase: (i / overlap).
      s := graphFunc value: t.
      e := EnvLinen attackTime: transitionTime sustainTime: sustainTime releaseTime: transitionTime level: 1 curves: #(#sin #sin #sin).
      s * (e env gen: t doneAction: 0)]
  !
!
