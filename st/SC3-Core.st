Object subclass: #AbstractUGen
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

AbstractUGen comment: 'Unit Generator (Abstract)'!

!AbstractUGen class methodsFor: 'instance creation'!
  apply: arg
    ^self perform: (self primaryFactoryMethod) withArguments: arg
  !
!

!AbstractUGen methodsFor: 'filtering'!
  doesNotUnderstand: aMessage
    "Reflective form of filter variants for UGen constructors.
     Examines only the first part of the message selector, and if that, when capitalised, names a class, applies it.

     WhiteNoise new hPZ1 pan2: 0 level: 0.05
     "
    | messageName endIndex aClass arguments |
    messageName := aMessage selector asString.
    endIndex := messageName indexOf: $: ifAbsent: [ messageName size + 1 ].
    messageName := messageName copyFrom: 1 to: endIndex - 1.
    messageName at: 1 put: ((messageName at: 1) asUppercase).
    aClass := Smalltalk at: messageName asSymbol ifAbsent: [ nil ].
    aClass ifNil: [^super doesNotUnderstand: aMessage ].
    arguments := Array new: (aMessage arguments size + 1).
    arguments at: 1 put: self.
    (2 to: arguments size) do: [ :index | arguments at: index put: (aMessage arguments at: index - 1) ].
    ^aClass apply: arguments
  !
!

!BlockClosure methodsFor: 'evaluating'!
  apply: arg
    "For .stc notation.
     Allow f(x,y) as notation to apply a block f to arguments x & y."
    ^self valueWithArguments: arg
  !
!

!AbstractUGen methodsFor: 'accessing'!
  paramNameFromIndex: i
    "Get parameter name given index.
     Parameter names are separated into paramNames and mceNames.
     The implicit names mul: and add: follow."
    | n m |
    n := self paramNames size.
    m := self mceNames size.
    i <= n ifTrue: [^self paramNames at: i].
    i <= (n + m) ifTrue: [^self mceNames at: i - n].
    ^#(#mul #add) at: i - n - m
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
  adaptToFloat: rcvr andSend: selector
    "If I am involved in arithmetic with a Float, convert it to a UGen."
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
  @ aUGen
    ^Point x: self y: aUGen
  !
  unitCps
    ^(self * 127) midicps
  !
!

!Number methodsFor: 'math'!
  % aNumber
    "% is the SC floating point modulo operator."
    ^self asUGen % aNumber
  !
!

!Collection methodsFor: 'arithmetic'!
  ** arg
    ^arg adaptToCollection: self andSend: #**
  !
  % arg
    "% is the SC floating point modulo operator."
    ^arg adaptToCollection: self andSend: #%
  !
  mix
    ^self sum
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
  asMce
    "Mark UGen as Mce.
     This is necessary because the current implementation defers expansion to hsc3."
    ^Mce source: self
  !
  transpose
    ^self asMce transpose
  !
  at: x
    ^self asMce at: x
  !
  clump: x
    ^self asMce clump: x
  !
  rotate: x
    ^self asMce rotate: x
  !
  dup: x
    x replicate: self
  !
  dup
    self dup: 2
  !
  first
    "First channel.  Useful for expressions that need to work with both 0 and 1 based index systems."
    ^self asMce at: 1
  !
  second
    "Second channel.  Useful for expressions that need to work with both 0 and 1 based index systems."
    ^self asMce at: 2
  !
!

AbstractUGen subclass: #UGen
  instanceVariableNames: 'ugenName paramNames mceNames numChan ugenRate specialIndex ugenId'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

UGen comment: 'Unit Generator.
ugenId is assigned during graph serialisation since identityHash is not unique.
All UGens have mul: and add: methods but there are no variables associated with these.
'!

!UGen methodsFor: 'accessing'!
  mceNames ^mceNames!
  mceNames: x mceNames := x!
  numChan ^numChan!
  numChan: x numChan := x!
  paramNames ^paramNames!
  paramNames: x paramNames := x!
  specialIndex ^specialIndex!
  specialIndex: x specialIndex := x!
  ugenName ^ugenName!
  ugenName: x ugenName := x!
  ugenRate ^ugenRate!
  ugenRate: x ugenRate := x!
  ugenId ^ugenId!
  ugenId: x ugenId := x!
!

!UGen methodsFor: 'collecting'!
  addToUGenSet: c
    | addFunc |
    addFunc := [ :x | (self perform: x) addToUGenSet: c ].
    c add: self.
    paramNames do: addFunc.
    mceNames do: addFunc.
  !
!

!UGen methodsFor: 'arithmetic'!
  mul: x
     "((p mul: q) mul: r) is (p * q * r) not (p * r)."
      ^x = 1 ifTrue: [ self ] ifFalse: [ self * x ]
  !
  add: x
     "((p add: q) add: r) is (p + q + r) not (p + r)."
      ^x = 0 ifTrue: [ self ] ifFalse: [ self + x ]
  !
  mul: mul add: add
    ^(self mul: mul) add: add
  !
  mulAdd: mul value: add
    ".stc alias for mul:add:"
    ^self mul: mul add: add
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
  a ^a!
  a: x a := x!
  b ^b!
  b: x b := x!
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
  a ^a!
  a: x a := x!
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
  rate ^rate!
  rate: x rate := x!
  source ^source!
  source: x source := x!
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

!Array methodsFor: 'rate control'!
  kr
    ^self asUGen kr
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
  source: x source := x!
  source ^source!
  transform: x transform := x!
  transform ^transform!
  size ^source size!
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
  with: c1 with: c2
    ^Mce source: (Array with: c1 with: c2)
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
  atWrap: x
    ^self copy transform: (Array with: #mceChannelWrap with: x - 1)
  !
  clump: x
    ^self copy transform: (Array with: #mceClump with: x)
  !
  product
    ^self copy transform: #(#mceProduct)
  !
  rotate: n
    ^self copy transform: (Array with: #mceRotate with: n)
  !
!

!AbstractUGen methodsFor: 'transforms'!
  reverse
    "Mce reverse"
    ^self asMce reverse
  !
  transpose
    "Mce transpose"
    ^self asMce transpose
  !
  product
    ^self asMce product
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
    ^(self arrayFill: f) asUGen
  !
!

!Interval methodsFor: 'enumerating'!
  mceFill: aBlock
    ^(self asArray collect: aBlock) asUGen
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
  source ^source!
  source: x source := x!
!

!Mix class methodsFor: 'instance creation'!
  source: x
    ^self new source: x
  !
  primaryFactoryMethod ^#source:!
!

!AbstractUGen methodsFor: 'arithmetic'!
  mix
    ^Mix source: self
  !
  rounded
    ^self roundTo: 1.0
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
  playNamedGraph: graphFileName
    SC3 playFile: (SC3 stsc3HelpGraphFile: graphFileName).
  !
!

!SC3 class methodsFor: 'environment'!
  stsc3Directory
    ^'/home/rohan/sw/stsc3'
  !
  stsc3File: fileName
    ^SC3 stsc3Directory , '/' , fileName
  !
  stsc3HelpGraphFile: graphFileName
    ^SC3 stsc3File: 'help/graph/' , graphFileName
  !
!

!SC3 class methodsFor: 'c smalltalk'!
  stcFileToSt: fileName
    "Translate .stc file to Smalltalk."
    | tmp cmd |
    tmp := '/tmp/stcFileToSt.st'.
    cmd := 'stsc3 translate stc st ' , fileName , ' ' , tmp.
    cmd systemCommand.
    (Delay forMilliseconds: 250) wait. "..."
    ^tmp readTextFile.
  !
  stcToSt: stcText
    | tmp |
    tmp := '/tmp/stcToSt.st'.
    tmp writeTextFile: [ :fd | stcText putOn: fd ].
    ^SC3 stcFileToSt: tmp
  !
  evalStOrStcFile: fileName
    "Evaluate an expression read from a file.  The file may be either a Smalltalk (.st) or a C-Smalltalk (.stc) file"
    | fileText |
    fileText := (fileName endsWith: '.stc') ifTrue: [SC3 stcFileToSt: fileName] ifFalse: [fileName readTextFile].
    ^fileText evalString
  !
  evalText: text fromFile: fileName
    "Evaluate text, deciding if it needs to be translated from C-Smalltalk based on the name of the file the text is from"
    (fileName endsWith: '.stc') ifTrue: [^(SC3 stcToSt: text) evalString] ifFalse: [^text evalString].
  !
  playFile: fileName
    (SC3 evalStOrStcFile: fileName) play
  !
  drawFile: fileName
    (SC3 evalStOrStcFile: fileName) draw
  !
!

AbstractUGen subclass: #Mrg
  instanceVariableNames: 'lhs rhs'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

Mrg comment: 'Multiple Root Graph'!

!Mrg methodsFor: 'collecting'!
  addToUGenSet: c
    lhs addToUGenSet: c.
    rhs addToUGenSet: c
  !
!

!Mrg methodsFor: 'accessing'!
  lhs ^lhs!
  lhs: x lhs := x!
  rhs ^rhs!
  rhs: x rhs := x!
!

!Mrg class methodsFor: 'instance creation'!
  lhs: x rhs: y
    "Multiple root graph.  The left hand input is the signal to be heard, the right hand side is the signal to be stored"
    | u |
    u := self new.
    u lhs: x.
    u rhs: y.
    ^u
  !
  primaryFactoryMethod ^#lhs:rhs:!
!

!AbstractUGen methodsFor: 'multiple root graphs'!
  mrg: rhs ^Mrg lhs: self rhs: rhs!
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
  name ^name!
  name: x name := x!
  init ^init!
  init: x init := x!
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
  k ^k!
  k: x k := x!
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

AbstractUGen subclass: #OverlapTexture
   instanceVariableNames: ''
   classVariableNames: ''
   poolDictionaries: ''
   category: 'Sound-SC3'
!

OverlapTexture comment: 'Texture Generator'!

!OverlapTexture class methodsFor: 'texture generation'!
  graphFunc: graphFunc sustainTime: sustainTime transitionTime: transitionTime overlap: overlap
    ^(1 to: overlap) mixFill: [ :i |
      | t s e |
      t := Impulse freq: (1 / (sustainTime + (transitionTime * 2))) phase: (i / overlap).
      s := graphFunc value: t.
      e := EnvLinen attackTime: transitionTime sustainTime: sustainTime releaseTime: transitionTime level: 1 curves: #(#sin #sin #sin).
      s * (e env gen: t doneAction: 0) ]
  !
  primaryFactoryMethod ^#graphFunc:sustainTime:transitionTime:overlap:!
!

AbstractUGen subclass: #MulAdd
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

MulAdd comment: 'Multiply add'!

!MulAdd class methodsFor: 'instance creation'!
  in: in mul: mul add: add
    in mul: mul.
    in add: add.
    ^in
  !
  primaryFactoryMethod ^#in:mul:add:!
!
