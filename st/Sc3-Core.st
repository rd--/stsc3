Object subclass: #AbstractUgen
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
!

AbstractUgen comment: 'Unit Generator (Abstract)'!

!AbstractUgen class methodsFor: 'instance creation'!
apply: argumentsArray
    ^self perform: (self primaryFactoryMethod) withArguments: argumentsArray
!
!

!AbstractUgen methodsFor: 'filtering'!
  doesNotUnderstand: aMessage
    "Reflective form of filter variants for Ugen constructors.
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
  apply: argumentsArray
    "For .stc notation.
     Allow f(x,y) as notation to apply a block f to arguments x & y."
    ^self valueWithArguments: argumentsArray
  !
!

!AbstractUgen methodsFor: 'accessing'!
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
    "Set Ugen parameters from an array of either ugen inputs or (name,input) associations."
    d withIndexDo: [:e :i |
      (e class = Association)
        ifTrue: [self perform: e key with: e value]
        ifFalse: [self instVarNamed: (self paramNameFromIndex: i) put: e]]
  !
!

!AbstractUgen methodsFor: 'converting'!
  adaptToInteger: rcvr andSend: selector
    "If I am involved in arithmetic with an Integer, convert it to a Ugen."
    ^rcvr asUgen perform: selector with: self
  !
  adaptToFraction: rcvr andSend: selector
    "If I am involved in arithmetic with a Fraction, convert it to a Ugen."
    ^rcvr asUgen perform: selector with: self
  !
  adaptToFloat: rcvr andSend: selector
    "If I am involved in arithmetic with a Float, convert it to a Ugen."
    ^rcvr asUgen perform: selector with: self
  !
  adaptToCollection: rcvr andSend: selector
    "If I am involved in arithmetic with a Collection, convert it to a Ugen."
    ^rcvr asUgen perform: selector with: self
  !
!

!AbstractUgen methodsFor: 'testing'!
  isZero
    "In the general case false, c.f. Constant.
     Required for general Number / Ugen math."
    ^false
  !
!

!AbstractUgen methodsFor: 'math'!
  ** aNumber
    ^self raisedTo: aNumber
  !
  @ aUgen
    ^Point x: self y: aUgen
  !
  unitCps
    ^(self * 127) midiCps
  !
!

!Number methodsFor: 'math'!
  % aNumber
    "% is the SC floating point modulo operator."
    ^self asUgen % aNumber
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

!AbstractUgen methodsFor: 'collecting'!
  addToUgenSet: c
    "Add inputs to Ugen set.  Only actual instances of Ugen are collected."
    self subclassResponsibility
  !
  ugenSet
    | c |
    c := IdentitySet new.
    self addToUgenSet: c.
    ^c
  !
!

!AbstractUgen methodsFor: 'multi-channel expansion'!
  asMce
    "Mark Ugen as Mce.
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

AbstractUgen subclass: #Ugen
  instanceVariableNames: 'ugenName paramNames mceNames numChan ugenRate specialIndex ugenId'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
!

Ugen comment: 'Unit Generator.
ugenId is assigned during graph serialisation since identityHash is not unique.
All Ugens have mul: and add: methods but there are no variables associated with these.
'!

!Ugen methodsFor: 'accessing'!
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

!Ugen methodsFor: 'collecting'!
  addToUgenSet: c
    | addFunc |
    addFunc := [ :x | (self perform: x) addToUgenSet: c ].
    c add: self.
    paramNames do: addFunc.
    mceNames do: addFunc.
  !
!

!Ugen methodsFor: 'arithmetic'!
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

Ugen subclass: #BinaryOpUGen
  instanceVariableNames: 'a b'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
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
    "Create binary operator Ugen"
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

Ugen subclass: #UnaryOpUGen
  instanceVariableNames: 'a'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
!

UnaryOpUGen comment: 'Unary Operators'!

!UnaryOpUGen methodsFor: 'accessing'!
  a ^a!
  a: x a := x!
!

!UnaryOpUGen class methodsFor: 'instance creation'!
  specialIndex: specialIndex a: a
    "Create unary operator Ugen"
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

AbstractUgen subclass: #Rate
  instanceVariableNames: 'source rate'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
!

Rate comment: 'Ugen Processing Rate'!

!Rate methodsFor: 'collecting'!
  addToUgenSet: c
    source addToUgenSet: c
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

!AbstractUgen methodsFor: 'rate control'!
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
    ^self asUgen kr
  !
!

AbstractUgen subclass: #Mce
  instanceVariableNames: 'source transform'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
!

Mce comment: 'Multiple Channel Expansion'!

!Array methodsFor: 'collecting'!
  addToUgenSet: c
    self do: [:x | x addToUgenSet: c]
  !
!

!Interval methodsFor: 'collecting'!
  addToUgenSet: c
    self asArray addToUgenSet: c
  !
!

!Mce methodsFor: 'collecting'!
  addToUgenSet: c
    source addToUgenSet: c
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

!AbstractUgen methodsFor: 'transforms'!
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
  asUgen
    ^self asArray asUgen
  !
!

!Array methodsFor: 'multi-channel expansion'!
  asUgen
    "Array as Ugen (ie. Mce)"
    ^Mce source: self
  !
!

!Number methodsFor: 'enumerating'!
  mceFill: f
    ^(self arrayFill: f) asUgen
  !
!

!Interval methodsFor: 'enumerating'!
  mceFill: aBlock
    ^(self asArray collect: aBlock) asUgen
  !
!

AbstractUgen subclass: #Mix
  instanceVariableNames: 'source'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
!

Mix comment: 'Ugen Mixer'!

!Mix methodsFor: 'collecting'!
  addToUgenSet: c
    source addToUgenSet: c
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

!AbstractUgen methodsFor: 'arithmetic'!
  mix
    ^Mix source: self
  !
  sum
    ^Mix source: self
  !
  rounded
    ^self roundTo: 1.0
  !
!

Object subclass: #Sc3
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
!

Sc3 comment: 'SuperCollider 3'!

!Sc3 class methodsFor: 'server control'!
reset
    "Reset Sc synthesiser."
    'hsc3-scsynth reset' systemCommand
!
playHelpGraphFile: graphFile
    "Play help file."
    Sc3 playFile: (Sc3 stsc3HelpGraphFile: graphFile).
!
!

!Sc3 class methodsFor: 'environment'!
stsc3Directory
    "Name of stsc3 directory."
    ^'/home/rohan/sw/stsc3'
!
stsc3File: fileName
    "Absolute file name of name given relative to the stsc3 directory."
    ^Sc3 stsc3Directory , '/' , fileName
!
stsc3HelpGraphFile: graphFileName
    "Absolute file name of name given relative to the help graph directory of stsc3."
    ^Sc3 stsc3File: 'help/graph/' , graphFileName
!
!

!Sc3 class methodsFor: 'c-smalltalk'!
stcFileToSt: fileName
    "Translate named C-Smalltalk (.stc) file to Smalltalk (.st), returning translated text."
    | tmp cmd |
    tmp := '/tmp/stcFileToSt.st'.
    cmd := 'stsc3 translate stc st ' , fileName , ' ' , tmp.
    cmd systemCommand.
    (Delay forMilliseconds: 250) wait. "..."
    ^tmp readTextFile.
!
stcToSt: stcText
    "Translate .stc text to .st text."
    | tmp |
    tmp := '/tmp/stcToSt.st'.
    tmp writeTextFile: [ :fd | stcText putOn: fd ].
    ^Sc3 stcFileToSt: tmp
!
evalStOrStcFile: fileName
    "Evaluate an expression read from a file.  The file may be either a Smalltalk (.st) or a C-Smalltalk (.stc) file"
    | fileText |
    fileText := (fileName endsWith: '.stc') ifTrue: [Sc3 stcFileToSt: fileName] ifFalse: [fileName readTextFile].
    ^fileText evalString
  !
evalText: text fromFile: fileName
    "Evaluate text, deciding if it needs to be translated from C-Smalltalk based on the name of the file the text is from"
    (fileName endsWith: '.stc') ifTrue: [^(Sc3 stcToSt: text) evalString] ifFalse: [^text evalString].
!
playFile: fileName
    "Play expression loaded from named file, which may be either an .st or .stc file."
    (Sc3 evalStOrStcFile: fileName) play
!
drawFile: fileName
    "Draw expression loaded from named file, which may be either an .st or .stc file."
    (Sc3 evalStOrStcFile: fileName) draw
!
!

AbstractUgen subclass: #Mrg
  instanceVariableNames: 'lhs rhs'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
!

Mrg comment: 'Multiple Root Graph'!

!Mrg methodsFor: 'collecting'!
  addToUgenSet: c
    lhs addToUgenSet: c.
    rhs addToUgenSet: c
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

!AbstractUgen methodsFor: 'multiple root graphs'!
  mrg: rhs ^Mrg lhs: self rhs: rhs!
!

!SequenceableCollection methodsFor: 'multiple root graphs'!
  mrg: rhs ^Mrg lhs: self rhs: rhs!
!

AbstractUgen subclass: #Control
  instanceVariableNames: 'name init'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
!

Control comment: 'Control Input'!

!Control methodsFor: 'collecting'!
  addToUgenSet: c
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

AbstractUgen subclass: #Constant
  instanceVariableNames: 'k'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
!

Constant comment: 'Constant Numerical Value'!

!Constant methodsFor: 'testing'!
  isZero
    ^k isZero
  !
!

!Constant methodsFor: 'collecting'!
  addToUgenSet: c
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

!Number methodsFor: 'Ugen constants'!
  asUgen
    ^Constant k: self
  !
!

!Number methodsFor: 'collecting'!
  addToUgenSet: c
  !
!

!Symbol methodsFor: 'collecting'!
  addToUgenSet: c
  !
!

!Boolean methodsFor: 'collecting'!
  addToUgenSet: c
  !
!

AbstractUgen subclass: #OverlapTexture
   instanceVariableNames: ''
   classVariableNames: ''
   poolDictionaries: ''
   category: 'Sound-Sc3'
!

OverlapTexture comment: 'Texture Generator'!

!OverlapTexture class methodsFor: 'texture generation'!
  graphFunc: graphFunc sustainTime: sustainTime transitionTime: transitionTime overlap: overlap
    ^(1 to: overlap) mixFill: [ :i |
      | t s e |
      t := Impulse freq: (1 / (sustainTime + (transitionTime * 2))) phase: (i / overlap).
      s := graphFunc value: t kr.
      e := EnvLinen attackTime: transitionTime sustainTime: sustainTime releaseTime: transitionTime level: 1 curve: #sin.
      s * (e asEnvGen: t withDoneAction: 0) ]
  !
  primaryFactoryMethod ^#graphFunc:sustainTime:transitionTime:overlap:!
!

!BlockClosure methodsFor: 'texture generation'!
  overlapTexture: sustainTime value: transitionTime value: overlap
    ^OverlapTexture graphFunc: self sustainTime: sustainTime transitionTime: transitionTime overlap: overlap
  !
!

!BlockClosure methodsFor: 'scsynth'!
  play
    self value play
  !
  draw
    self value draw
  !
!
