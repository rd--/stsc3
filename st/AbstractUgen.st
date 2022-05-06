Object subclass: #AbstractUgen
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

AbstractUgen comment: 'Unit Generator (Abstract)'!

!AbstractUgen methodsFor: 'arithmetic'!
mul: x
     "((p mul: q) mul: r) is (p * q * r) not (p * r)."
      ^x = 1 ifTrue: [ self ] ifFalse: [ self * x ]
!
add: x
     "((p add: q) add: r) is (p + q + r) not (p + r)."
      ^x = 0 ifTrue: [ self ] ifFalse: [ self + x ]
!
mul: mul add: add
    ^(self mul: mul) add: add!
mix
    ^Mix source: self!
sum
    ^Mix source: self!
rounded
    ^self roundTo: 1.0!
inExpRangeFrom: x to: y
    "LinExp with input range of (-1,1)"
    ^LinExp in: self srclo: (-1) srchi: 1 dstlo: x dsthi: y!
inRangeFrom: x to: y
    "LinLin with input range of (-1,1)"
    | m a |
    m := (y - x) / 2.
    a := x + m.
    ^self * m + a!
exprange: anAssoc
    "inExpRangeFrom:to: from Association"
    ^self inExpRangeFrom: anAssoc key to: anAssoc value!
range: anAssoc
    "inRangeFrom:to: from Association"
    ^self inRangeFrom: anAssoc key to: anAssoc value!
exprange: lo value: hi
    ".stc exprange"
    ^self inExpRangeFrom: lo to: hi!
range: lo value: hi
    ".stc range"
    ^self inRangeFrom: lo to: hi!
!

!AbstractUgen methodsFor: 'buffer management'!
clearBuf
    "ClearBuf does not copy the buffer number through so this is an Mrg node."
    ^Mrg lhs: self rhs: (ClearBuf buf: self)!
!

!AbstractUgen methodsFor: 'collecting'!
addToUgenSet: c
    "Add inputs to Ugen set.  Only actual instances of Ugen are collected."
    self subclassResponsibility!
ugenSet
    | c |
    c := IdentitySet new.
    self addToUgenSet: c.
    ^c!
!

!AbstractUgen methodsFor: 'converting'!
adaptToInteger: rcvr andSend: selector
    "If I am involved in arithmetic with an Integer, convert it to a Ugen."
    ^rcvr asUgen perform: selector with: self!
adaptToFraction: rcvr andSend: selector
    "If I am involved in arithmetic with a Fraction, convert it to a Ugen."
    ^rcvr asUgen perform: selector with: self!
adaptToFloat: rcvr andSend: selector
    "If I am involved in arithmetic with a Float, convert it to a Ugen."
    ^rcvr asUgen perform: selector with: self!
adaptToCollection: rcvr andSend: selector
    "If I am involved in arithmetic with a Collection, convert it to a Ugen."
    ^rcvr asUgen perform: selector with: self!
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
    ^aClass apply: arguments!
!

!AbstractUgen methodsFor: 'math'!
** aNumber
    ^self raisedTo: aNumber!
@ aUgen
    ^Point x: self y: aUgen!
unitCps
    ^(self * 127) midiCps!
!

!AbstractUgen methodsFor: 'multi-channel expansion'!
asMce
    "Mark Ugen as Mce.
     This is necessary because the current implementation defers expansion to hsc3."
    ^Mce source: self!
transpose
    ^self asMce transpose!
at: x
    ^self asMce at: x!
clump: x
    ^self asMce clump: x!
rotate: x
    ^self asMce rotate: x!
dup: x
    x replicate: self!
dup
    self dup: 2!
first
    "First channel.  Useful for expressions that need to work with both 0 and 1 based index systems."
    ^self asMce at: 1!
second
    "Second channel.  Useful for expressions that need to work with both 0 and 1 based index systems."
    ^self asMce at: 2!
!

!AbstractUgen methodsFor: 'multiple root graphs'!
mrg: rhs
    ^Mrg lhs: self rhs: rhs!
!

!AbstractUgen methodsFor: 'panning'!
splay2
    ^Splay2 inArray: self!
!

!AbstractUgen methodsFor: 'printing'!
writeHaskellTo: fn
    "Write my hsc3 representation to a file."
    | c |
    c := self ugenSet.
    c asArray with: (1 to: c size) do: [ :u :i | u ugenId: i ]. "<- assign id to Ugens for serialisation"
    fn writeTextFile: [ :fd |
     'let {' putOn: fd.
     c do: [:x | x printHaskellOn: fd. ' = ' putOn: fd. x printHaskellDefinitionOn: fd.] separatedBy: [fd cr. '; ' putOn: fd].
     '} in ' putOn: fd.
     self printHaskellOn: fd.
     fd cr]!
play
    "Write my hsc3 representation to a temporary file and run hsc3-graphs to play me."
    self writeHaskellTo: '/tmp/st.hs'.
    'hsc3-graphs fragments hs min play /tmp/st.hs' systemCommand!
draw
    "Write my hsc3 representation to a temporary file and run hsc3-graphs to draw me."
    self writeHaskellTo: '/tmp/st.hs'.
    'hsc3-graphs fragments hs min draw /tmp/st.hs' systemCommand!
printUgens
    "Write my hsc3 representation to a temporary file and run hsc3-graphs to print me."
    self writeHaskellTo: '/tmp/st.hs'.
    'hsc3-graphs fragments hs min dump-ugens /tmp/st.hs' systemCommand!
!

!AbstractUgen methodsFor: 'rate control'!
ar
    ^Rate source: self rate: #ar!
kr
    ^Rate source: self rate: #kr!
ir
    ^Rate source: self rate: #ir!
dr
    ^Rate source: self rate: #dr!
!

!AbstractUgen methodsFor: 'selecting'!
select2: trueUgen ifFalse: falseUgen
    ^Select2 predicate: self ifTrue: trueUgen ifFalse: falseUgen!
select2: trueUgen value: falseUgen
    "For .stc notation"
    ^Select2 predicate: self ifTrue: trueUgen ifFalse: falseUgen!
!

!AbstractUgen methodsFor: 'smoothing'!
lag: lagTime
    ^Lag in: self lagTime: lagTime!
!

!AbstractUgen methodsFor: 'testing'!
isZero
    "In the general case false, c.f. Constant.
     Required for general Number / Ugen math."
    ^false!
!

!AbstractUgen methodsFor: 'transforms'!
reverse
    "Mce reverse"
    ^self asMce reverse!
transpose
    "Mce transpose"
    ^self asMce transpose!
product
    ^self asMce product!
!

!AbstractUgen class methodsFor: 'instance creation'!
apply: argumentsArray
    "Ugens have primary factory methods."
    ^self perform: (self primaryFactoryMethod) withArguments: argumentsArray!
!
