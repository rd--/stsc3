Object subclass: #AbstractUgen
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

AbstractUgen comment: 'Unit Generator (Abstract)'!

!AbstractUgen methodsFor: 'arithmetic'!
mul: x
      ^x = 1 ifTrue: [ self ] ifFalse: [ self * x ]
!
add: x
      ^x = 0 ifTrue: [ self ] ifFalse: [ self + x ]
!
mul: mul add: add
    ^(self mul: mul) add: add!
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
    "Add inputs to Ugen set.  Only instances of PrimitiveUgen are collected."
    ^nil!
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
    "If I am involved in arithmetic with a Collection, return a Collection of the results of each element combined with me in that expression."
    ^rcvr collect: [:element | element perform: selector with: self]
!

!AbstractUgen methodsFor: 'filtering'!
doesNotUnderstand: aMessage
    "Reflective form of filter variants for Ugen constructors.
     Examines only the first part of the message selector, and if that, when capitalised, names a class, applies it.
     WhiteNoise new hPZ1 pan2: 0 level: 0.05
     To be removed.
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

!AbstractUgen methodsFor: 'multiple root graphs'!
mrg: rhs
    ^Mrg lhs: self rhs: rhs!
!

!AbstractUgen methodsFor: 'panning'!
splay2
    ^Splay2 inArray: self!
!

!AbstractUgen methodsFor: 'sc3'!
play
    Sc3 play: self!
draw
    Sc3 draw: self!
printUgens
    Sc3 printUgens: self!
!

!AbstractUgen methodsFor: 'smoothing'!
lag: lagTime
    "Convenience"
    ^Lag in: self lagTime: lagTime!
!

!AbstractUgen methodsFor: 'testing'!
isZero
    ^false!
!

!AbstractUgen class methodsFor: 'instance creation'!
apply: argumentsArray
    "Ugens have primary factory methods."
    ^self perform: (self primaryFactoryMethod) withArguments: argumentsArray!
!
