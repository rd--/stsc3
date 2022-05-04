Object subclass: #AbstractUgen
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

AbstractUgen comment: 'Unit Generator (Abstract)'!

!AbstractUgen class methodsFor: 'instance creation'!
apply: argumentsArray
    "Ugens have primary factory methods."
    ^self perform: (self primaryFactoryMethod) withArguments: argumentsArray! !

!AbstractUgen methodsFor: 'accessing'!
paramNameFromIndex: index
    "Get parameter name given index.
     Parameter names are separated into paramNames and mceNames.
     The implicit names mul: and add: follow."
    | n m |
    n := self paramNames size.
    m := self mceNames size.
    index <= n ifTrue: [^self paramNames at: index].
    index <= (n + m) ifTrue: [^self mceNames at: index - n].
    ^#(#mul #add) at: index - n - m! !

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
    ^rcvr asUgen perform: selector with: self! !

!AbstractUgen methodsFor: 'testing'!
isZero
    "In the general case false, c.f. Constant.
     Required for general Number / Ugen math."
    ^false! !

!AbstractUgen methodsFor: 'math'!
** aNumber
    ^self raisedTo: aNumber!
@ aUgen
    ^Point x: self y: aUgen!
unitCps
    ^(self * 127) midiCps! !

!AbstractUgen methodsFor: 'collecting'!
addToUgenSet: c
    "Add inputs to Ugen set.  Only actual instances of Ugen are collected."
    self subclassResponsibility!
ugenSet
    | c |
    c := IdentitySet new.
    self addToUgenSet: c.
    ^c! !

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
    ^self asMce at: 2! !

!AbstractUgen methodsFor: 'rate control'!
ar
    ^Rate source: self rate: #ar!
kr
    ^Rate source: self rate: #kr!
ir
    ^Rate source: self rate: #ir!
dr
    ^Rate source: self rate: #dr! !


!AbstractUgen methodsFor: 'transforms'!
reverse
    "Mce reverse"
    ^self asMce reverse!
transpose
    "Mce transpose"
    ^self asMce transpose!
product
    ^self asMce product! !

!AbstractUgen methodsFor: 'arithmetic'!
mix
    ^Mix source: self!
sum
    ^Mix source: self!
rounded
    ^self roundTo: 1.0! !

!AbstractUgen methodsFor: 'multiple root graphs'!
mrg: rhs
    ^Mrg lhs: self rhs: rhs! !

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
    ^aClass apply: arguments! !
