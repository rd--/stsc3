Object subclass: #Ugen
  instanceVariableNames: 'scUgen port'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Ugen comment: 'Output proxy'!

!Ugen methodsFor: 'accessing'!
scUgen
    ^scUgen!
port
    ^port!
!

!Ugen methodsFor: 'initialization'!
scUgen: anScUgen port: anInteger
    scUgen := anScUgen.
    port := anInteger!
!

!Ugen class methodsFor: 'instance creation'!
scUgen: anScUgen port: anInteger
    ^self new scUgen: anScUgen port: anInteger!
!

!Ugen methodsFor: 'rate control'!
rate
    ^scUgen rate!
kr
    scUgen kr.
    ^self!
!

!Ugen methodsFor: 'mrg'!
mrg: rhs
    scUgen mrg: rhs.
    ^self!
!

!Ugen methodsFor: 'testing'!
isUgen
    ^true!
isOutUgen
    ^scUgen name = 'Out'!
isZero
    ^false!
!

!Ugen methodsFor: 'collecting'!
addToUgenSet: c protectedBy: w
    scUgen addToUgenSet: c protectedBy: w!
!

!Ugen methodsFor: 'arithmetic'!
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
** aNumber
    ^self raisedTo: aNumber!
@ aUgen
    ^Point x: self y: aUgen!
unitCps
    ^(self * 127) midiCps!
!

!Ugen methodsFor: 'buffer management'!
clearBuf
    "ClearBuf does not copy the buffer number through so this is an Mrg node."
    ^self mrg: (ClearBuf buf: self)!
!

!Ugen methodsFor: 'converting'!
adaptToInteger: rcvr andSend: selector
    ^BinaryOpUgen selector: selector a: rcvr b: self!
adaptToFraction: rcvr andSend: selector
    ^BinaryOpUgen selector: selector a: rcvr b: self!
adaptToFloat: rcvr andSend: selector
    ^BinaryOpUgen selector: selector a: rcvr b: self!
adaptToCollection: rcvr andSend: selector
    ^rcvr collect: [ :item | item perform: selector with: self]!
!

!Ugen methodsFor: 'sc3'!
play
    Sc3 play: self!
draw
    Sc3 draw: self!
printUgens
    Sc3 printUgens: self!
!

!Ugen methodsFor: 'convenience'!
lag: lagTime
    ^Lag in: self lagTime: lagTime!
splay2
    ^Splay2 inArray: self!
!
