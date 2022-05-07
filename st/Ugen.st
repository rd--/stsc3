Object subclass: #Ugen
  instanceVariableNames: 'scUgen port'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Ugen comment: 'Output proxy'!

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
!

!Ugen methodsFor: 'collecting'!
addToUgenSet: c protectedBy: w
    scUgen addToUgenSet: c protectedBy: w!
!

!Ugen methodsFor: 'printing'!
printHaskellOn: fd
    "Print my Hsc3 form."
    '(mceChannel ' putOn: fd. (port - 1) asString putOn: fd. ' ' putOn: fd. scUgen printHaskellOn: fd. ') ' putOn: fd!
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

"???"
!Ugen methodsFor: 'converting'!
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

!Ugen methodsFor: 'panning'!
splay2
    ^Splay2 inArray: self!
!

!Ugen methodsFor: 'sc3'!
play
    Sc3 play: self!
draw
    Sc3 draw: self!
printUgens
    Sc3 printUgens: self!
!

!Ugen methodsFor: 'smoothing'!
lag: lagTime
    "Convenience"
    ^Lag in: self lagTime: lagTime!
!

!Ugen methodsFor: 'testing'!
isZero
    ^false!
!
