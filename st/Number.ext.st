!Number methodsFor: 'enumerating'!
iota
    "Array of numbers from 1 to myself."
    ^(1 to: self) asArray!
from: start by: step
    "Array of myself places from start by step."
    ^(start to: self - 1 * step + start by: step) asArray!
!

!Number methodsFor: 'arithmetic'!
midiCps
    "Convert from fractional midi note number to frequency (in hertz)"
    ^440 * (2 raisedTo: ((self - 69) * (1 / 12)))!
cpsMidi
    "Cycles per second to fractional midi note number."
    ^(((self * (1 / 440)) log: 2) * 12) + 69!
dbAmp
    "Decibels to linear amplitude"
    ^10 raisedTo: (self * 0.05)!
ampDb
    "Linear amplitude to decibels"
    ^(self log: 10) * 20!
% aNumber
    "% is the Sc floating point modulo operator."
    ^self asUgen % aNumber!
linExpFrom: lo to: hi
    "Myself (I am in range 0.0 to 1.0) mapped to the indicated range following an exponential curve."
    ^lo * ((hi / lo) ln * self) exp!
!

!Number methodsFor: 'collecting'!
addToUgenSet: aSet
    ^nil!
!

!Number methodsFor: 'Ugen constants'!
asUgen
    ^Constant k: self!
!

!Number methodsFor: 'enumerating'!
mceFill: aBlock
    ^(self arrayFill: aBlock) asUgen!
!

!Number methodsFor: 'printing'!
printHaskellOn: fd
    "Print my hsc3 representation."
    self asString putOn: fd
  !
!

