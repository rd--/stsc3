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
    aNumber isUgen ifTrue: [ ^BinaryOpUgen selector: #% a: self b: aNumber ] ifFalse: [ ^self \\ aNumber ]!
linExpFrom: lo to: hi
    "Myself (I am in range 0.0 to 1.0) mapped to the indicated range following an exponential curve."
    ^lo * ((hi / lo) ln * self) exp!
!

!Number methodsFor: 'collecting'!
addToUgenSet: c protectedBy: w
    c add: self.
    ^self!
!

!Number methodsFor: 'rate control'!
kr
    ^self!
rate
    ^Rate ir!
!