!Collection methodsFor: 'arithmetic'!
abs
    "Absolute values of my elements."
    ^self collect: [ :item | item abs ]!
distort
    ^self collect: [ :item | item distort ]!
midiCps
    "My elements converted from midi note numbers to frequencies in hertz."
    ^self collect: [ :item | item midiCps ]!
dbAmp
    "My elements converted from decibels to linear amplitude."
    ^self collect: [ :item | item dbAmp ]!
mean
    "Sum of myself divided by my size."
    ^self sum / self size!
product
    "All of my elements multiplied togeather."
    ^self inject: 1 into: [ :accumulator :item | accumulator * item ]!
rounded
    ^self collect: [ :item | item rounded ]!
sin
    ^self collect: [ :item | item sin ]!
softClip
    ^self collect: [ :item | item softClip ]!
squared
    ^self collect: [ :item | item squared ]!
tanh
    ^self collect: [ :item | item tanh ]!
** arg
    ^arg adaptToCollection: self andSend: #**!
% arg
    ^arg adaptToCollection: self andSend: #%!
!

!Collection methodsFor: 'mrg'!
mrg: rhs
    (self detect: [ :item | item isUgen] ifNone: [ self error: 'mrg?' ]) mrg: rhs.
    ^self!
!

!Collection methodsFor: 'collecting'!
addToUgenSet: c protectedBy: w
    self do: [ :item | item addToUgenSet: c protectedBy: w]!
!

!Collection methodsFor: 'rate control'!
kr
    self do: [ :item | item kr].
    ^self!
rate
    ^(self collect: [ :item | item rate ]) max!
!
