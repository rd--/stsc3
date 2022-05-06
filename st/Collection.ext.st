!Collection methodsFor: 'arithmetic'!
abs
    "Absolute values of my elements."
    ^self collect: [ :item | item abs ]!
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
    "All of my elements multipled togeather."
    ^self inject: 1 into: [ :accumulator :item | accumulator * item ]!
** arg
    "** is the Sc exponentiation operators.  C.f. raisedTo:"
    ^arg adaptToCollection: self andSend: #**!
% arg
    "% is the Sc floating point modulo operator.  C.f. //"
    ^arg adaptToCollection: self andSend: #%!
!

!Collection methodsFor: 'alias'!
mix
    "Alias for sum.  To be removed."
    ^self sum!
!

