!Collection methodsFor: 'arithmetic'!
abs
    ^self collect: [ :item | item abs ]!
ceil
    ^self collect: [ :item | item ceil ]!
cos
    ^self collect: [ :item | item cos ]!
cpsMidi
    ^self collect: [ :item | item cpsMidi ]!
cubed
    ^self collect: [ :item | item cubed ]!
dbAmp
    ^self collect: [ :item | item dbAmp ]!
distort
    ^self collect: [ :item | item distort ]!
exp
    ^self collect: [ :item | item exp ]!
frac
    ^self collect: [ :item | item frac ]!
log
    ^self collect: [ :item | item log ]!
midiCps
    ^self collect: [ :item | item midiCps ]!
mean
    "Sum of myself divided by my size."
    ^self sum / self size!
negated
    ^self collect: [ :item | item negated ]!
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
