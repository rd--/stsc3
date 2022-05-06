PrimitiveUgen subclass: #UnaryOpUgen
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

UnaryOpUgen comment: 'Unary Operators'!

!UnaryOpUgen methodsFor: 'initialization'!
specialIndex: p1 a: p2
    name := #UnaryOpUGen. "note case"
    rate := p2 rate.
    numChan := 1.
    specialIndex := p1.
    inputArray := {p2}!
!

!UnaryOpUgen class methodsFor: 'instance creation'!
specialIndex: p1 a: p2
    "Create unary operator Ugen"
    ^self new specialIndex: p1 a: p2!
!
