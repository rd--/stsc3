PrimitiveUgen subclass: #BinaryOpUgen
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BinaryOpUgen comment: 'Binary Operators'!

!BinaryOpUgen methodsFor: 'initialisation'!
specialIndex: p1 a: p2 b: p3
    name := #BinaryOpUGen. "note case"
    numChan := 1.
    rate := {p2. p3} rate.
    specialIndex := p1.
    inputArray := {p2. p3}!
!

!BinaryOpUgen class methodsFor: 'instance creation'!
specialIndex: specialIndex a: a b: b
    "Create binary operator Ugen"
    ^self new specialIndex: specialIndex a: a b: b!
!
