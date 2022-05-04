Ugen subclass: #BinaryOpUgen
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BinaryOpUgen comment: 'Binary Operators'!
!BinaryOpUgen methodsFor: 'initialisation'!
specialIndex: p1 a: p2 b: p3
    ugenName := #BinaryOpUGen. "note case"
    paramNames := #(#a #b).
    mceNames := #().
    numChan := 1.
    ugenRate := #(0 1).
    specialIndex := p1.
    ugenId := nil.
    paramValues := {p2. p3}.
    mceValues := {}!
!


!BinaryOpUgen class methodsFor: 'instance creation'!
specialIndex: specialIndex a: a b: b
    "Create binary operator Ugen"
    ^self new specialIndex: specialIndex a: a b: b!
!



