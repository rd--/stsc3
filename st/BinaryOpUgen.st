PrimitiveUgen subclass: #BinaryOpUgen
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BinaryOpUgen comment: 'Binary Operators'!

!BinaryOpUgen class methodsFor: 'instance creation'!
specialIndex: specialIndex a: a b: b
    "This is special cased so it can optimize constant math..."
    ^UgenBuilder name: 'BinaryOpUGen' numChan: 1 rateSpec: #(1 2) specialIndex: specialIndex inputArray: {a. b}!
!
