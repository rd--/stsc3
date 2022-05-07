ScUgen subclass: #BinaryOpUgen
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BinaryOpUgen comment: 'Binary operator unit generator'!

!BinaryOpUgen class methodsFor: 'instance creation'!
specialIndex: specialIndex a: a b: b
    ^UgenBuilder name: 'BinaryOpUGen' numChan: 1 rateSpec: #(1 2) specialIndex: specialIndex inputArray: {a. b}!
!
