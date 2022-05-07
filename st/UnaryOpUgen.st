PrimitiveUgen subclass: #UnaryOpUgen
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

UnaryOpUgen comment: 'Unary Operators'!

!UnaryOpUgen class methodsFor: 'instance creation'!
specialIndex: specialIndex a: a
    "This is special cased so it can optimize constant math..."
    ^UgenBuilder name: 'UnaryOpUGen' numChan: 1 rateSpec: #(1) specialIndex: specialIndex inputArray: {a}!
!
