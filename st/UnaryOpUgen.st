ScUgen subclass: #UnaryOpUgen
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

UnaryOpUgen comment: 'Unary operator unit generator'!

!UnaryOpUgen class methodsFor: 'instance creation'!
specialIndex: specialIndex a: a
    ^UgenBuilder name: 'UnaryOpUGen' numChan: 1 rateSpec: #(1) specialIndex: specialIndex inputArray: {a}!
!
