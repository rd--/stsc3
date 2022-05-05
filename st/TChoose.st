PsuedoUgen subclass: #TChoose
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

TChoose comment: 'Randomly select one of several inputs on trigger.'!

!TChoose class methodsFor: 'instance creation'!
trig: trig array: array
    ^Select which: (TIRand lo: 0 hi: (array size - 1) trig: trig) array: array!
primaryFactoryMethod
    ^#trig:array:!
!



