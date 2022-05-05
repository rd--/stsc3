PsuedoUgen subclass: #XLn
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

XLn comment: 'Line generator'!

!XLn class methodsFor: 'instance creation'!
start: start end: end dur: dur
    ^XLine start: start end: end dur: dur doneAction: 0!
primaryFactoryMethod
    ^#start:end:dur:!
!



