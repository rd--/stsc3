AbstractUgen subclass: #Ln
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Ln comment: 'Line generator'!

!Ln class methodsFor: 'instance creation'!
start: start end: end dur: dur
    ^Line start: start end: end dur: dur doneAction: 0!
primaryFactoryMethod
    ^#start:end:dur:!
!



