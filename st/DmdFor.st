AbstractUgen subclass: #DmdFor
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

DmdFor comment: 'Demand with duration input (Duty).'!

!DmdFor class methodsFor: 'instance creation'!
dur: dur reset: reset level: level
    ^Duty dur: dur reset: reset doneAction: 0 level: level
!
primaryFactoryMethod
    ^#dur:reset:level:
!
!



