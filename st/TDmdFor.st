AbstractUgen subclass: #TDmdFor
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

TDmdFor comment: 'Demand with duration input (Duty).'!

!TDmdFor class methodsFor: 'instance creation'!
dur: dur reset: reset level: level
    ^TDuty dur: dur reset: reset doneAction: 0 level: level gapFirst: 0
!
primaryFactoryMethod
    ^#dur:reset:level:
!
!



