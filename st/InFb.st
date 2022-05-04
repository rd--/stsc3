Ugen subclass: #InFb
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

InFb comment: 'Read signal from a bus with a current or one cycle old timestamp.'!

!InFb class methodsFor: 'instance creation'!
numChan: numChan bus: bus
    ^InFeedback numChan: numChan bus: bus!
primaryFactoryMethod
    ^#numChan:bus:!
!



