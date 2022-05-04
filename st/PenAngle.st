AbstractUgen subclass: #PenAngle
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PenAngle comment: 'No comment'!

!PenAngle class methodsFor: 'instance creation'!
voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 4 !
primaryFactoryMethod ^#voiceNumber:!
!



