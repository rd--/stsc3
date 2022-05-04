AbstractUgen subclass: #PenY
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PenY comment: 'No comment'!

!PenY class methodsFor: 'instance creation'!
voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 2 !
primaryFactoryMethod ^#voiceNumber:!
!



