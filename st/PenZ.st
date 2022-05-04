AbstractUgen subclass: #PenZ
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PenZ comment: 'No comment'!

!PenZ class methodsFor: 'instance creation'!
voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 3 !
primaryFactoryMethod ^#voiceNumber:!
!



