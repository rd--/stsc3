PsuedoUgen subclass: #PenRadius
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PenRadius comment: 'No comment'!

!PenRadius class methodsFor: 'instance creation'!
voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 5 !
primaryFactoryMethod ^#voiceNumber:!
!



