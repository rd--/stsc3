PsuedoUgen subclass: #PenDown
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PenDown comment: 'No comment'!

!PenDown class methodsFor: 'instance creation'!
voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 0 !
primaryFactoryMethod ^#voiceNumber:!
!



