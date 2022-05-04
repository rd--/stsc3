AbstractUgen subclass: #PenX
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PenX comment: 'No comment'!

!PenX class methodsFor: 'instance creation'!
voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 1 !
primaryFactoryMethod ^#voiceNumber:!
!



