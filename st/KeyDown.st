AbstractUgen subclass: #KeyDown
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

KeyDown comment: 'No comment'!

!KeyDown class methodsFor: 'instance creation'!
voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 0 !
primaryFactoryMethod ^#voiceNumber:!
!



