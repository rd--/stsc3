PsuedoUgen subclass: #KeyVelocity
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

KeyVelocity comment: 'No comment'!

!KeyVelocity class methodsFor: 'instance creation'!
voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 3 !
primaryFactoryMethod ^#voiceNumber:!
!



