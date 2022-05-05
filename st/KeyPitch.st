PsuedoUgen subclass: #KeyPitch
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

KeyPitch comment: 'No comment'!

!KeyPitch class methodsFor: 'instance creation'!
voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 7 !
primaryFactoryMethod ^#voiceNumber:!
!



