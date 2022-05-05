PsuedoUgen subclass: #KeyTimbre
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

KeyTimbre comment: 'No comment'!

!KeyTimbre class methodsFor: 'instance creation'!
voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 2 !
primaryFactoryMethod ^#voiceNumber:!
!



