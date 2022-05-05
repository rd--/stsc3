PsuedoUgen subclass: #AudioIn
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

AudioIn comment: 'Audio input'!

!AudioIn class methodsFor: 'instance creation'!
channels: channels
    ^In numChan: 1 bus: NumOutputBuses new + channels - 1!
primaryFactoryMethod
    ^#channels:!
!



