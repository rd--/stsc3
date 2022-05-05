PsuedoUgen subclass: #ControlIn
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

ControlIn comment: 'Control input'!

!ControlIn class methodsFor: 'instance creation'!
numChan: numChan bus: bus
    ^(In numChan: numChan bus: bus) kr!
channels: channels
    ^In numChan: 1 bus: channels - 1!
primaryFactoryMethod
    ^#channels:!
!



