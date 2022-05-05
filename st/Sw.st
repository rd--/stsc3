PsuedoUgen subclass: #Sw
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Sw comment: 'Switch (maintained or momentary) controller input'!

!Sw class methodsFor: 'instance creation'!
index: index
    ^(In numChan: 1 bus: 12000 + index) kr!
primaryFactoryMethod
    ^#index:!
!



