AbstractUgen subclass: #Cc
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Cc comment: 'Continuous controller input'!

!Cc class methodsFor: 'instance creation'!
index: index
    ^(In numChan: 1 bus: 11000 + index) kr!
primaryFactoryMethod
    ^#index:!
!



