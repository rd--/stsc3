PsuedoUgen subclass: #Shuf
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Shuf comment: 'Demand rate random sequence generator'!

!Shuf class methodsFor: 'instance creation'!
repeats: repeats list: list
    ^Dshuf repeats: repeats list: list!
primaryFactoryMethod
    ^#repeats:list:!
!
