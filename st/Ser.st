AbstractUgen subclass: #Ser
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Ser comment: 'Demand rate arithmetic series Ugen.'!

!Ser class methodsFor: 'instance creation'!
repeats: repeats list: list
    ^Dser repeats: repeats list: list!
primaryFactoryMethod
    ^#repeats:list:!
!
