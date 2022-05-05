PsuedoUgen subclass: #Choose
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Choose comment: 'Random sequence generator.'!

!Choose class methodsFor: 'instance creation'!
repeats: repeats list: list
    ^Drand repeats: repeats list: list!
primaryFactoryMethod
    ^#repeats:list:!
!



