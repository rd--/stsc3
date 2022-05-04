AbstractUgen subclass: #Seq
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Seq comment: 'Sequence generator.'!

!Seq class methodsFor: 'instance creation'!
repeats: repeats list: list
    ^Dseq repeats: repeats list: list!
primaryFactoryMethod
    ^#repeats:list:!
!
