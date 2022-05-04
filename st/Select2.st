AbstractUgen subclass: #Select2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Select2 comment: 'Select one of two branches based on predicate signal'!

!Select2 class methodsFor: 'instance creation'!
predicate: predicate ifTrue: trueUgen ifFalse: falseUgen
    ^(predicate * (trueUgen - falseUgen)) + falseUgen!
primaryFactoryMethod
    ^#predicate:ifTrue:ifFalse:!
!



