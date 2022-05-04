AbstractUgen subclass: #Splay2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Splay2 comment: 'Pan an array of channels across the stereo field.'!

!Splay2 class methodsFor: 'instance creation'!
inArray: inArray
    ^Splay input: inArray spread: 1 level: 1 center: 0 levelComp: true!
primaryFactoryMethod
    ^#inArray:!
!
