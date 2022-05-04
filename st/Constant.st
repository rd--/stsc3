AbstractUgen subclass: #Constant
  instanceVariableNames: 'k'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Constant comment: 'Constant Numerical Value'!
!Constant methodsFor: 'accessing'!
k
    ^k!
k: x
    k := x!
!

!Constant methodsFor: 'collecting'!
addToUgenSet: c
    ^nil!
!

!Constant methodsFor: 'printing'!
printHaskellOn: fd
    "Print my hsc3 representation."
    fd
      nextPutAll: '(constant (';
      nextPutAll: self k asString;
      nextPutAll: ')) '
  !
!

!Constant methodsFor: 'testing'!
isZero
    ^k isZero!
!


!Constant class methodsFor: 'instance creation'!
k: k
    ^self new k: k!
!



