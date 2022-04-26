AbstractUgen subclass: #Constant
  instanceVariableNames: 'k'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
!

Constant comment: 'Constant Numerical Value'!

!Constant methodsFor: 'testing'!
  isZero
    ^k isZero
  !
!

!Constant methodsFor: 'collecting'!
  addToUgenSet: c
  !
!

!Constant methodsFor: 'accessing'!
  k ^k!
  k: x k := x!
!

!Constant class methodsFor: 'instance creation'!
  k: k
    | u |
    u := self new.
    u k: k.
    ^u
  !
!

