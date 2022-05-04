AbstractUgen subclass: #Mrg
  instanceVariableNames: 'lhs rhs'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
!

Mrg comment: 'Multiple Root Graph'!

!Mrg methodsFor: 'collecting'!
addToUgenSet: aSet
    lhs addToUgenSet: aSet.
    rhs addToUgenSet: aSet
!!

!Mrg methodsFor: 'accessing'!
lhs
    ^lhs
!
lhs: aSignal
    lhs := aSignal
!
rhs
    ^rhs
!
rhs: aSignal
    rhs := aSignal
!!

!Mrg class methodsFor: 'instance creation'!
lhs: x rhs: y
    "Multiple root graph.  The left hand input is the signal to be heard, the right hand side is the signal to be stored"
    | u |
    u := self new.
    u lhs: x.
    u rhs: y.
    ^u
!
primaryFactoryMethod
    ^#lhs:rhs:
!!
