AbstractUgen subclass: #Mrg
  instanceVariableNames: 'lhs rhs'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
!

Mrg comment: 'Multiple Root Graph'!

!Mrg methodsFor: 'collecting'!
  addToUgenSet: c
    lhs addToUgenSet: c.
    rhs addToUgenSet: c
  !
!

!Mrg methodsFor: 'accessing'!
  lhs ^lhs!
  lhs: x lhs := x!
  rhs ^rhs!
  rhs: x rhs := x!
!

!Mrg class methodsFor: 'instance creation'!
  lhs: x rhs: y
    "Multiple root graph.  The left hand input is the signal to be heard, the right hand side is the signal to be stored"
    | u |
    u := self new.
    u lhs: x.
    u rhs: y.
    ^u
  !
  primaryFactoryMethod ^#lhs:rhs:!
!
