AbstractUgen subclass: #Rate
  instanceVariableNames: 'source rate'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
!

Rate comment: 'Ugen Processing Rate'!

!Rate methodsFor: 'collecting'!
addToUgenSet: c
    source addToUgenSet: c
!
!

!Rate methodsFor: 'accessing'!
rate
    ^rate
!
rate: x
    rate := x
!
source
    ^source
!
source: x
    source := x
!
!

!Rate class methodsFor: 'instance creation'!
source: p1 rate: p2
    | r |
    r := self new.
    r source: p1.
    r rate: p2.
    ^r
!
!
