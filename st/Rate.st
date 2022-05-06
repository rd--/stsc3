Object subclass: #Rate
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Rate comment: 'Ugen processing rate'!

!Rate class methodsFor: 'constants'!
ir
    ^0!
kr
    ^1!
ar
    ^2!
dr
    ^3!
symbol: anInteger
    ^#(#ir #kr #ar #dr) at: (anInteger + 1)!
!
