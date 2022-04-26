!Interval methodsFor: 'enumerating'!
mixFill: aBlock
    "Apply aBlock to my elements and sum the results.
    Since Sc and St disagree about indexing, mixFill: with index input to block is implemented at Interval."
    ^(self asArray collect: aBlock) sum
!
!

!Interval methodsFor: 'randomness'!
choose
    "Choose a random element of myself.  Alias for atRandom."
    ^self atRandom
!
!

!Interval methodsFor: 'collecting'!
addToUgenSet: c
    self asArray addToUgenSet: c
!
!

!Interval methodsFor: 'multi-channel expansion'!
asUgen
    ^self asArray asUgen
!
!

!Interval methodsFor: 'enumerating'!
  mceFill: aBlock
    ^(self asArray collect: aBlock) asUgen
  !
!
