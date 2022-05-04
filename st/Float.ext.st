!Float methodsFor: 'randomness'!
rand
    "Generate a random float between 0 and myself (inclusive)"
    ^(100000000 atRandom / 100000000.0) * self
!
rand: upperBound
    "Generate a random float between myself and upperBound (inclusive)"
    ^(100000000 atRandom / 100000000.0) * (upperBound - self) + self
!
rand2
    "Generate a random float between myself negated and myself (inclusive)"
    ^(self negated) rand: self
!
exprand: upperBound
    "Generate a random float with exponential distribution between myself and upperBound (inclusive)"
    ^1.0 rand linExpFrom: self to: upperBound.
!
coin
    "With probability of myself, return true. With probability of 1 - myself, return false."
    ^1.0 rand < self
!
!

!Float methodsFor: 'printing'!
printHaskellOn: fd
    "Print my hsc3 representation.  Special cased for Infinity"
    | txt |
    txt := (self = Float infinity) ifTrue: [ 'dinf' ] ifFalse: [ self asString ].
    txt putOn: fd
  !
!

