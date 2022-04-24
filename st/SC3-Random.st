!Integer methodsFor: 'randomness'!
rand
    "Generate a random integer between 1 and myself (inclusive).  Alias for atRandom."
    ^self atRandom
!
rand: upperBound
    "Generate a random integer between myself and upperBound (inclusive)."
    ^(upperBound - self + 1) atRandom + self - 1
!
rand2
    "Generate a random integer between myself negated and myself (inclusive)."
    ^(self negated) rand: self
!
exprand: upperBound
    "Generate a random float with exponential distribution between myself and upperBound (inclusive)"
    ^1.0 rand linExpFrom: self asFloat to: upperBound asFloat.
!
!

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
    "With probability of mself, return true. With probability of 1 - myself, return false."
    ^1.0 rand < self
!
!

!Array methodsFor: 'randomness'!
choose
    "Choose a random element of myself.  Alias for atRandom."
    ^self atRandom
!
!

!Interval methodsFor: 'randomness'!
  choose
    "Choose a random element of myself.  Alias for atRandom."
    ^self atRandom
  !
!
