!Integer methodsFor: 'enumerating'!
iota
    ^(1 to: self)!
from: start by: step
    ^(start to: self - 1 * step + start by: step)!
to: aNumber collect: aBlock
    ^(self to: aNumber) collect: aBlock!
!

!Integer methodsFor: 'randomness'!
rand
    "Generate a random integer between 1 and myself (inclusive).  Alias for atRandom."
    ^self atRandom!
rand: upperBound
    "Generate a random integer between myself and upperBound (inclusive)."
    ^(upperBound - self + 1) atRandom + self - 1!
rand2
    "Generate a random integer between myself negated and myself (inclusive)."
    ^(self negated) rand: self!
exprand: upperBound
    "Generate a random float with exponential distribution between myself and upperBound (inclusive)"
    ^1.0 rand linExpFrom: self asFloat to: upperBound asFloat.!
!
