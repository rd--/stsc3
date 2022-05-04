!Integer methodsFor: 'enumerating'!
arrayFill: aBlock
    "Array of myself number of places, each being a result of evaluating aBlock.  C.f. Array class>>fill."
    ^(1 to: self) collect: [ :unusedItem | aBlock value ]
!
mixFill: aBlock
    "Send arrayFill: to myself, and then sum result.  From Sc."
    ^(self arrayFill: aBlock) sum
!
replicate: item
    "Array of myself number of places, each being item."
    ^self arrayFill: [ item ]
!
!

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

