!Integer methodsFor: 'randomness'!
  rand
    "Random Integer between 1 and self (inclusive), alias for atRandom"
    ^self atRandom
  !
  rand: n
    "Random Integer between self and n (inclusive)"
    ^(n - self + 1) atRandom + self - 1
  !
  rand2
    "Random Integer between -self and self (inclusive)"
    ^(self negated) rand: self
  !
  exprand: n
    "Random Float with exponential distribution between self and n (inclusive)"
    ^1.0 rand linExpFrom: self asFloat to: n asFloat.
  !
!

!Float methodsFor: 'randomness'!
  rand
    "Random Float between 0 and self (inclusive)"
    ^(100000000 atRandom / 100000000.0) * self
  !
  rand: n
    "Random Float between self and n (inclusive)"
    ^(100000000 atRandom / 100000000.0) * (n - self) + self
  !
  rand2
    "Random Integer between -self and self (inclusive)"
    ^(self negated) rand: self
  !
  exprand: n
    "Random Float with exponential distribution between self and n (inclusive)"
    ^1.0 rand linExpFrom: self to: n.
  !
  coin
    "With probability self, return true. With probability 1 - self, return false."
    ^1.0 rand < self
  !
!

!Array methodsFor: 'randomness'!
  choose
    "Random element of Array, alias for atRandom"
    ^self atRandom
  !
!
