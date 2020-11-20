!Integer methodsFor: 'randomness'!
 rand ^ self atRandom !
 rand: n ^ (n - self + 1) atRandom + self - 1 !
 rand2 ^ (self negated) rand: self !
!

!Float methodsFor: 'randomness'!
 rand ^ (100000000 atRandom / 100000000.0) * self !
 rand: n ^ (100000000 atRandom / 100000000.0) * (n - self) + self !
 rand2 ^ (self negated) rand: self !
!
