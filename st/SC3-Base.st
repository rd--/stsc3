!Array methodsFor: 'rearranging'!
  transpose
    "Transpose rows and columns of two-dimensional array"
    ^(1 to: self first size) collect: [:c | self collect: [:r | r at: c]]
  !
!

!Integer methodsFor: 'enumerating'!
  arraySeries: start step: step
    "Fill with ramp of values"
    ^(1 to: self) collect: [:i| (step * (i - 1)) + start]
  !
  arrayGeom: start grow: grow
    "Fill with geometric series.  What is the Smalltalk method for this pattern."
    |accum|
    accum := start.
    ^(1 to: self) collect: [:i| |entry| entry := accum. accum := grow * accum. entry]
  !
  arrayFill: f
    "Array of (1 to: self) collected with f"
    ^(1 to: self) collect: [:i | f value: i]
  !
  arrayFillZeroIndexed: f
    "Array of (0 to: self - 1) collected with f"
    ^(1 to: self) collect: [:i | f value: (i - 1)]
  !
  replicate: x
    "Array of self places each being x"
    ^self arrayFill: [:i | x]
  !
  mixFill: f
    "sum of arrayFill"
    ^(self arrayFill: f) sum
  !
  mixFillZeroIndexed: f
    "sum of arrayFillZeroIndexed"
    ^(self arrayFillZeroIndexed: f) sum
  !
!
