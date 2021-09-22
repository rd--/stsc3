!SequenceableCollection methodsFor: 'rearranging'!
  transpose
    "Transpose rows and columns of two-dimensional array"
    ^(1 to: self first size) collect: [:c | self collect: [:r | r at: c]]
  !
  groupsOf: n
    "My elements taken n at a time. Ignore any leftovers at the end."
    ^(n to: self size by: n) collect: [:index | self copyFrom: index - n + 1 to: index]
  !
  withExtending: other collect: aBlock
    "Like with:collect: but cycle shorter list."
    |n|
    n := self size max: other size.
    ^(1 to: n) collect: [:index| aBlock value: (self atWrap: index) value: (other atWrap: index)]
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

!Number methodsFor: 'enumerating'!
  iota
    "Array of sequence from 1 to myself"
    ^(1 to: self) asArray
  !
  from: start by: step
    "Array of myself places from start by step"
    ^(start to: self - 1 * step + start by: step) asArray
  !
!

!BlockClosure methodsFor: 'enumerating'!
  dup: aNumber
    "An array of aNumber places,
     each filled by sending value to myself if I have no arguments,
     or value: counter if I have one argument.

    [ 10 rand ] dup: 10
    [ :x | x / 2 ] dup: 10
    "
    | fillFunc |
    fillFunc := (self numArgs = 1) ifTrue: [ self ] ifFalse: [ [ :x | self value ] ].
    ^aNumber arrayFill: fillFunc
  !
!

!Collection methodsFor: 'maths'!
  mean
    "Sum of myself divided by my size"
    ^self sum / self size
  !
!
