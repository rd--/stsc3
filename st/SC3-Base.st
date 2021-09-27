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
    | n |
    n := self size max: other size.
    ^(1 to: n) collect: [ :index | aBlock value: (self atWrap: index) value: (other atWrap: index) ]
  !
  extendCyclically: n
    "Myself extended to n places by cycling"
    ^(1 to: n) collect: [ :index | self atWrap: index ]
  !
  extendToBeOfEqualSize
    "All of my collections extended to to be of the same size as my largest collection"
    | n |
    n := (self collect: [ :e | e size ]) max.
    ^self collect: [ :e | e extendCyclically: n ]
  !
!

!SequenceableCollection methodsFor: 'enumerating'!
  withIndexCollectZeroIndexed: f
    "sum of arrayFillZeroIndexed"
    ^self withIndexCollect: [ :e :i | f value: e value: i - 1 ]
  !
!

!Array class methodsFor: 'enumerating'!
  series: size value: start value: step
    "SC idiom to make an arithmetic series (ramp)"
    ^(1 to: size) collect: [ :i | (step * (i - 1)) + start ]
  !
  geom: size value: start value: grow
    "SC idiom to make a geometric series."
    | accum |
    accum := start.
    ^(1 to: size) collect: [ :i | | entry | entry := accum. accum := grow * accum. entry ]
  !
  fill: size value: aBlock
    "SC idiom to fill an array using a block"
    ^(1 to: size) collect: [ :i | aBlock value: i ]
  !
!

!Integer methodsFor: 'enumerating'!
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
  dup
    ^self dup: 2
  !
!

!Collection methodsFor: 'maths'!
  mean
    "Sum of myself divided by my size"
    ^self sum / self size
  !
  product
    "Fold of multiply"
    ^self inject: 1 into: [ :i :j | i * j ]
  !
!

!String methodsFor: 'file i/o'!
  evalFile
    "evalString of readTextFile."
    ^self readTextFile evalString
  !
!
