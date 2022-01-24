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
  ++ otherCollection
    ^self , otherCollection
  !
  clump: groupSize
    "Array of myself partitioned into adjacent sequences of groupSize elements.  It is an error if I cannot be equally partitioned."
    | answerSize answer start |
    answerSize := (self size / groupSize) ceiling.
    answer := Array new: answerSize.
    start := 1.
    (1 to: answerSize) do: [ :i |
      | end |
      end := start + groupSize - 1.
      answer at: i put: (self copyFrom: start to: end).
      start := end + 1
    ].
    ^answer
  !
!

!SequenceableCollection methodsFor: 'indexing'!
  nth: index
      "One indexing name. For .stc, since at: in Sc is zero-indexed."
      ^self at: index
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
    ^(1 to: size) collect: [ :i |
      | entry |
      entry := accum.
      accum := grow * accum.
      entry ]
  !
  fill: size value: aBlock
    "SC idiom to fill an array using a zero argument block.
     The argument as index form is not implemented since St and Sc disagree about indexing."
    ^(1 to: size) collect: [ :i | aBlock value: i ]
  !
!

!Integer methodsFor: 'enumerating'!
  arrayFill: aBlock
    "c.f. Array class>>fill"
    ^(1 to: self) collect: [ :i | aBlock value ]
  !
  replicate: item
    "Array of myself number of places, each being item"
    ^self arrayFill: [ item ]
  !
  mixFill: f
    ^(self arrayFill: f) sum
  !
!

!Interval methodsFor: 'enumerating'!
  mixFill: aBlock
    ^(self asArray collect: aBlock) sum
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
    "An array of aNumber places, each filled by sending value to myself.
     Sc allows an index argument, however Sc and St disagree about indexing.

    [ 10 rand ] dup: 10
    "
    ^aNumber arrayFill: self
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

!Dictionary class methodsFor: 'instance creation'!
  newFromAssociations: assocArray
    "Dictionary from Array of associations"
    | aDict |
    aDict := Dictionary new.
    assocArray do: [ :anAssoc | aDict add: anAssoc ].
    ^aDict
  !
!

!Boolean methodsFor: '.stc'!
  if: trueBlock value: falseBlock
    ^self ifTrue: trueBlock ifFalse: falseBlock
  !
!
