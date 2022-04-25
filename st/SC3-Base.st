!SequenceableCollection methodsFor: 'rearranging'!
transpose
    "Transpose my rows and columns (I am a two-dimensional array).
     My number of columns is inferred from the size of the my first element."
    ^(1 to: self first size) collect: [:c | self collect: [:r | r at: c]]
!
groupsOf: groupSize
    "My elements taken groupSize at a time. Ignore any leftovers at the end."
    ^(groupSize to: self size by: groupSize) collect: [:index | self copyFrom: index - groupSize + 1 to: index]
!
withExtending: other collect: aBlock
    "Like with:collect: but cycle shorter list."
    | maximumSize |
    maximumSize := self size max: other size.
    ^(1 to: maximumSize) collect: [ :index | aBlock value: (self atWrap: index) value: (other atWrap: index) ]
!
extendCyclically: answerSize
    "Myself extended to answerSize places by cycling"
    ^(1 to: answerSize) collect: [ :index | self atWrap: index ]
!
extendToBeOfEqualSize
    "All of my collections extended to to be of the same size as my largest collection"
    | maximumSize |
    maximumSize := (self collect: [ :e | e size ]) max.
    ^self collect: [ :e | e extendCyclically: maximumSize ]
!
++ otherCollection
    "Myself with otherCollection appended.  Alias for ,"
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
    "Alias for at:. For .stc, since at: in Sc is zero-indexed."
    ^self at: index
!
!

!Array class methodsFor: 'enumerating'!
series: size value: start value: step
    "Generate an arithmetic series (ramp) of size places from start by step.  From Sc."
    ^(1 to: size) collect: [ :item | (step * (item - 1)) + start ]
!
geom: size value: start value: grow
    "Generate a geometric series (curve) of size places from start by grow.  From Sc."
    | accum |
    accum := start.
    ^(1 to: size) collect: [ :unusedItem |
      | entry |
      entry := accum.
      accum := grow * accum.
      entry ]
!
fill: size value: aBlock
    "Generate an array using a zero argument block to fill each slot.  From Sc.
     The argument as index form is not implemented since St and Sc disagree about indexing."
    ^(1 to: size) collect: [ :unusedItem | aBlock value ]
!
!

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

!Interval methodsFor: 'enumerating'!
mixFill: aBlock
    "Apply aBlock to my elements and sum the results.
    Since Sc and St disagree about indexing, mixFill: with index input to block is implemented at Interval."
    ^(self asArray collect: aBlock) sum
!
!

!Number methodsFor: 'enumerating'!
iota
    "Array of numbers from 1 to myself."
    ^(1 to: self) asArray
!
from: start by: step
    "Array of myself places from start by step."
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
    "An array of myself evaluated twice."
    ^self dup: 2
!
!

!BlockClosure methodsFor: 'application'!
apply: anArray
    "Alias for valueWithArguments:.  For .stc notation."
    ^self valueWithArguments: anArray
!
!

!Collection methodsFor: 'maths'!
mean
    "Sum of myself divided by my size."
    ^self sum / self size
!
product
    "Product of my item."
    ^self inject: 1 into: [ :accumulator :item | accumulator * item ]
!
!

!String methodsFor: 'file i/o'!
evalFile
    "Read the text file I name and evaluate it's contents."
    ^self readTextFile evalString
!
!

!Dictionary class methodsFor: 'instance creation'!
newFromAssociations: assocArray
    "Generate a dictionary from an Array of associations."
    | aDict |
    aDict := Dictionary new.
    assocArray do: [ :anAssoc | aDict add: anAssoc ].
    ^aDict
!
!

!Boolean methodsFor: '.stc'!
if: trueBlock value: falseBlock
    "Alias for ifTrue:ifFalse:.  This is the SuperCollider if method, for .stc notation."
    ^self ifTrue: trueBlock ifFalse: falseBlock
!
!
