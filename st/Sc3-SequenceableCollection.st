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
!!

!SequenceableCollection methodsFor: 'indexing'!
nth: index
    "Alias for at:. For .stc, since at: in Sc is zero-indexed."
    ^self at: index
!!

!SequenceableCollection methodsFor: 'arithmetic'!
+~ other
    "Like + but cycles shorter collection"
    ^self withExtending: other collect: [:i :j| i + j]
!!

!SequenceableCollection methodsFor: 'multiple root graphs'!
mrg: rhs
    ^Mrg lhs: self rhs: rhs
!!
