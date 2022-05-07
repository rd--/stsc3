!SequenceableCollection methodsFor: 'rearranging'!
transpose
    "Transpose my rows and columns (I am a two-dimensional array).
     My number of columns is inferred from the size of the my first element."
    ^(1 to: self first size) collect: [:c | self collect: [:r | r at: c]]!
groupsOf: groupSize
    "My elements taken groupSize at a time. Ignore any leftovers at the end."
    ^(groupSize to: self size by: groupSize) collect: [:index | self copyFrom: index - groupSize + 1 to: index]!
withExtending: other collect: aBlock
    "Like with:collect: but cycle shorter list."
    | maximumSize |
    maximumSize := self size max: other size.
    ^(1 to: maximumSize) collect: [ :index | aBlock value: (self atWrap: index) value: (other atWrap: index) ]!
extendCyclically: answerSize
    "Myself extended to answerSize places by cycling"
    ^(1 to: answerSize) collect: [ :index | self atWrap: index ]!
extendToBeOfEqualSize
    "All of my elements extended to to be of the same size as my largest element.
     #(1 (2 3) (4 5 6)) extendToBeOfEqualSize
    "
    | maximumSize |
    maximumSize := (self collect: [ :item | item asArray size ]) max.
    ^self collect: [ :item | item asArray extendCyclically: maximumSize ]!
++ otherCollection
    "Myself with otherCollection appended.  Alias for ,"
    ^self , otherCollection!
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
    ^answer!
    rotateLeft: n
        ^(1 + n to: self size + n) collect: [ :index | self atWrap: index ]!
    rotateRight: n
        ^(1 - n to: self size - n) collect: [ :index | self atWrap: index ]!
    rotate: n
        ^self rotateRight: n!
!

!SequenceableCollection methodsFor: 'indexing'!
nth: index
    "Alias for at:. For .stc, since at: in Sc is zero-indexed."
    ^self at: index!
findIndex: aBlock
    1 to: self size do: [ :index | (aBlock value: (self at: index)) ifTrue: [ ^index ] ].
    ^nil!
!

!SequenceableCollection methodsFor: 'arithmetic'!
max: arg
    ^arg adaptToCollection: self andSend: #max:!
roundTo: arg
    ^arg adaptToCollection: self andSend: #roundTo:!
+~ other
    "Like + but cycles shorter collection"
    ^self withExtending: other collect: [ :lhs :rhs| lhs + rhs ]!
!

!Array methodsFor: 'randomness'!
choose
    ^self atRandom!
fisherYatesShuffle
    self size to: 2 by: -1 do: [ :item | self swap: item with: (item atRandom) ]!
!

!SequenceableCollection methodsFor: 'Ugen creation'!
asLocalBuf
    | lhs rhs |
    lhs := LocalBuf numChannels: 1 numFrames: self size.
    rhs := SetBuf buf: lhs offset: 0 length: self size array: self.
    ^lhs mrg: rhs!
!
