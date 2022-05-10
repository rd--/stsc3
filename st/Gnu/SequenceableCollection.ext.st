!SequenceableCollection methodsFor: 'stsc3'!
append: operand
    ^self , operand!
atWrap: index
    ^self at: index - 1 \\ self size + 1!
concatenation
    | result index |
    result := Array new: (self inject: 0 into: [:sum :each | sum + each size]).
    index := 0.
    self do: [:each | each do: [:item | result at: (index := index+1) put: item]].
    ^result!
shuffle
    ^self fisherYatesShuffle!
shuffled
    ^self copy shuffle!
withIndexDo: elementAndIndexBlock
    "Just like with:do: except that the iteration index supplies the second argument to the block."
    1 to: self size do: [ :index | elementAndIndexBlock value: (self at: index) value: index ]!
withIndexCollect: elementAndIndexBlock
    "Just like with:collect: except that the iteration index supplies the second argument to the block."
    | result |
    result := self species new: self size.
    1 to: self size do: [ :index | result at: index put: (elementAndIndexBlock value: (self at: index) value: index)].
    ^result!
!
