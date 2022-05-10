!Object methodsFor: 'maths'!
adaptToFloat: rcvr andSend: selector
    ^ self adaptToNumber: rcvr andSend: selector!
adaptToFloat: rcvr andCompare: selector
    ^ self adaptToFloat: rcvr andSend: selector!
adaptToInteger: rcvr andSend: selector
    ^ self adaptToNumber: rcvr andSend: selector!
!

!Collection methodsFor: 'maths'!
adaptToCollection: rcvr andSend: selector
    rcvr isSequenceable & self isSequenceable ifFalse: [self error: 'Only sequenceable collections may be combined arithmetically'].
    ^rcvr withExtending: self collect: [:rcvrElement :myElement | rcvrElement perform: selector with: myElement]!
adaptToNumber: rcvr andSend: selector
    ^ self collect: [:element | rcvr perform: selector with: element]!
!

!Collection methodsFor: 'arithmetic'!
* arg
    ^arg adaptToCollection: self andSend: #*!
+ arg
    ^arg adaptToCollection: self andSend: #+!
- arg
    ^arg adaptToCollection: self andSend: #-!
/ arg
    ^arg adaptToCollection: self andSend: #/!
// arg
    ^arg adaptToCollection: self andSend: #//!
raisedTo: arg
    ^arg adaptToCollection: self andSend: #raisedTo:!
!

!Number methodsFor: 'converting'!
adaptToFloat: rcvr andSend: selector
    ^ rcvr perform: selector with: self asFloat!
adaptToInteger: rcvr andSend: selector
    ^ self subclassResponsibility!
adaptToCollection: rcvr andSend: selector
    ^rcvr collect: [:element | element perform: selector with: self]!
!

!SmallInteger methodsFor: 'maths'!
+ aNumber
    <primitive: VMpr_SmallInteger_plus>
     aNumber isSmallInteger ifTrue: [ ^(LargeInteger fromInteger: self) + (LargeInteger fromInteger: aNumber) ].
    ^ aNumber adaptToInteger: self andSend: #+!
* aNumber
    <primitive: VMpr_SmallInteger_times>
     aNumber isSmallInteger ifTrue: [ ^(LargeInteger fromInteger: self) * (LargeInteger fromInteger: aNumber) ].
   ^ aNumber adaptToInteger: self andSend: #*!
/ aNumber
    ^ self asFloat / aNumber!
!

!Float methodsFor: 'maths'!
adaptToInteger: rcvr andSend: selector
    ^ rcvr asFloat perform: selector with: self!
+ aNumber
     <primitive: VMpr_FloatD_plus>
    ^ aNumber adaptToFloat: self andSend: #+!
- aNumber
     <primitive: VMpr_FloatD_minus>
    ^ aNumber adaptToFloat: self andSend: #-!
* aNumber
    <primitive: VMpr_FloatD_times>
    ^ aNumber adaptToFloat: self andSend: #*!
/ aNumber
    <primitive: VMpr_FloatD_divide>
    ^ aNumber adaptToFloat: self andSend: #/!
< aNumber
    <primitive: VMpr_FloatD_lt>
    ^ aNumber adaptToFloat: self andCompare: #<!
> aNumber
    <primitive: VMpr_FloatD_gt>
    ^ aNumber adaptToFloat: self andCompare: #>!
<= aNumber
    <primitive: VMpr_FloatD_le>
    ^ aNumber adaptToFloat: self andCompare: #<=!
>= aNumber
    <primitive: VMpr_FloatD_ge>
    ^ aNumber adaptToFloat: self andCompare: #>=!
!

!String methodsFor: 'file i/o'!
readTextFile
    "Read entire contents from file as String."
    | stream text |
    stream := FileStream open: self mode: FileStream read.
    text := stream contents.
    stream close.
    ^text!
writeTextFile: aBlock
    "Write text file passing aBlock a stream."
    | stream |
    stream := FileStream open: self mode: FileStream write.
    aBlock value: stream.
    stream close!
putOn: aStream
  "Run nextPut: for each character in self."
    ^1 to: self size do: [ :item | aStream nextPut: (self at: item) ]!
!

!Stream methodsFor: 'readability'!
<< items
 	items putOn: self.
	^self!
!

!Magnitude methodsFor: 'streaming'!
putOn: aStream
	(aStream isBinary ifTrue: [ self asByteArray ] ifFalse: [ self asString]) putOn: aStream!
!

!String methodsFor: 'evaluating'!
evalString
    "Evaluate myself (I am a Smalltalk expression)"
    ^Behavior evaluate: self!
!

!String methodsFor: 'o/s'!
systemCommand
    "Run system command."
    ^Smalltalk system: self!

!Character methodsFor: 'file i/o'!
putOn: aStream
    ^aStream nextPut: self!
!

!Dictionary class methodsFor: 'instance creation'!
newFrom: aCollection
    "Generate a dictionary from a collection of associations."
    | answer |
    answer := Dictionary new.
    aCollection do: [ :anAssociation | answer add: anAssociation ].
    ^answer!
newFromPairs: anArray
    "Answer an instance of me associating (anArray at:i) to (anArray at: i+1) for each odd i.  anArray must have an even number of entries."
    | newDictionary |
    newDictionary := self new: (anArray size / 2).
    1 to: (anArray size - 1) by: 2 do: [ :item |
        newDictionary at: (anArray at: item) put: (anArray at: item + 1)
    ].
    ^newDictionary!
!

!Integer methodsFor: 'randomness'!
atRandom
    ^Random between: 1 and: self!
!

!Integer methodsFor: 'bit manipulation'!
<< shiftAmount
    shiftAmount < 0 ifTrue: [ self error: 'negative arg' ].
    ^self bitShift: shiftAmount!
>> shiftAmount
    shiftAmount < 0 ifTrue: [ self error: 'negative arg' ].
    ^ self bitShift: 0 - shiftAmount!
!

!SequenceableCollection methodsFor: 'randomness'!
shuffle
    ^self fisherYatesShuffle!
shuffled
    ^self copy shuffle!
!

!Collection methodsFor: 'math functions'!
sum
    "Sum elements of collection."
    ^self inject: 0 into: [ :total :item | total + item ]!
max
    "Maximum element of collection."
    ^self inject: (self atRandom) into: [ :maximum :item | (maximum > item) ifTrue: [ maximum ] ifFalse: [ item ]]!
sqrt
    ^self collect: [ :item | item sqrt ]!
reciprocal
    "Return the reciever full of reciprocated elements"
    ^self collect: [ :item | item reciprocal ]!
!

!Collection methodsFor: 'testing'!
isCollection
    ^true!
!

!SequenceableCollection methodsFor: 'joining'!
concatenation
    "Flattens the collection by one level into an Array."
    | result index |
    result := Array new: (self inject: 0 into: [:sum :each | sum + each size]).
    index := 0.
    self do: [:each | each do: [:item | result at: (index := index+1) put: item]].
    ^result!
append: operand
    ^self , operand!
atWrap: index
    "Answer the index'th element of the receiver.  If index is out of bounds,
     let it wrap around from the end to the beginning until it is in bounds."
    ^self at: index - 1 \\ self size + 1!
!

!Number methodsFor: 'mathematical functions'!
** aNumber
    ^BinaryOpUgen selector: #raisedTo: a: self b: aNumber!
!

!Number methodsFor: 'testing'!
isZero
    ^self = 0!
!

!SequenceableCollection methodsFor: 'enumerating'!
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

!SequenceableCollection methodsFor: 'accessing'!
atWrap: index
    "Answer the index'th element of the receiver.  If index is out of bounds,
     let it wrap around from the end to the beginning until it is in bounds."
    ^self at: index - 1 \\ self size + 1!
!

!Object methodsFor: 'evaluating'!
value
    ^self!
!

!Object methodsFor: 'testing'!
isCollection
    ^false!
!

!Magnitude methodsFor: 'testing'!
min: aMin max: aMax
    ^(self min: aMin) max: aMax!
!

!Float class methodsFor: 'constants'!
infinity
    ^FloatD infinity!
!

!FileDescriptor methodsFor: 'binary i/o'!
nextInt32Put: anInt
    ^self nextPutLong: anInt!
nextFloat32Put: aFloat
    ^self nextPutFloat: aFloat!
!

!BlockClosure methodsFor: 'evaluating'!
value: p1 value: p2 value: p3 value: p4
    ^self valueWithArguments: {p1. p2. p3. p4}!
value: p1 value: p2 value: p3 value: p4 value: p5
    ^self valueWithArguments: {p1. p2. p3. p4. p5}!
!
