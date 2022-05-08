Object subclass: #Constant
  instanceVariableNames: 'value'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!
Constant comment: 'Constant numerical value. Required for Gnu coerce:'!
!Constant methodsFor: 'initialization'!
value: aNumber
    aNumber isNumber ifTrue: [ value := aNumber ] ifFalse: [self error: 'not a number?']!
!
!Constant methodsFor: 'testing'!
isZero
    ^value isZero!
isNumber
    ^true!
asFloat
    ^value asFloat!
asString
    ^value asString!
!
!Constant class methodsFor: 'instance creation'!
value: aNumber
    ^self new value: aNumber!
!
!Constant methodsFor: 'rate'!
rate
    ^Rate ir!
!
!Constant methodsFor: 'collecting'!
addToUgenSet: c protectedBy: w
    c add: value.
    ^self!
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
    "Gnu Smalltalk doesn't implement **. Special cases for Ugens and Collections."
    aNumber isZero ifTrue: [ ^1 ].
    aNumber isUgen ifTrue: [^(Constant value: self) raisedTo: aNumber].
    aNumber isCollection ifTrue: [^aNumber collect: [ :x | self ** x]].
    ^self raisedTo: aNumber
  !
!

!Number methodsFor: 'testing'!
isZero
    ^self = 0!
!

!Array methodsFor: 'coercing'!
  generality
    "Ugen is 500, Array is more general than Ugen?"
    ^600
  !
  coerce: aNumber
    "Coerce argument (which will be some form of Number) to a Ugen"
    ^aNumber dup: self size
  !
!

!Interval methodsFor: 'coercing'!
  generality
    "Array is 600, Interval is less general than Array?"
    ^590
  !
  coerce: aNumber
    "Coerce argument (which will be some form of Number) to a Ugen"
    ^aNumber dup: self size
  !
!

!Array methodsFor: 'converting'!
  adaptToCollection: rcvr andSend: selector
    "If I am involved in arithmetic with another Collection, return a Collection of
    the results of each element combined with the scalar in that expression."
    rcvr isSequenceable & self isSequenceable ifFalse:
      [self error: 'Only sequenceable collections may be combined arithmetically'].
    ^rcvr withExtending: self collect: [:rcvrElement :myElement | rcvrElement perform: selector with: myElement]
  !
!

!Interval methodsFor: 'converting'!
  adaptToCollection: rcvr andSend: selector
    ^self asArray adaptToCollection: rcvr andSend: selector
  !
!

!Number methodsFor: 'converting'!
  adaptToCollection: rcvr andSend: selector
    "If I am involved in arithmetic with a Collection, return a Collection of the results of each element combined with me in that expression."
    ^rcvr collect: [:element | element perform: selector with: self]
  !
!

!Collection methodsFor: 'arithmetic'!
  * arg
    ^arg adaptToCollection: self andSend: #*
  !
  + arg
    ^arg adaptToCollection: self andSend: #+
  !
  - arg
    ^arg adaptToCollection: self andSend: #-
  !
  / arg
    ^arg adaptToCollection: self andSend: #/
  !
  // arg
    ^arg adaptToCollection: self andSend: #//
  !
  raisedTo: arg
    ^arg adaptToCollection: self andSend: #raisedTo:
  !
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

!Ugen methodsFor: 'coercing'!
generality
    "Float is 400, Ugen is more general than Float."
    ^500!
coerce: aNumber
    "Coerce argument (which will be some form of Number) to a Ugen"
    ^Constant value: aNumber!
zero
    "Gnu has a curious coercion scheme.  It rewrites x < y as (x - y) < 0."
    ^Constant value: 0!
!


!Constant methodsFor: 'arithmetic'!
  + b ^ BinaryOpUgen specialIndex: 0 a: self b: b!
  - b ^ BinaryOpUgen specialIndex: 1 a: self b: b!
  * b ^ BinaryOpUgen specialIndex: 2 a: self b: b!
  / b ^ BinaryOpUgen specialIndex: 4 a: self b: b!
  % b ^ BinaryOpUgen specialIndex: 5 a: self b: b!
  /= b ^ BinaryOpUgen specialIndex: 7 a: self b: b!
  < b ^ BinaryOpUgen specialIndex: 8 a: self b: b!
  > b ^ BinaryOpUgen specialIndex: 9 a: self b: b!
  <= b ^ BinaryOpUgen specialIndex: 10 a: self b: b!
  >= b ^ BinaryOpUgen specialIndex: 11 a: self b: b!
  min: b ^ BinaryOpUgen specialIndex: 12 a: self b: b!
  max: b ^ BinaryOpUgen specialIndex: 13 a: self b: b!
  bitAnd: b ^ BinaryOpUgen specialIndex: 14 a: self b: b!
  bitOr: b ^ BinaryOpUgen specialIndex: 15 a: self b: b!
  lcm: b ^ BinaryOpUgen specialIndex: 17 a: self b: b!
  gcd: b ^ BinaryOpUgen specialIndex: 18 a: self b: b!
  roundTo: b ^ BinaryOpUgen specialIndex: 19 a: self b: b!
  truncateTo: b ^ BinaryOpUgen specialIndex: 21 a: self b: b!
  raisedTo: b ^ BinaryOpUgen specialIndex: 25 a: self b: b!
  bitShiftLeft: b ^ BinaryOpUgen specialIndex: 26 a: self b: b!
  bitShiftRight: b ^ BinaryOpUgen specialIndex: 27 a: self b: b!
  amClip: b ^ BinaryOpUgen specialIndex: 40 a: self b: b!
  clip2: b ^ BinaryOpUgen specialIndex: 42 a: self b: b!
  fold2: b ^ BinaryOpUgen specialIndex: 44 a: self b: b!
!

!Constant methodsFor: 'arithmetic'!
  negated ^ UnaryOpUgen specialIndex: 0 a: self!
  abs ^ UnaryOpUgen specialIndex: 5 a: self!
  ceil ^ UnaryOpUgen specialIndex: 8 a: self!
  floor ^ UnaryOpUgen specialIndex: 9 a: self!
  frac ^ UnaryOpUgen specialIndex: 10 a: self!
  sign ^ UnaryOpUgen specialIndex: 11 a: self!
  squared ^ UnaryOpUgen specialIndex: 12 a: self!
  cubed ^ UnaryOpUgen specialIndex: 13 a: self!
  sqrt ^ UnaryOpUgen specialIndex: 14 a: self!
  exp ^ UnaryOpUgen specialIndex: 15 a: self!
  reciprocal ^ UnaryOpUgen specialIndex: 16 a: self!
  midiCps ^ UnaryOpUgen specialIndex: 17 a: self!
  cpsMidi ^ UnaryOpUgen specialIndex: 18 a: self!
  midiRatio ^ UnaryOpUgen specialIndex: 19 a: self!
  ratioMidi ^ UnaryOpUgen specialIndex: 20 a: self!
  dbAmp ^ UnaryOpUgen specialIndex: 21 a: self!
  ampDb ^ UnaryOpUgen specialIndex: 22 a: self!
  ln ^ UnaryOpUgen specialIndex: 25 a: self!
  log2 ^ UnaryOpUgen specialIndex: 26 a: self!
  log ^ UnaryOpUgen specialIndex: 27 a: self!
  sin ^ UnaryOpUgen specialIndex: 28 a: self!
  cos ^ UnaryOpUgen specialIndex: 29 a: self!
  tanh ^ UnaryOpUgen specialIndex: 36 a: self!
  distort ^ UnaryOpUgen specialIndex: 42 a: self!
  softClip ^ UnaryOpUgen specialIndex: 43 a: self!
!
