!Ugen methodsFor: 'coercing'!
  generality
    "Float is 400, Ugen is more general than Float."
    ^500
  !
  coerce: aNumber
    "Coerce argument (which will be some form of Number) to a Ugen"
    ^aNumber asUgen
  !
  zero
    "Gnu has a curious coercion scheme.  It rewrites x < y as (x - y) < 0."
    0 asUgen
  !
!

!String methodsFor: 'file i/o'!
  readTextFile
    "Read entire contents from file as String."
    | fd txt |
    fd := FileStream open: self mode: FileStream read.
    txt := fd contents.
    fd close.
    ^txt
  !
  writeTextFile: blk
    "Write text file passing blk a file descriptor."
    | fd |
    fd := FileStream open: self mode: FileStream write.
    blk value: fd.
    fd close
  !
  evalString
    "Evaluate string."
    ^Behavior evaluate: self
  !
  systemCommand
    "Run system command."
    ^Smalltalk system: self
  !
  putOn: fd
  "Run nextPut: for each character in self."
    ^1 to: self size do: [ :i | fd nextPut: (self at: i) ]
  !
!

!Character methodsFor: 'file i/o'!
  putOn: fd
    "Run nextPut:."
    ^fd nextPut: self
  !
!

!Dictionary class methodsFor: 'instance creation'!
  newFrom: x
    "Alias for from:."
    ^self from: x
  !
  newFromPairs: anArray
	"Answer an instance of me associating (anArray at:i) to (anArray at: i+1) for each odd i.  anArray must have an even number of entries."
	| newDictionary |
	newDictionary := self new: (anArray size / 2).
	1 to: (anArray size - 1) by: 2 do: [ :i |
		newDictionary at: (anArray at: i) put: (anArray at: i + 1).
	].
	^ newDictionary
  !
!

!Integer methodsFor: 'randomness'!
  atRandom
    "Random number between 1 and self."
    ^Random between: 1 and: self
  !
!

!Integer methodsFor: 'bit manipulation'!
  << shiftAmount
    "left shift"
    shiftAmount < 0 ifTrue: [self error: 'negative arg'].
    ^ self bitShift: shiftAmount
  !
  >> shiftAmount
    "right shift"
    shiftAmount < 0 ifTrue: [self error: 'negative arg'].
    ^ self bitShift: 0 - shiftAmount
  !
!

!SequenceableCollection methodsFor: 'randomness'!
  shuffle
    "Durstenfeld's version of the Fisher-Yates shuffle"
    self size to: 2 by: -1 do: [ :i |
      self swap: i with: (i atRandom) ]
  !
  shuffled
    ^self copy shuffle
  !
!

!Collection methodsFor: 'math functions'!
  sum
    "Sum elements of collection."
    ^self inject: 0 into: [:a :c | c + a]
  !
  max
    "Maximum element of collection."
    ^self inject: 0 into: [:a :c | (a > c) ifTrue: [a] ifFalse: [c]].
  !
  sqrt
    ^ self collect: [:each | each sqrt]
  !
  reciprocal
    "Return the reciever full of reciprocated elements"
    ^ self collect: [:a | a reciprocal]
  !
!

!Collection methodsFor: 'testing'!
  isCollection
    "Am I some kind of Collection?"
    ^true
  !
!

!SequenceableCollection methodsFor: 'joining'!
  concatenation
    "Flattens the collection by one level into an Array."
    | result index |
    result := Array new: (self inject: 0 into: [:sum :each | sum + each size]).
    index := 0.
    self do: [:each | each do: [:item | result at: (index := index+1) put: item]].
    ^result !
  append: operand
    ^self , operand
  !
  atWrap: index
    "Answer the index'th element of the receiver.  If index is out of bounds,
     let it wrap around from the end to the beginning until it is in bounds."
    ^ self at: index - 1 \\ self size + 1
  !
!

!Array methodsFor: 'coercing'!
  generality
    "Ugen is 500, Array is more general than Ugen?"
    ^600
  !
  coerce: aNumber
    "Coerce argument (which will be some form of Number) to a Ugen"
    ^(self size) replicate: aNumber
  !
!

!Interval methodsFor: 'coercing'!
  generality
    "Array is 600, Interval is less general than Array?"
    ^590
  !
  coerce: aNumber
    "Coerce argument (which will be some form of Number) to a Ugen"
    ^(self size) replicate: aNumber
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

!Number methodsFor: 'mathematical functions'!
  ** aNumber
    "Gnu Smalltalk doesn't implement **.
     Special cases for Ugens and Collections."
    (aNumber = 0) ifTrue: [^1].
    aNumber isUgen ifTrue: [^self asUgen raisedTo: aNumber].
    aNumber isCollection ifTrue: [^aNumber collect: [ :x | self ** x]].
    ^self raisedTo: aNumber
  !
!

!Number methodsFor: 'testing'!
  isZero
    ^self = 0
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
    1 to: self size do:
      [:index |
        elementAndIndexBlock
          value: (self at: index)
          value: index]
  !
  withIndexCollect: elementAndIndexBlock
  "Just like with:collect: except that the iteration index supplies the second argument to the block."
  | result |
  result := self species new: self size.
  1 to: self size do:
    [:index | result at: index put:
    (elementAndIndexBlock
      value: (self at: index)
      value: index)].
  ^ result
  !
!

!SequenceableCollection methodsFor: 'accessing'!
  atWrap: index
    "Answer the index'th element of the receiver.  If index is out of bounds,
     let it wrap around from the end to the beginning until it is in bounds."
    ^ self at: index - 1 \\ self size + 1
  !
!

!Object methodsFor: 'evaluating'!
  value
    "Value protocol.  Value at Object is identity."
    ^self
  !
!

!Object methodsFor: 'testing'!
  isCollection
    "Am I some kind of Collection?"
    ^false
  !
!

!Magnitude methodsFor: 'comparing'!
  min: aMin max: aMax
    ^ (self min: aMin) max: aMax
  !
!

!Float class methodsFor: 'constants'!
  infinity
    ^FloatD infinity
  !
!

!BlockClosure methodsFor: 'evaluating'!
  value: p1 value: p2 value: p3 value: p4
    ^self valueWithArguments: {p1. p2. p3. p4}
  !
  value: p1 value: p2 value: p3 value: p4 value: p5
    ^self valueWithArguments: {p1. p2. p3. p4. p5}
  !
!

!FileDescriptor methodsFor: 'binary i/o'!
  nextInt32Put: anInt
    ^self nextPutLong: anInt
  !
  nextFloat32Put: anInt
    ^self nextPutFloat: anInt
  !
!
