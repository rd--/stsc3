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
  evalFile
    "evalString of readTextFile."
    ^self readTextFile evalString
  !
  systemCommand
    "Run system command."
    ^Smalltalk system: self
  !
  putOn: fd
  "Run nextPut: for each character in self."
    ^1 to: self size do: [:i | fd nextPut: (self at: i)]
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
!

!Integer methodsFor: 'randomness'!
  atRandom
    "Random number between 1 and self."
    ^Random between: 1 and: self
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
!

!SequenceableCollection methodsFor: 'joining'!
  concatenation
    "Flattens the collection by one level into an Array."
    | result index |
    result := Array new: (self inject: 0 into: [:sum :each | sum + each size]).
    index := 0.
    self do: [:each | each do: [:item | result at: (index := index+1) put: item]].
    ^result !
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
!
