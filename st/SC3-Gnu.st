!String methodsFor: 'file i/o'!
 readTextFile |fd txt| fd := FileStream open: self mode: FileStream read. txt := fd contents. fd close. ^txt !
 writeTextFile: blk |fd| fd := FileStream open: self mode: FileStream write. blk value: fd. fd close !
 evalString ^ Behavior evaluate: self !
 evalFile ^ self readTextFile evalString !
 systemCommand ^ Smalltalk system: self !
"
 dirAllFilesWithExt: ext ^ ((FileDirectory on: self) fullNamesOfAllFilesInSubtree) select: [:x | x endsWith: ext] !
 fileList ^ FileList openOn: (FileDirectory on: self) !
"
 putOn: fd ^ 1 to: self size do: [:i | fd nextPut: (self at: i)] !
!

!Character methodsFor: 'file i/o'!
 putOn: fd ^ fd nextPut: self !
!

!Dictionary class methodsFor: 'instance creation'!
 newFrom: x ^ self from: x !
!

!Integer methodsFor: 'randomness'!
 atRandom ^ Random between: 1 and: self !
!

!Array methodsFor: 'numerics'!
 sum ^ self inject: 0 into: [:a :c | c + a] !
!

!SequenceableCollection methodsFor: 'joining'!
 concatenation
  "Flattens the collection by one level into an Array."
  |result index|
  result := Array new: (self inject: 0 into: [:sum :each | sum + each size]).
  index := 0.
  self do: [:each | each do: [:item | result at: (index := index+1) put: item]].
  ^ result !
!
