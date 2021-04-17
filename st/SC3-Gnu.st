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
