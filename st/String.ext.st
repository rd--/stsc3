!String methodsFor: 'file i/o'!
evalFile
    "Read the text file I name and evaluate it's contents."
    ^self readTextFile evalString!
!

!String methodsFor: 'printing'!
asHaskell
    ^'"' , self , '"'
  !
printHaskellOn: fd
    "Print my hsc3 representation."
    fd
      nextPut: $";
      nextPutAll: self;
      nextPut: $"
  !
!

