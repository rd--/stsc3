!String methodsFor: 'file i/o'!
evalFile
    "Read the text file I name and evaluate it's contents."
    ^self readTextFile evalString!
!
