!String methodsFor: 'file i/o'!
 readTextFile ^ FileStream oldFileNamed: self do:  [:in | in contentsOfEntireFile ] !
 writeTextFile: blk |fd| fd := FileStream forceNewFileNamed: self . blk value: fd . fd close !
 dirAllSmalltalkFiles ^ ((FileDirectory on: self) fullNamesOfAllFilesInSubtree) select: [:x | x endsWith: '.st'] !
 evalString ^ Compiler evaluate: self !
 evalFile ^ self readTextFile evalString !
!
