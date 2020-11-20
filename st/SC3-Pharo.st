!String methodsFor: 'file i/o'!
 readTextFile ^ self asFileReference readStream upToEnd !
 writeTextFile: blk |r| r := self asFileReference . r exists ifTrue: [r delete] . r writeStreamDo: blk !
 dirAllSmalltalkFiles ^ (self asFileReference allChildrenMatching: '*.st') collect: [:x | x fullName] !
 evalString ^ OpalCompiler evaluate: self !
 evalFile ^ self readTextFile evalString !
!
