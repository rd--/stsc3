!String methodsFor: 'file i/o'!
 readTextFile ^ self asFileReference readStream upToEnd !
 writeTextFile: blk self asFileReference delete . self asFileReference writeStreamDo: blk !
 dirAllSmalltalkFiles ^ (self asFileReference allChildrenMatching: '*.st') collect: [:x | x fullName] !
 evalString ^ OpalCompiler evaluate: self !
 evalFile ^ self readTextFile evalString !
!
