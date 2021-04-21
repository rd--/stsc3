!String methodsFor: 'file i/o'!
 readTextFile
  ^self asFileReference readStream upToEnd
 !
 writeTextFile: blk
  |r|
  r := self asFileReference.
  r exists ifTrue: [r delete].
  r writeStreamDo: blk
 !
 dirAllFilesWithExt: ext
  ^(self asFileReference allChildrenMatching: ('*' , ext)) collect: [:x | x fullName]
 !
 evalString
  ^OpalCompiler evaluate: self
 !
 evalFile
  ^self readTextFile evalString
 !
 systemCommand
  ^LibC runCommand: self
 !
 fileListOn
  ^FileList openOn: self
 !
!
