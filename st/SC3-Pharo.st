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
  systemCommand
    ^LibC runCommand: self
  !
  fileListOn
    ^FileList openOn: self
  !
!

!Collection methodsFor: 'arithmetic'!
  isZero
    ^false
  !
!

!Float methodsFor: 'arithmetic'!
  tanh
    "Answer hyperbolic tangent of receiver.
    Trivial implementation is:
        ^self sinh/self cosh
    This implementation takes care not to overflow."
    | ex emx |
    self = 0.0 ifTrue: [^self].    "Handle negativeZero"
    self > 20.0 ifTrue: [^1.0].
    self < -20.0 ifTrue: [^-1.0].
    ex := self exp.
    emx := ex reciprocal.
    ^(ex - emx) / (ex + emx)
  !
!
