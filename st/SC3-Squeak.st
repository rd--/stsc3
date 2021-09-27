SystemOrganization addCategory: #'Sound-SC3'!

!String methodsFor: 'file i/o'!
  readTextFile
    ^FileStream oldFileNamed: self do:  [:in | in contentsOfEntireFile ]
  !
  writeTextFile: blk
    |fd|
    fd := FileStream forceNewFileNamed: self.
    blk value: fd.
    fd close
  !
  dirAllFilesWithExt: ext
    ^((FileDirectory on: self) fullNamesOfAllFilesInSubtree) select: [:x | x endsWith: ext]
  !
  evalString
    ^Compiler evaluate: self
  !
  systemCommand
    "Run command synchronously.  Note, waitForCommand: hangs Squeak?"
    ^OSProcess command: self
  !
  fileListOn
    ^FileList openOn: (FileDirectory on: self)
  !
  workspaceOn
    (Workspace new contents: self readTextFile) openLabel: ('Workspace on ' , self)
  !
!
