SystemOrganization addCategory: #'Sound-SC3'!

!String methodsFor: 'file i/o'!
  readTextFile
    "Read my contents (I am the name of a name of an existing text file)"
    ^FileStream oldFileNamed: self do: [ :in | in contentsOfEntireFile ]
  !
  writeTextFile: aBlock
    "Run aBlock on me as a FileStream (I am the name of a text file, possibly already existing)"
    | fd |
    fd := FileStream forceNewFileNamed: self.
    aBlock value: fd.
    fd flush ; close
  !
  dirAllFilesWithExt: ext
    "List of all files with a particular file extension residing in me (I am the name of a directory)"
    ^((FileDirectory on: self) fullNamesOfAllFilesInSubtree) select: [ :x | x endsWith: ext ]
  !
  evalString
    "Evaluate myself (I am a Smalltalk expression)"
    ^Compiler evaluate: self
  !
  systemCommand
    "Run myself as a synchronous OSProcess (I am an OS command)."
    ^OSProcess command: self "<- waitForCommand: hangs Squeak 5.3?"
  !
  fileListOn
    "Open a FileList on myself (I am the name of a directory)"
    ^FileList openOn: (FileDirectory on: self)
  !
  workspaceOn
    "Open a Workspace on myself (I am the name of a file)"
    (Workspace new contents: self readTextFile) openLabel: ('Workspace on ' , self)
  !
!
