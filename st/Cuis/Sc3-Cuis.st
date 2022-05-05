!String methodsFor: 'file i/o'!
  readTextFile
    ^(self asFileEntry) textContents
  !
  writeTextFile: blk
    ^(self asFileEntry) forceWriteStreamDo: blk
  !
  dirAllFilesWithExt: ext
    |c|
    c := OrderedCollection new.
    (self asDirectoryEntry) allFilesDo: [ :x | c add: x name ] matches: [ :x | x name endsWith: ext ].
    ^c asArray
  !
  evalString
    ^Compiler evaluate: self
  !
  systemCommand
    ^OSProcess command: self
  !
  putOn: fd
    ^1 to: self size do: [ :i | fd nextPut: (self at: i) ]
  !
  fileListOn
    ^FileListWindow openFileList "does not open on self..."
  !
  workspaceOn
    (Workspace new contents: self readTextFile) openLabel: ('Workspace on ' , self)
  !
!

!Character methodsFor: 'file i/o'!
  putOn: fd ^ fd nextPut: self !
!

!Collection methodsFor: 'arithmetic'!
  isZero
    "Answer whether the receiver is zero.
     Required to write 1 / #(1 2 3).
     Could also be self anySatisfy: [:x| x isZero]"
    ^false
  !
!

!Number methodsFor: 'mathematical functions'!
** aNumber
    "Special case of aNumber ** aUgen"
    self asUgen raisedTo: aNumber
!
!