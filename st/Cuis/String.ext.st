!String methodsFor: 'stsc3'!
dirAllFilesWithExt: ext
    |c|
    c := OrderedCollection new.
    (self asDirectoryEntry) allFilesDo: [ :x | c add: x name ] matches: [ :x | x name endsWith: ext ].
    ^c asArray!
evalString
    ^Compiler evaluate: self!
fileListOn
    "does not open on self..."
    ^FileListWindow openFileList!
putOn: fd
    ^1 to: self size do: [ :i | fd nextPut: (self at: i) ]!
readTextFile
    ^(self asFileEntry) textContents!
systemCommand
    ^OSProcess command: self!
workspaceOn
    (Workspace new contents: self readTextFile) openLabel: ('Workspace on ' , self)!
writeTextFile: blk
    ^(self asFileEntry) forceWriteStreamDo: blk!
!
