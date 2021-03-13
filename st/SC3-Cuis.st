!String methodsFor: 'file i/o'!
 readTextFile ^ (self asFileEntry) textContents !
 writeTextFile: blk ^ (self asFileEntry) forceWriteStreamDo: blk !
 dirAllFilesWithExt: ext |c| c := OrderedCollection new . (self asDirectoryEntry) allFilesDo: [:x | c add: (x name)] matches: [:x | (x name) endsWith: ext] . ^ c asArray !
 evalString ^ Compiler evaluate: self !
 evalFile ^ self readTextFile evalString !
 systemCommand ^ OSProcess command: self !
 putOn: fd ^ 1 to: self size do: [:i | fd nextPut: (self at: i)] !
!

!Character methodsFor: 'file i/o'!
 putOn: fd ^ fd nextPut: self !
!

"
PluggableListMorph#

l model: self  listGetter: #files  indexGetter: #index indexSetter: #index: mainView: nil menuGetter: #menu:shifted:  keystrokeAction: #key:from: .
"
