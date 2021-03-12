!String methodsFor: 'file i/o'!
 readTextFile ^ (self asFileEntry) textContents !
 writeTextFile: blk ^ (self asFileEntry) forceWriteStreamDo: blk !
 dirAllFilesWithExt: ext ^ ('/tmp' asDirectoryEntry) fileNamesMatching: ('*',ext) !
 evalString ^ Compiler evaluate: self !
 evalFile ^ self readTextFile evalString !
 systemCommand ^ OSProcess command: self !
!

"
String#putOn:

PluggableListMorph#

l model: self  listGetter: #files  indexGetter: #index indexSetter: #index: mainView: nil menuGetter: #menu:shifted:  keystrokeAction: #key:from: .

('/tmp' asDirectoryEntry) fileNamesMatching: '*.svg'

"
