SystemWindow subclass: #Sc3Browser
  instanceVariableNames: 'directoryName index'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
!

Sc3Browser comment: 'List view on Ugen graph fragments'!

!Sc3Browser methodsFor: 'accessing'!
directoryName ^directoryName!
directoryName: x directoryName := x!
index ^index!
!

!Sc3Browser methodsFor: 'accessing'!
index: anInteger
  index := anInteger.
  self changed: #index!
files
    | st stc |
    st := (directoryName dirAllFilesWithExt: '.st').
    stc := (directoryName dirAllFilesWithExt: '.stc').
    ^(st , stc) sort!
!

!Sc3Browser class methodsFor: 'instance creation'!
openOn: x
    ^self new index: 0; directoryName: x; createWindow; openInWorld; yourself!
!

!Sc3Browser methodsFor: 'operating'!
play
    Sc3 playFile: (self files at: index)!
edit
    Sc3Editor openOn: (self files at: index)!
draw
    Sc3 drawFile: (self files at: index)!
menu: m
    m add: 'play (p)' target: self selector: #play.
    m add: 'stop (s)' target: Sc3 selector: #reset.
    m add: 'edit (e)' target: self selector: #edit.
    m add: 'draw (d)' target: self selector: #draw.
    ^m!
menu: m shifted: i
    ^self menu: m!
key: k from: i
    k == $p ifTrue: [self play].
    k == $s ifTrue: [Sc3 reset].
    k == $e ifTrue: [self edit].
    k == $d ifTrue: [self draw]!
!

!Sc3Browser methodsFor: 'initialization'!
createWindow
    | l |
    l := PluggableListMorph new.
    l on: self list: #files selected: #index changeSelected: #index: menu: #menu:shifted: keystroke: #key:from:.
    l color: Color veryVeryLightGray.
    self addMorph: l frame: (0@0 corner: 1@1).
    self setLabel: 'Sc3 Browser on: ', self directoryName!
!
