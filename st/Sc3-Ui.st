!String methodsFor: 'user interface'!
viewImageFile
    | i w |
    i := (Form fromFileNamed: self) asMorph.
    w := SystemWindow new.
    w extent: (i extent).
    w setLabel: self.
    w model: (Model new).
    w addMorph: i frame: (0@0 corner: 1@1).
    w openInWorldExtent: (i extent + (10@32))! !

!String methodsFor: 'user interface'!
viewTextFile
    | w t b m |
    w := SystemWindow new extent: 700@432.
    w setLabel: self.
    w model: (Model new).
    t := PluggableTextMorph on: (self readTextFile) text: #contents accept: #acceptContents: readSelection: nil menu: nil.
    w addMorph: t frame: (0@0 corner: 1@1).
    w openInWorldExtent: 700@432 "800@494 700@432 600@370 400@248 500@309"! !

SystemWindow subclass: #Sc3Editor
instanceVariableNames: 'fileName editor'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Sc3Editor comment: 'Editor for Ugen graph fragments'!

!Sc3Editor methodsFor: 'accessing'!
fileName ^fileName!
fileName: x fileName := x!
editor ^editor!
editor: x editor := x! !

!Sc3Editor class methodsFor: 'instance creation'!
openOn: x
    ^self new fileName: x; createWindow; openInWorld; yourself! !

!Sc3Editor methodsFor: 'operating'!
text
    ^fileName readTextFile
!
play
    | text |
    text := editor textMorph selection asString.
    (Sc3 evalText: text fromFile: fileName) play
!
draw
    | text |
    text := editor textMorph selection asString.
    (Sc3 evalText: text fromFile: fileName) draw
!
menu: m shifted: i
    m add: 'play (p)' target: self selector: #play.
    m add: 'stop (s)' target: Sc3 selector: #reset.
    m add: 'draw (d)' target: self selector: #draw.
    ^m! !

!Sc3Editor methodsFor: 'initialization'!
createWindow
    | t |
    t := PluggableTextMorph on: self text: #text accept: nil readSelection: nil menu: #menu:shifted:.
    t color: Color veryVeryLightGray.
    self editor: t.
    self addMorph: t frame: (0@0 corner: 1@1).
    self setLabel: 'Sc3 Editor on: ' , self fileName! !

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
index ^index! !

!Sc3Browser methodsFor: 'accessing'!
index: anInteger
  index := anInteger.
  self changed: #index
!
files
    | st stc |
    st := (directoryName dirAllFilesWithExt: '.st').
    stc := (directoryName dirAllFilesWithExt: '.stc').
    ^(st , stc) sort! !

!Sc3Browser class methodsFor: 'instance creation'!
openOn: x
    ^self new index: 0; directoryName: x; createWindow; openInWorld; yourself! !

!Sc3Browser methodsFor: 'operating'!
play
    Sc3 playFile: (self files at: index)
!
edit
    Sc3Editor openOn: (self files at: index)
!
draw
    Sc3 drawFile: (self files at: index)
!
menu: m
    m add: 'play (p)' target: self selector: #play.
    m add: 'stop (s)' target: Sc3 selector: #reset.
    m add: 'edit (e)' target: self selector: #edit.
    m add: 'draw (d)' target: self selector: #draw.
    ^m
!
menu: m shifted: i
    ^self menu: m
!
key: k from: i
    k == $p ifTrue: [self play].
    k == $s ifTrue: [Sc3 reset].
    k == $e ifTrue: [self edit].
    k == $d ifTrue: [self draw]! !

!Sc3Browser methodsFor: 'initialization'!
createWindow
    | l |
    l := PluggableListMorph new.
    l on: self list: #files selected: #index changeSelected: #index: menu: #menu:shifted: keystroke: #key:from:.
    l color: Color veryVeryLightGray.
    self addMorph: l frame: (0@0 corner: 1@1).
    self setLabel: 'Sc3 Browser on: ', self directoryName! !
