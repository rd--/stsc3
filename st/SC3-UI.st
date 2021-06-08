!String methodsFor: 'user interface'!
  viewImageFile
    | i w |
    i := (Form fromFileNamed: self) asMorph.
    w := SystemWindow new.
    w extent: (i extent).
    w setLabel: self.
    w model: (Model new).
    w addMorph: i frame: (0@0 corner: 1@1).
    w openInWorldExtent: (i extent + (10@32))
  !
!

!String methodsFor: 'user interface'!
  viewTextFile
    | w t b m |
    w := SystemWindow new extent: 700@432.
    w setLabel: self.
    w model: (Model new).
    t := PluggableTextMorph on: (self readTextFile) text: #contents accept: #acceptContents: readSelection: nil menu: nil.
    w addMorph: t frame: (0@0 corner: 1@1).
    w openInWorldExtent: 700@432 "800@494 700@432 600@370 400@248 500@309"
  !
!

SystemWindow subclass: #SC3Editor
  instanceVariableNames: 'fileName editor'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'SC3'
!

SC3Editor comment: 'Editor for UGen graph fragments'!

!SC3Editor methodsFor: 'accessing'!
  fileName ^fileName
  !
  fileName: x fileName := x
  !
  editor ^editor
  !
  editor: x editor := x
  !
!

!SC3Editor class methodsFor: 'instance creation'!
  openOn: x
    | v |
    v := self new.
    v fileName: x; createWindow; openInWorld.
    ^v
  !
!

!SC3Editor methodsFor: 'operating'!
  text
    ^self fileName readTextFile
  !
  play
    editor textMorph selection asString evalString play
  !
  draw
    editor textMorph selection asString evalString draw
  !
  menu: m shifted: _i
    m add: 'play (p)' target: self selector: #play.
    m add: 'stop (s)' target: SC3 selector: #reset.
    m add: 'draw (d)' target: self selector: #draw.
    ^m
  !
!

!SC3Editor methodsFor: 'initialization'!
  createWindow
    | t |
    t := PluggableTextMorph on: self text: #text accept: nil readSelection: nil menu: #menu:shifted:.
    t color: Color veryVeryLightGray.
    self editor: t.
    self addMorph: t frame: (0@0 corner: 1@1).
    self setLabel: 'SC3 Editor on: ' , self fileName
  !
!

SystemWindow subclass: #SC3Browser
  instanceVariableNames: 'directoryName index'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'SC3'
!

SC3Browser comment: 'List view on UGen graph fragments'!

!SC3Browser methodsFor: 'accessing'!
  directoryName ^directoryName
  !
  directoryName: x directoryName := x
  !
  index ^index
  !
  index: x index := x. self changed: #index
  !
  files ^(directoryName dirAllFilesWithExt: '.st') sort
  !
!

!SC3Browser class methodsFor: 'instance creation'!
  openOn: x
    | v |
    v := self new.
    v index: 0; directoryName: x; createWindow; openInWorld.
    ^v
  !
!

!SC3Browser methodsFor: 'operating'!
  play
    (self files at: index) evalFile play
  !
  edit
    SC3Editor openOn: (self files at: index)
  !
  draw
    (self files at: index) evalFile draw
  !
  menu: m shifted: _i
    m add: 'play (p)' target: self selector: #play.
    m add: 'stop (s)' target: SC3 selector: #reset.
    m add: 'edit (e)' target: self selector: #edit.
    m add: 'draw (d)' target: self selector: #draw.
    ^m
  !
  key: k from: _i
    k == $p ifTrue: [self play].
    k == $s ifTrue: [SC3 reset].
    k == $e ifTrue: [self edit].
    k == $d ifTrue: [self draw]
  !
!

!SC3Browser methodsFor: 'initialization'!
  createWindow
    | l |
    l := PluggableListMorph new.
    l on: self list: #files selected: #index changeSelected: #index: menu: #menu:shifted: keystroke: #key:from:.
    l color: Color veryVeryLightGray.
    self addMorph: l frame: (0@0 corner: 1@1).
    self setLabel: 'SC3 Browser on: ', self directoryName
  !
!
