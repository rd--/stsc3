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
editor: x editor := x!
!

!Sc3Editor class methodsFor: 'instance creation'!
openOn: x
    ^self new fileName: x; createWindow; openInWorld; yourself!
!

!Sc3Editor methodsFor: 'operating'!
text
    ^fileName readTextFile!
play
    | text |
    text := editor textMorph selection asString.
    (Sc3 evalText: text fromFile: fileName) play!
draw
    | text |
    text := editor textMorph selection asString.
    (Sc3 evalText: text fromFile: fileName) draw!
menu: m shifted: i
    m add: 'play (p)' target: self selector: #play.
    m add: 'stop (s)' target: Sc3 selector: #reset.
    m add: 'draw (d)' target: self selector: #draw.
    ^m!
!

!Sc3Editor methodsFor: 'initialization'!
createWindow
    | t |
    t := PluggableTextMorph on: self text: #text accept: nil readSelection: nil menu: #menu:shifted:.
    t color: Color veryVeryLightGray.
    self editor: t.
    self addMorph: t frame: (0@0 corner: 1@1).
    self setLabel: 'Sc3 Editor on: ' , self fileName!
!
