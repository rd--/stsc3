!String methodsFor: 'file io'!
readTextFile
|fd txt|
fd := FileStream fileNamed: self .
txt := fd contentsOfEntireFile .
fd close .
^ txt ! !

!String methodsFor: 'file io'!
evalTextFile ^ Compiler evaluate: (self readTextFile) ! !

!String methodsFor: 'user interface'!
viewImageFile
|i w|
i := (Form fromFileNamed: self) asMorph .
w := SystemWindow new .
w extent: (i extent) .
w setLabel: self .
w model: (Model new) .
w addMorph: i frame: (0@0 corner: 1@1) .
w openInWorldExtent: (i extent + (10@32)) ! !

!String methodsFor: 'user interface'!
viewTextFile
|txt b t w m|
w := SystemWindow new extent: 500@500 .
w setLabel: self .
w model: (Model new) .
t := (TextMorph new) string: (self readTextFile) fontName: 'Bitmap DejaVu Sans' size: 16 .
t backgroundColor: Color white .
w addMorph: t fullFrame: (LayoutFrame fractions: (0@0 corner: 1.0@0.8) offsets: (0@10 corner: 0@0)) .
b := AlignmentMorph newRow .
b color: Color veryVeryLightGray ; rubberBandCells: false ; vResizing: #shrinkWrap ; cellGap: 6@0 .
{{'Play' . [(Compiler evaluate: (t selection)) play]} . {'Stop' . [SC3 reset]} . {'Draw' . [(Compiler evaluate: (t selection)) draw]}} do: [:x | b addMorphBack: (SimpleButtonMorph new target: (x at: 2) ; label: (x at: 1) ; actionSelector: #value)] .
w addMorph: b fullFrame: (LayoutFrame fractions: (0.0@0.8 corner: 1.0@1.0) offsets: (0@0 corner: 0@0)) .
w openInWorldExtent: 500@350 ! !

SystemWindow subclass: #GraphBrowser
instanceVariableNames: 'directoryName index' classVariableNames: '' poolDictionaries: '' category: 'SC3' !
!GraphBrowser commentStamp: '<historical>' prior: 0!
List view on fragments. !

!GraphBrowser methodsFor: 'private'!
directoryName: x directoryName := x !
index: x index := x . self changed: #index ! !

!GraphBrowser methodsFor: 'accessing'!
directoryName ^ directoryName !
index ^ index !
files ^ ((FileDirectory on: directoryName) fullNamesOfAllFilesInSubtree) sort select: [:x | x endsWith: '.st'] ! !

!GraphBrowser class methodsFor: 'instance creation'!
openOn: x |v| v := self new . v index: 0 . v directoryName: x . v createWindow . v openInWorld . ^ v ! !

!GraphBrowser methodsFor: 'operating'!
play (self files at: index) evalTextFile play !
"edit (FileStream fileNamed: (self files at: index)) edit"
edit (self files at: index) viewTextFile !
draw (self files at: index) evalTextFile draw !
menu: m shifted: _i m add: 'play (p)' target: self selector: #play . m add: 'stop (s)' target: SC3 selector: #reset . m add: 'edit (e)' target: self selector: #edit . m add: 'draw (d)' target: self selector: #draw . ^ m !
key: k from: _i k == $p ifTrue: [self play] . k == $s ifTrue: [SC3 reset] . k == $e ifTrue: [self edit] . k == $d ifTrue: [self draw] ! !

!GraphBrowser methodsFor: 'initialization'!
createWindow
 |l|
 l := PluggableListMorph new .
 l on: self list: #files selected: #index changeSelected: #index: menu: #menu:shifted: keystroke: #key:from: .
 l color: Color veryVeryLightGray .
 self addMorph: l fullFrame: (LayoutFrame fractions: (0@0 corner: 1@1) offsets: (0@10 corner: 0@0)) .
 self setLabel: 'Graph Browser' ! !
