GraphBrowser openOn: '/home/rohan/sw/stsc3/help/' .

"Envelope"

Envelope initialize . "build dictionary (class variable)"
Envelope curveNameDictionary . "curve name dictionary"

"File I/O - Text"

'/tmp/str.text' writeTextFile: [ :fd | 5.0 asString putOn: fd  ] .

FileStream oldFileNamed: '/home/rohan/sw/stsc3/help/stsc3.st' do:  [:in | in contentsOfEntireFile ] . "squeak"
'/home/rohan/sw/stsc3/help/stsc3.st' asFileEntry textContents . "squeak - FM library"
'/home/rohan/sw/stsc3/help/stsc3.st' asFileReference readStream upToEnd . "pharo"

"File I/O - Object"

d := {'a'->1 . 'b'->2 . 'c'->'3'} as: Dictionary.
FileStream forceNewFileNamed: '/tmp/dict.st' do: [:out | d storeOn: out] . "store"
FileStream oldFileNamed: '/tmp/dict.st' do:  [:in | Object readFrom: in] . "read"

"File I/O - Directory"

(FileDirectory on: '/home/rohan/sw/stsc3/help/') fullNamesOfAllFilesInSubtree . "recursive directory listing ; squeak"

'/home/rohan/sw/stsc3/help/' asFileReference allChildren . "recursive directory listing ; pharo"
'/home/rohan/sw/stsc3/help/' asFileReference allChildrenMatching: '*.st' . "recursive directory listing with pattern; pharo"

"Transcript"

Transcript open . "-> opens Transcript"
1 to: 5 do: [:x| Transcript cr ; show: x] . "-> prints to Transcript"

"Random - Float"

5.0 rand . "-> random Float in 0 to 5"
2.0 rand: 3.0 . "-> random Float in 2 to 3"
4.0 rand2 . "-> random Float in -4 to 4"

"Random - Integer"

5 rand . "-> random Integer in 0 to 5"
1 rand: 5 . "-> random Integer in 1 to 5"
4 rand2 . "-> random Integer in -4 to 4"

"Random - Array"

{1 . 2 . 3 . 4 . 5} atRandom . "random element of Array ; SC3 choose"
50 arrayFill: [:_i | 1 rand: 5] . "array of random Integers in 1 to 5"

"Array"

#(1 2 3 4 5) . "literal array syntax ; {1 . 2 . 3 . 4 . 5}"
#((1 2) (3 4)) . "literal array syntax ; {{1 . 2} . {3 . 4}}"

1 to: 5 collect: [:x| x * x] . "-> #(1 4 9 16 25)"
6 replicate: 1 . "-> #(1 1 1 1 1 1)"
#((1 2) (3 4)) concatenation . "-> #((1 3) (2 4))"
#((1 2) (3 4)) transpose concatenation . "-> #(1 3 2 4)"
5 mixFill: [:x| x * x] . "-> 55"

"Symbols"

#x isSymbol . "-> true"

"Haskell - List"

(1 to: 5) first . "-> 1 ; head"
(1 to: 5) last . "-> 5"
(1 to: 5) size . "-> 5 ; length"
(1 to: 5) collect: [:x| x * x] . "-> #(1 4 9 16 25) ; map"
(1 to: 5) select: [:x| x > 3] . "-> #(4 5) ; filter"
(1 to: 5) copyFrom: 2 to: 5 . "-> #(2 3 4 5) ; drop"
(1 to: 5) at: 3 . "-> 3 ; ! ; one-indexed"

"Evaluate"

Compiler evaluate: '1+1' "squeak ; evaluate string"
OpalCompiler evaluate: '1+1' "pharo ; evaluate string"

"Dictionary"

d := Dictionary new . d at: 'select' put: 'x' ; at: 'from' put: 'y' ; at: 'list' put: 'z' .
d := Dictionary newFromPairs: #(select x from y list z)

"Conditionals"

x := nil .
x isNil ifTrue: ['x'] ifFalse: ['y'] .
x isNil ifTrue: 'x' ifFalse: 'y' .

"SC3"

SC3 reset . "C-c C-."

'/tmp/hsc3.png' viewImageFile .
'/home/rohan/sw/stsc3/help/graph/jmcc-analog-bubbles.st' readTextFile . "read text file"
'/home/rohan/sw/stsc3/help/graph/jmcc-analog-bubbles.st' evalTextFile play . "read and evaluate text file"
'/home/rohan/sw/stsc3/help/graph/jmcc-analog-bubbles.st' viewTextFile . "editor for text file with play, stop and draw buttons"

"Morph"

menu := PopUpMenu labelArray: #('circle' 'oval' 'square' 'rectangle' 'triangle') lines: #(2 4).
menu startUpWithCaption: 'Choose a shape'

(UpdatingStringMorph on: Time selector: #now) openInHand . "clock"

"UI"

UIManager default chooseDirectory
UIManager default chooseFrom: #(select from list)
UIManager default confirm: 'really?'
UIManager default confirm: 'really?' orCancel: [Transcript show: 'cancel' ; cr]
UIManager default edit: 'edit text'
UIManager default edit: 'edit text' label: 'make changes?' accept: [:x| Transcript show: x ; cr]
UIManager default inform: 'the information'
UIManager default request: 'name?'
UIManager default requestPassword: 'pwd?'

"squeak=yes pharo=no"
UIManager default chooseClassOrTrait
UIManager default chooseDirectoryFrom: (FileDirectory on: '/home/rohan/sw/stsc3/help/')
UIManager default chooseFileMatching: nil
UIManager default chooseFileMatchingSuffixes: #('st')
UIManager default chooseFromLabeledValues: (Dictionary newFromPairs: #(select x from y list z))
UIManager default chooseMultipleFrom: #(select multiple from list)
UIManager default multiLineRequest: 'text' centerAt: Sensor peekPosition initialAnswer: 'type text' answerHeight: 600
UIManager default saveFilenameRequest: 'save file' initialAnswer: 'x.st'

"pharo=yes squeak=no"
UIManager default chooseDirectoryFrom: ('/home/rohan/sw/stsc3/help/' asFileReference)
UIManager default chooseFullFileNameMatching: nil

"FileList"

FileList open .
FileList openOn: (FileDirectory on: '/home/rohan/sw/stsc3/help/') . "squeak"
FileList openOn: '/home/rohan/sw/stsc3/help/' . "pharo"

"Workspace"

Workspace open .
Workspace open contents: 'Workspace open .' . "pharo"
Workspace open openFile: '/home/rohan/sw/stsc3/help/stsc3.st' . "pharo"

'/tmp/hsc3.png' viewImageFile "UI image file viewer"

"OS ; squeak"
MCMcmUpdater updateFromRepository: 'http://www.squeaksource.com/OSProcess' .
MCMcmUpdater updateFromRepository: 'http://www.squeaksource.com/CommandShell'.

"OS ; pharo"
Metacello new configuration: 'OSProcess'; repository: 'http://www.squeaksource.com/MetacelloRepository'; load
Metacello new configuration: 'CommandShell'; repository: 'http://www.squeaksource.com/MetacelloRepository'; load

OSProcess command: 'ls -l /etc'
OSProcess waitForCommand: 'echo sleeping...; sleep 3; echo I just slept for three seconds'

OSProcess outputOf: 'ls | cat | wc'
OSProcess editOutputOf: 'ps -axjf'

"Squeak"

(FileChooserDialog openOn: (FileDirectory on: '/home/rohan/sw/stsc3/help/graph/') suffixList: #(st) label: 'select st file') evalTextFile play . "file chooser on dir"

FileList2 modalFileSelectorForSuffixes: #(st) directory: (FileDirectory on: '/home/rohan/sw/stsc3/help/') . "file chooser on dir"

'a text' edit . "-> UIManager default edit: 'edit text'"

Display writePNGfileNamed: '/tmp/display.png' . "squeak ; screenshot"

Form fromUser asMorph openInWorld . "squeak ; screenshot selection"

(ImageReadWriter formFromFileNamed: '/tmp/hsc3.png') asMorph openInHand . "squeak ; image display"

(Form fromFileNamed: '/tmp/hsc3.png') asMorph openInHand . "squeak ; image display"
