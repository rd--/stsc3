'/home/rohan/sw/stsc3/help/' fileListOn. "open file browser on named directory"
'/home/rohan/sw/stsc3/help/stsc3.help.st' workspaceOn. "open Workspace on named file"

SC3Browser openOn: '/home/rohan/sw/stsc3/help/' "open ugen graph browser on stsc3/help"

"Envelope"

Env initialize. "build dictionary (class variable)"
Env curveNameDictionary. "curve name dictionary"

"UGen"

SinOsc freq: 440 phase: 0. "freq:phase: is a class method"
SinOsc new freq: 440 ; phase: 0. "freq: and phase: are instance methods and ; is the cascade operator"
SinOsc new inspect. "initialize sets default values for parameters"

"File I/O - Text"

'/tmp/str.text' writeTextFile: [:fd | 5.0 asString putOn: fd].
'/tmp/str.text' readTextFile.

"File I/O - Directory"

'/home/rohan/sw/stsc3/help/' dirAllFilesWithExt: '.st'. "recursive directory listing by file extension"

"Evaluate"

'1+1' evalString == 2. "-> true ; evaluate string"
'/home/rohan/sw/stsc3/help/graph/jmcc-analog-bubbles.st' evalFile play. "evaluate text file"

"SC3"

SC3 reset. "C-c C-."

"UI"

'/home/rohan/sw/stsc3/lib/png/smalltalk-balloon.png' viewImageFile. "viewer for image file"
'/home/rohan/sw/stsc3/lib/png/squeak-mouse.png' viewImageFile. "viewer for image file"
'/home/rohan/sw/stsc3/help/graph/jmcc-analog-bubbles.st' viewTextFile. "editor for text file ; play, stop and draw buttons"

"Mouse ; Cursor ; Hand"

World activeHand position. "get mouse position"

"Browser"

Browser openBrowser. "open Browser"
Browser newOnCategory: 'SC3'. "open Browser on Category"
Browser newOnClass: UGen. "open Browser on Class"
Browser newOnClass: UGen messageCategory: 'rate control'. "open Browser on Protocol of Class"
Browser newOnClass: UGen selector: #kr. "open Browser on Method of Class"

"Transcript"

Transcript open. "-> opens Transcript ; MVC"
1 to: 5 do: [:x | Transcript cr ; show: x]. "-> prints to Transcript"

"Method Finder"

ToolBuilder open: SelectorBrowser new. "open Method Finder (Selector Browser) tool ; squeak ; MVC"
SelectorBrowser new open. "squeak ; MVC"

"Message Names"

MessageNames openMessageNames. "open Message Names tool"

"Random - Float"

5.0 rand. "-> random Float in 0 to 5"
2.0 rand: 3.0. "-> random Float in 2 to 3"
4.0 rand2. "-> random Float in -4 to 4"

100 arrayFill: [:i | 5.0 rand]. "-> 100 random Floats in 0 to 5"

"Random - Integer"

5 rand. "-> random Integer in 1 to 5 ; i.e. atRandom"
1 rand: 5. "-> random Integer in 1 to 5"
4 rand2. "-> random Integer in -4 to 4"

100 arrayFill: [:i | 5 rand]. "-> 100 random Integers in 0 to 5"

"Random - Array"

{1. 2. 3. 4. 5} choose. "random element of Array ; i.e. atRandom"
100 arrayFill: [:i | {#a. #b. #c. #d. #e} choose]. "-> 100 randomly chosen symbols"

"Array"

#(1 2 3 4 5). "literal array syntax ; {1. 2. 3. 4. 5}"
#(#(1 2) #(3 4)). "literal array syntax ; {{1. 2}. {3. 4}}"

1 to: 5 collect: [:x | x * x]. "-> #(1 4 9 16 25)"
6 replicate: 1. "-> #(1 1 1 1 1 1)"
#(#(1 2) #(3 4)) concatenation. "-> #(#(1 3) #(2 4))"
#(#(1 2) #(3 4)) transpose concatenation. "-> #(1 3 2 4)"
5 mixFill: [:x | x * x]. "-> 30 ; mixFill is zero indexed (as in supercollider)"
5 arrayFill: [:x | x * x]. "-> #(1 4 9 16 25)"
5 arrayFillZeroIndexed: [:x | x * x]. "-> #(0 1 4 9 16)"

Array with: 1 with: 2.3 with: 'str'. "{1. 2.3. 'str'}"

"Array - Math"

3 adaptToCollection: #(1 2 3) andSend: #*
#(1 2 3) * 3

"Interval"

(Interval from: 5 to: 9) asArray "-> #(5 6 7 8 9)"
(Interval from: 5 to: 15 by: 2) asArray "-> #(5 7 9 11 13 15)"

"Strings"

'file' , '.ext' = 'file.ext'. "-> true"

"Symbols"

#x isSymbol = true. "-> true"

"Haskell - List"

(1 to: 5) first = 1. "-> 1 ; head"
(1 to: 5) last = 5. "-> 5"
(1 to: 5) size = 5. "-> 5 ; length"
(1 to: 5) collect: [:x | x * x]. "-> #(1 4 9 16 25) ; map"
(1 to: 5) select: [:x | x > 3]. "-> #(4 5) ; filter"
(1 to: 5) copyFrom: 2 to: 5. "-> #(2 3 4 5) ; drop"
(1 to: 5) at: 3. "-> 3 ; haskell=! ; one-indexed"

"Dictionary"

d := Dictionary new. d at: 'select' put: 'x' ; at: 'from' put: 'y' ; at: 'list' put: 'z'.
d := Dictionary newFrom: {'select' -> 'x'. 'from' -> 'y'. 'list' -> 'z'}.
d := Dictionary newFromPairs: {'select'. 'x'. 'from'. 'y'. 'list'. 'z'}.

"Conditionals"

x := nil.
x isNil ifTrue: ['x'] ifFalse: ['y']. "-> x"

"Collections"

#(1 2 3 4 5) sum = 15. "-> true"

"Class"

SinOsc class. "SinOsc class"
SinOsc new class. "SinOsc"

"Morph"

menu := PopUpMenu labelArray: #('circle' 'oval' 'square' 'rectangle' 'triangle') lines: #(2 4).
menu startUpWithCaption: 'Choose a shape'

(UpdatingStringMorph on: Time selector: #now) openInHand. "clock"

"UI"

ListChooser chooseItemFrom: #(select an item from an array) title: 'Select:' addAllowed: true
ListChooser chooseIndexFrom: #(select an item from an array) title: 'Select:' addAllowed: true

ListMultipleChooser chooseItemListFrom: #(select an item from an array) title: 'Select:'
ListMultipleChooser chooseIndexListFrom: #(select an item from an array) title: 'Select:'

DirectoryChooserDialog openOn: (FileDirectory on: '/home/rohan/sw/stsc3/help/') label: 'Select:'
FileChooserDialog openOn: (FileDirectory on: '/home/rohan/sw/stsc3/help/') pattern: '*.st' label: 'Select:'
FileSaverDialog openOn: (FileDirectory on: '/home/rohan/sw/stsc3/help/') initialFilename: 'x.st' label: 'Select:'

UIManager default chooseDirectory
UIManager default chooseFrom: #(select from list) "few items = buttons"
UIManager default chooseFrom: ((100 to: 125) asArray) "many items = list"
UIManager default confirm: 'really?'
UIManager default confirm: 'really?' orCancel: [Transcript show: 'cancel' ; cr]
UIManager default edit: 'edit text'
UIManager default edit: 'edit text' label: 'make changes?' accept: [:x | Transcript show: x ; cr]
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

FileList open. "opens FileList ; default directory"
FileList openInMVC. "squeak ; MVC"
FileList openOn: (FileDirectory on: '/home/rohan/sw/stsc3/help/'). "squeak"
FileList openOn: '/home/rohan/sw/stsc3/help/'. "pharo"

"ProcessBrowser"

ProcessBrowser open. "open process browser ; MVC"

"Workspace"

Workspace open. "opens workspace ; Squeak ; MVC"
Workspace open contents: 'Workspace open .'. "pharo"
Workspace open openFile: '/home/rohan/sw/stsc3/help/stsc3.st'. "pharo"

"Message Names"

MessageNames openMessageNames.
ToolBuilder open: (MessageNames methodBrowserSearchingFor: 'freqscale').

'/tmp/hsc3.png' viewImageFile. "UI image file viewer"

"OS ; squeak"

MCMcmUpdater updateFromRepository: 'http://www.squeaksource.com/OSProcess'.
MCMcmUpdater updateFromRepository: 'http://www.squeaksource.com/CommandShell'.

"OS ; cuis"

Feature require: 'OSProcess'

"OS ; pharo (use LibC)"

Metacello new configuration: 'OSProcess'; repository: 'http://www.squeaksource.com/MetacelloRepository'; load
Metacello new configuration: 'CommandShell'; repository: 'http://www.squeaksource.com/MetacelloRepository'; load

OSProcess command: 'ls -l /etc'
OSProcess waitForCommand: 'echo sleeping...; sleep 3; echo I just slept for three seconds'

OSProcess outputOf: 'ls | cat | wc'
OSProcess editOutputOf: 'ps -axjf'

"Squeak"

(FileChooserDialog openOn: (FileDirectory on: '/home/rohan/sw/stsc3/help/graph/') suffixList: #(st) label: 'select st file') evalTextFile play. "file chooser on dir"

FileList2 modalFileSelectorForSuffixes: #(st) directory: (FileDirectory on: '/home/rohan/sw/stsc3/help/'). "file chooser on dir"

'a text' edit. "-> UIManager default edit: 'edit text'"

Display writePNGfileNamed: '/tmp/display.png'. "squeak ; screenshot"

Form fromUser asMorph openInWorld. "squeak ; screenshot selection"

(ImageReadWriter formFromFileNamed: '/tmp/hsc3.png') asMorph openInHand. "squeak ; image display"

(Form fromFileNamed: '/tmp/hsc3.png') asMorph openInHand. "squeak ; image display"

"File I/O - Object"

d := {'a'->1. 'b'->2. 'c'->'3'} as: Dictionary.
FileStream forceNewFileNamed: '/tmp/dict.st' do: [:out | d storeOn: out]. "store"
FileStream oldFileNamed: '/tmp/dict.st' do:  [:in | Object readFrom: in]. "read"

"Smalltalk"

Smalltalk garbageCollect.
Smalltalk aboutThisSystem. "System Reporter"

"Math"

0 raisedTo: 1. "-> 0"
1 raisedTo: 0. "-> 1"
0 raisedTo: 0. "-> 1 (gst = error)"
0.0 raisedTo: 1.0. "-> 0.0"
1.0 raisedTo: 0.0. "-> 1.0"
0.0 raisedTo: 0.0. "-> 1.0"
1 / 0. "-> error"
1.0 / 0.0. "-> Inf"
Float pi. "-> 3.141592653589793"

"GNU Smalltalk"

Smalltalk system: 'ls /home/rohan/sw/stsc3/README'.
Smalltalk getenv: 'HOME'. "/home/rohan"

ObjectMemory snapshot. "save image file"

FileStream fileIn: 'SC3-Base.st'.
FileStream fileIn: 'SC3-Core.st'.
FileStream fileIn: 'SC3-Random.st'.
FileStream fileIn: 'SC3-Math.st'.
FileStream fileIn: 'SC3-UGen.st'.
FileStream fileIn: 'SC3-UGen-Composite.st'.
FileStream fileIn: 'SC3-Env.st'.
FileStream fileIn: 'SC3-Haskell.st'.
FileStream fileIn: 'SC3-Gnu.st'.

"Help Graphs"

'/home/rohan/sw/stsc3/help/graph/f0-tw-0041.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/f0-tw-0045.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/f0-tw-0077.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/f0-tw-0084.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/f0-tw-0121.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/f0-tw-0125.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/f0-tw-0456384156159574016.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/f0-tw-1125557217086857216.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/f0-tw-1254441448327479299.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/f0-tw-1338987643057688579.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/f0-tw-1343283697840226307.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jar-1-4Qx.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jar-rk_20120422.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jl-1-9.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jl-bitwise.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jl-rain-thunder.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-analog-bubbles-mouse.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-analog-bubbles.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-babbling-brook.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-berlin-1977.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-birdies.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-demanding-studies.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-harmonic-swimming.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-harmonic-tumbling.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-harmonic-zither.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-lfo-modulation.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-modal-space.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-moto-rev.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-narrow-band-filtered-crackle-noise.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-plucked-strings.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-pulsing-bottles-ir.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-pulsing-bottles.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-random-pulsations.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-random-sine-waves.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-reverberated-sine-percussion.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-sample-and-hold-liquidities.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-scratchy.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-sprinkler-mouse.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-sprinkler.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-strummable-guitar.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-strummable-silk.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-sweepy-noise.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-synthetic-piano.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-tank.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-theremin.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jmcc-why-supercollider.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/jrhb-gcd.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/nc-sctutorial.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/nv-tw-41.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/pj-forest-sounds.st' evalFile play.
'/home/rohan/sw/stsc3/help/graph/tw-tw-01.st' evalFile play.

SC3 reset.
