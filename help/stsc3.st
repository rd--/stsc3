(SinOsc freq: 440 phase: 0) * 0.1

(SinOsc freq: (KR source: ((SinOsc freq: 1 phase: 0) * 4 + 200)) phase: 0) * 0.1

Out bus: 0 channelsArray: (Pan2 in: (SinOsc freq: 440 phase: 0) s: 0 level: 1)

o := SinOsc freq: 440 phase: 0
o hash
o freq
o perform: #freq
o paramNames collect: [:x | o perform: x]
o class == SinOsc
o class name == #SinOsc

69 midicps

1 to: 5 do: [:x| Transcript cr ; show x]
1 to: 5 collect: [:x| x * x]
2 mceFill: [:_x| PinkNoise new]
5 mixFill: [:x| x * x]

5.0 rand      "in 0 to 5"
2.0 rand: 3.0 "in 2 to 3"
4.0 rand2     "in -4 to 4"

#{1 . 2 . 3 . 4 . 5} atRandom "choose"

Compiler evaluate: '1+1' "evaluate string"

"Dictionary"

d := Dictionary new . d at: 'select' put: 'x' ; at: 'from' put: 'y' ; at: 'list' put: 'z' .
d := Dictionary newFromPairs: #(select x from y list z)

"SC3"

'/tmp/hsc3.png' viewImageFile .
'/home/rohan/sw/stsc3/help/graph/jmcc-analog-bubbles.st' readTextFile .
'/home/rohan/sw/stsc3/help/graph/jmcc-analog-bubbles.st' evalTextFile .
'/home/rohan/sw/stsc3/help/graph/jmcc-analog-bubbles.st' viewTextFile .

'/home/rohan/sw/stsc3/help/graph/jmcc-analog-bubbles.st' evalTextFile play.
SC3 reset .

(FileChooserDialog openOn: (FileDirectory on: '/home/rohan/sw/stsc3/help/graph/') suffixList: #(st) label: 'select st file') evalTextFile play

GraphBrowser openOn: '/home/rohan/sw/stsc3/help/graph/' .


"Morph"

menu := PopUpMenu labelArray: #('circle' 'oval' 'square' 'rectangle' 'triangle') lines: #(2 4).
menu startUpWithCaption: 'Choose a shape'

(UpdatingStringMorph on: Time selector: #now) openInHand .

"UI"

UIManager default chooseClassOrTrait
UIManager default chooseDirectory
UIManager default chooseDirectoryFrom: (FileDirectory on: '/home/rohan/sw/stsc3/help/')
UIManager default chooseFileMatching: nil
UIManager default chooseFileMatchingSuffixes: #('st')
UIManager default saveFilenameRequest: 'save file' initialAnswer: 'x.st'
UIManager default chooseFrom: #(select from list)
UIManager default chooseFromLabeledValues: (Dictionary newFromPairs: #(select x from y list z))
UIManager default chooseMultipleFrom: #(select multiple from list)
UIManager default confirm: 'really?'
UIManager default confirm: 'really?' orCancel: [Transcript show: 'cancel' ; cr]
UIManager default inform: 'information'
UIManager default edit: 'edit text'
UIManager default edit: 'edit text' label: 'make changes?' accept: [:x| Transcript show: x ; cr]
UIManager default request: 'name?'
UIManager default requestPassword: 'pwd?'
UIManager default multiLineRequest: 'text' centerAt: Sensor peekPosition initialAnswer: 'type text' answerHeight: 600

FileList openOn: (FileDirectory on: '/home/rohan/sw/stsc3/help/')

'a text' edit

'/tmp/hsc3.png' viewImageFile "UI image file viewer"

Display writePNGfileNamed: '/tmp/display.png' . "screenshot"
Form fromUser asMorph openInWorld
(ImageReadWriter formFromFileNamed: '/tmp/hsc3.png') asMorph openInHand
(Form fromFileNamed: '/tmp/hsc3.png') asMorph openInHand


"OS"

MCMcmUpdater updateFromRepository: 'http://www.squeaksource.com/OSProcess' .
MCMcmUpdater updateFromRepository: 'http://www.squeaksource.com/CommandShell'.

OSProcess command: 'ls -l /etc'
OSProcess waitForCommand: 'echo sleeping...; sleep 3; echo I just slept for three seconds'
OSProcess outputOf: 'ls | cat | wc'
OSProcess editOutputOf: 'ps -axjf'
