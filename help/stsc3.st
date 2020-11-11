SinOsc freq: 440 phase: 0

SinOsc freq: (KR source: (SinOsc freq: 2 phase: 0)) phase: 0

Out bus: 0 channelsArray: (Pan2 in: (SinOsc freq: 440 phase: 0) pos: 0 level: 1)

o := SinOsc freq: 440 phase: 0
o hash
o freq
o perform: #freq
o paramNames collect: [:x | o perform: x]
o class == SinOsc
o class name == #SinOsc

69 midicps

"UI"

UIManager default chooseClassOrTrait
UIManager default chooseDirectory
UIManager default chooseFileMatching: nil
UIManager default chooseFileMatchingSuffixes: #('st')
UIManager default saveFilenameRequest: 'save file' initialAnswer: 'x.st'
UIManager default edit: 'a text'
UIManager default edit: 'a text' label: 'make changes?' accept: [:x| Transcript show: x ; cr]
UIManager default request: 'name?'
UIManager default requestPassword: 'pwd?'
UIManager default multiLineRequest: 'text' centerAt: Sensor peekPosition initialAnswer: 'type' answerHeight: 600
