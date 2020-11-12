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

1 to: 5 do: [:x| Transcript cr ; show x]
1 to: 5 collect: [:x| x * x]
2 mceFill: [:_x| PinkNoise new]
5 mixFill: [:x| x * x]

5.0 rand      "in 0 to 5"
2.0 rand: 3.0 "in 2 to 3"
4.0 rand2     "in -4 to 4"

#{1 . 2 . 3 . 4 . 5} atRandom "choose"

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
