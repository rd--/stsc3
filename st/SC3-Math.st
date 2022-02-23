!Number methodsFor: 'arithmetic'!
  midiCps
    "Convert from fractional midi note number to frequency (in hertz)"
    ^440 * (2 raisedTo: ((self - 69) * (1 / 12)))
  !
  cpsMidi
    "Cycles per second to fractional midi note number."
    ^(((self * (1 / 440)) log: 2) * 12) + 69
  !
  dbAmp
    "Decibels to linear amplitude"
     ^10 raisedTo: (self * 0.05)
  !
  ampDb
    "Linear amplitude to decibels"
    ^(self log: 10) * 20
!

!Float methodsFor: 'arithmetic'!
  linExpFrom: lo to: hi
    "Return myself (I am in range 0.0 to 1.0) to the indicated exponential range"
    ^lo * ((hi / lo) ln * self) exp
  !
!

!Collection methodsFor: 'arithmetic'!
  abs
    ^self collect: [:x| x abs]
  !
  midiCps
    ^self collect: [:x| x midiCps]
  !
  dbAmp
    ^self collect: [ :x| x dbAmp ]
  !
!

!SequenceableCollection methodsFor: 'arithmetic'!
  +~ other
    "Like + but cycles shorter collection"
    ^self withExtending: other collect: [:i :j| i + j]
  !
!
