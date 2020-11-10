!UGen methodsFor: 'printing'!
 printLispOn: fd
  '(construct-ugen ' putOn: fd .
  name asString printLispOn: fd . fd space .
  rate putOn: fd . fd space .
  '(list ' putOn: fd . paramNames do: [:x | (self perform: x) printLispOn: fd . fd space] . ') ' putOn: fd .
  mceNames isEmpty ifTrue: ['nil' putOn: fd] ifFalse: [(self perform: (mceNames at: 1)) printLispOn: fd] . fd space . "LISP DOES NOT ALLOW MULTIPLE MCE INPUTS"
  numChan putOn: fd . fd space .
  (paramNames includes: #specialIndex) ifTrue: [self specialIndex putOn: fd] ifFalse: ['nil' putOn: fd] . fd space .
  '(make-uid ' putOn: fd . self hash putOn: fd . ')' putOn: fd . ')' putOn: fd ! !

!KR methodsFor: 'printing'!
printLispOn: fd '(KR ' putOn: fd . source printLispOn: fd . ')' putOn: fd ! !

!SmallInteger methodsFor: 'printing'!
printLispOn: fd self putOn: fd ! !

!SmallFloat64 methodsFor: 'printing'!
printLispOn: fd self putOn: fd ! !

!Array methodsFor: 'printing'!
 printLispOn: fd
  '(make-mce (list ' putOn: fd .
  self do: [:x | x printLispOn: fd . ] separatedBy: [fd space] .
  '))' putOn: fd ! !

!String methodsFor: 'printing'!
printLispOn: fd $" putOn: fd . self putOn: fd . $" putOn: fd ! !
