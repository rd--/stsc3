!UGen methodsFor: 'printing'!
 printLispOn: fd
  '(construct-ugen ' putOn: fd .
  name asString printLispOn: fd . fd space .
  rate putOn: fd . fd space .
  '(list ' putOn: fd . paramNames do: [:x | (self perform: x) printLispOn: fd . fd space] . ') ' putOn: fd .
  "LISP DOES NOT ALLOW MULTIPLE MCE INPUTS"
  mceNames isEmpty ifTrue: ['nil' putOn: fd] ifFalse: [(self perform: (mceNames at: 1)) printLispOn: fd] . fd space .
  numChan putOn: fd . fd space .
  specialIndex notNil ifTrue: [specialIndex putOn: fd] ifFalse: ['nil' putOn: fd] . fd space .
  '(make-uid ' putOn: fd . self hash putOn: fd . ')' putOn: fd . ')' putOn: fd !
 writeLispTo: fn |fd| fd := FileStream newFileNamed: fn . self printLispOn: fd . fd cr . fd close ! !

!KR methodsFor: 'printing'!
printLispOn: fd '(KR ' putOn: fd . source printLispOn: fd . ')' putOn: fd ! !

!MCE methodsFor: 'printing'!
printLispOn: fd source printLispOn: fd . ! !

!Mix methodsFor: 'printing'!
printLispOn: fd '(mix ' putOn: fd . source printLispOn: fd . ')' putOn: fd ! !

!Fraction methodsFor: 'printing'!
printLispOn: fd (self asFloat) putOn: fd ! !

!Number methodsFor: 'printing'!
printLispOn: fd self putOn: fd ! !

!Array methodsFor: 'printing'!
 printLispOn: fd
  '(make-mce (list ' putOn: fd .
  self do: [:x | x printLispOn: fd . ] separatedBy: [fd space] .
  '))' putOn: fd ! !

!String methodsFor: 'printing'!
printLispOn: fd $" putOn: fd . self putOn: fd . $" putOn: fd ! !
