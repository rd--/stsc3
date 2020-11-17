!UGen methodsFor: 'printing'!
 printHaskellOn: fd
  '(mkUGen Nothing all_rates ' putOn: fd .
  '(Left ' putOn: fd . rate asString asUppercase asSymbol putOn: fd . ') ' putOn: fd .
  name asString printHaskellOn: fd . fd space .
  '[' putOn: fd . paramNames do: [:x | (self perform: x) printHaskellOn: fd . fd space] separatedBy: [',' putOn: fd] . '] ' putOn: fd .
  mceNames isEmpty ifTrue: ['Nothing ' putOn: fd] ifFalse: ['(Just [' putOn: fd . mceNames do: [:x | (self perform: x) printHaskellOn: fd . fd space] separatedBy: [',' putOn: fd] . ']) ' putOn: fd ].
  numChan = #mceInput ifTrue: ['(length (mceChannels ' putOn: fd . (self perform: (mceNames at: 1)) printHaskellOn: fd . '))' putOn: fd] ifFalse: [numChan putOn: fd] . fd space .
  '(Special ' putOn: fd . specialIndex notNil ifTrue: [specialIndex putOn: fd] ifFalse: [0 putOn: fd] . ') ' putOn: fd .
  '(UId ' putOn: fd . self hash putOn: fd . ')' putOn: fd . ')' putOn: fd !
 writeHaskellTo: fn |fd| fd := FileStream forceNewFileNamed: fn . self printHaskellOn: fd . fd cr . fd close !
 play self writeHaskellTo: '/tmp/st.hs' . OSProcess command: 'hsc3-graphs fragments hs play /tmp/st.hs' ! !

!KR methodsFor: 'printing'!
printHaskellOn: fd '(rewriteToControlRate ' putOn: fd . source printHaskellOn: fd . ')' putOn: fd ! !

!DR methodsFor: 'printing'!
printHaskellOn: fd '(rewriteToDemandRate ' putOn: fd . source printHaskellOn: fd . ')' putOn: fd ! !

!MCE methodsFor: 'printing'!
printHaskellOn: fd source printHaskellOn: fd . ! !

!MRG methodsFor: 'printing'!
printHaskellOn: fd '(mrg2 ' putOn: fd . lhs printHaskellOn: fd . rhs printHaskellOn: fd . ')' putOn: fd ! !

!Mix methodsFor: 'printing'!
printHaskellOn: fd '(mix ' putOn: fd . source printHaskellOn: fd . ')' putOn: fd ! !

!Fraction methodsFor: 'printing'!
printHaskellOn: fd (self asFloat) putOn: fd ! !

!Number methodsFor: 'printing'!
printHaskellOn: fd self putOn: fd ! !

!Array methodsFor: 'printing'!
 printHaskellOn: fd
  '(mce [' putOn: fd .
  self do: [:x | x printHaskellOn: fd . ] separatedBy: [',' putOn: fd] .
  '])' putOn: fd ! !

!String methodsFor: 'printing'!
printHaskellOn: fd $" putOn: fd . self putOn: fd . $" putOn: fd ! !

!Symbol methodsFor: 'printing'!
printHaskellOn: fd self putOn: fd ! !

!Splay methodsFor: 'printing'!
printHaskellOn: fd
'(splay ' putOn: fd .
#(input spread level center levelComp) do: [:x| (self perform: x) printHaskellOn: fd] separatedBy: [' ' putOn: fd] .
') ' putOn: fd ! !

!True methodsFor: 'printing'!
printHaskellOn: fd #True putOn: fd ! !

!False methodsFor: 'printing'!
printHaskellOn: fd #False putOn: fd ! !

!Control methodsFor: 'printing'!
printHaskellOn: fd '(control KR ' putOn: fd . self key asString printHaskellOn: fd . self init putOn: fd . ') ' putOn: fd ! !
