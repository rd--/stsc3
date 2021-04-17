!UGen methodsFor: 'printing'!
 printHaskellOn: fd
  |mce nc|
  '(mkUGen Nothing all_rates ' putOn: fd .
  '(Left ' putOn: fd .
  ugenRate asString asUppercase putOn: fd .
  ') ' putOn: fd .
  ugenName asString printHaskellOn: fd .
  fd space .
  '[' putOn: fd .
  paramNames do: [:x | (self perform: x) printHaskellOn: fd . fd space] separatedBy: [',' putOn: fd] .
  '] ' putOn: fd .
  mce := ['(Just [' putOn: fd .
          mceNames do: [:x | (self perform: x) printHaskellOn: fd . fd space] separatedBy: [',' putOn: fd] .
          ']) ' putOn: fd] .
  mceNames isEmpty ifTrue: ['Nothing ' putOn: fd] ifFalse: mce .
  nc := ['(length (mceChannels ' putOn: fd .
         (self perform: (mceNames at: 1)) printHaskellOn: fd .
         '))' putOn: fd] .
  numChan = #mceInput ifTrue: nc ifFalse: [numChan asString putOn: fd] .
  fd space .
  '(Special ' putOn: fd .
  specialIndex notNil ifTrue: [specialIndex asString putOn: fd] ifFalse: ['0' putOn: fd]
  . ') ' putOn: fd .
  '(UId ' putOn: fd .
  self hash asString putOn: fd .
  ')' putOn: fd .
  ')' putOn: fd !
 writeHaskellTo: fn
  fn writeTextFile: [:fd | self printHaskellOn: fd . fd cr] !
 play
  self writeHaskellTo: '/tmp/st.hs' .
  'hsc3-graphs fragments hs play /tmp/st.hs' systemCommand !
 draw
  self writeHaskellTo: '/tmp/st.hs' .
  'hsc3-graphs fragments hs draw /tmp/st.hs' systemCommand !
!

!Rate methodsFor: 'printing'!
 printHaskellOn: fd
  '(rewriteToRate ' putOn: fd .
  rate asString asUppercase putOn: fd .
  fd space .
  source printHaskellOn: fd . ')'
  putOn: fd !
!

!MCE methodsFor: 'printing'!
 printHaskellOn: fd
  transform notNil ifTrue: [transform putOn: fd . ' ' putOn: fd] .
  source printHaskellOn: fd !
!

!MRG methodsFor: 'printing'!
 printHaskellOn: fd
  '(mrg2 ' putOn: fd .
  lhs printHaskellOn: fd .
  rhs printHaskellOn: fd .
  ')' putOn: fd !
!

!Mix methodsFor: 'printing'!
 printHaskellOn: fd
  '(mix ' putOn: fd .
  source printHaskellOn: fd .
  ')' putOn: fd !
!

!Fraction methodsFor: 'printing'!
 printHaskellOn: fd
  (self asFloat asString) putOn: fd !
!

!Number methodsFor: 'printing'!
 printHaskellOn: fd
  self asString putOn: fd !
!

!Array methodsFor: 'printing'!
 printHaskellOn: fd
  '(mce [' putOn: fd .
  self do: [:x | x printHaskellOn: fd . ] separatedBy: [',' putOn: fd] .
  '])' putOn: fd !
!

!String methodsFor: 'printing'!
 printHaskellOn: fd
  $" putOn: fd .
  self putOn: fd .
  $" putOn: fd !
!

!Symbol methodsFor: 'printing'!
 printHaskellOn: fd
  self putOn: fd !
!

!Splay methodsFor: 'printing'!
 printHaskellOn: fd
 '(splay ' putOn: fd .
 #(#input #spread #level #center #levelComp) do: [:x| (self perform: x) printHaskellOn: fd] separatedBy: [' ' putOn: fd] .
 ') ' putOn: fd !
!

!True methodsFor: 'printing'!
 printHaskellOn: fd
  'True' putOn: fd !
!

!False methodsFor: 'printing'!
 printHaskellOn: fd
  'False' putOn: fd !
!

!Control methodsFor: 'printing'!
 printHaskellOn: fd
  "Print Control value"
  '(control KR ' putOn: fd .
  self name asString printHaskellOn: fd .
  fd space .
  self init asString putOn: fd .
  ') ' putOn: fd !
!
