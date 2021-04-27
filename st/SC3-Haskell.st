!UGen methodsFor: 'printing'!
 printHaskellOn: fd
  "Print my hsc3 representation."
  |mce nc|
  '(mkUGen Nothing all_rates ' putOn: fd.
  ugenRate isArray
   ifTrue: ['(Right ' putOn: fd. ugenRate printHaskellArrayOn: fd. ') ' putOn: fd]
   ifFalse: ['(Left ' putOn: fd. ugenRate asString asUppercase putOn: fd. ') ' putOn: fd].
  ugenName asString printHaskellOn: fd.
  fd space.
  '[' putOn: fd.
  paramNames do: [:x | (self perform: x) printHaskellOn: fd. fd space] separatedBy: [',' putOn: fd].
  '] ' putOn: fd.
  mce := ['(Just [' putOn: fd.
          mceNames do: [:x | (self perform: x) printHaskellOn: fd. fd space] separatedBy: [',' putOn: fd].
          ']) ' putOn: fd].
  mceNames isEmpty ifTrue: ['Nothing ' putOn: fd] ifFalse: mce.
  nc := ['(length (mceChannels ' putOn: fd.
         (self perform: (mceNames at: 1)) printHaskellOn: fd.
         '))' putOn: fd].
  numChan = #mceInput ifTrue: nc ifFalse: [numChan asString putOn: fd].
  fd space.
  '(Special ' putOn: fd.
  specialIndex notNil ifTrue: [specialIndex asString putOn: fd] ifFalse: ['0' putOn: fd]
 . ') ' putOn: fd.
  '(UId ' putOn: fd.
  self hash asString putOn: fd.
  ')' putOn: fd.
  ')' putOn: fd
 !
 writeHaskellTo: fn
  "Write my hsc3 representation to a file."
  fn writeTextFile: [:fd | self printHaskellOn: fd. fd cr]
 !
 play
  "Write my hsc3 representation to a temporary file and run hsc3-graphs to play me."
  self writeHaskellTo: '/tmp/st.hs'.
  'hsc3-graphs fragments hs play /tmp/st.hs' systemCommand
 !
 draw
  "Write my hsc3 representation to a temporary file and run hsc3-graphs to draw me."
  self writeHaskellTo: '/tmp/st.hs'.
  'hsc3-graphs fragments hs draw /tmp/st.hs' systemCommand
 !
 printUGens
  "Write my hsc3 representation to a temporary file and run hsc3-graphs to print me."
  self writeHaskellTo: '/tmp/st.hs'.
  'hsc3-graphs fragments hs dump-ugens /tmp/st.hs' systemCommand
 !
!

!Rate methodsFor: 'printing'!
 printHaskellOn: fd
  "Print my haskell representation."
  '(rewriteToRate ' putOn: fd.
  rate asString asUppercase putOn: fd.
  fd space.
  source printHaskellOn: fd.
  ')' putOn: fd
 !
!

!MCE methodsFor: 'printing'!
 printHaskellOn: fd
  "Print my haskell representation."
  transform notNil ifTrue: [transform putOn: fd. ' ' putOn: fd].
  source printHaskellOn: fd
 !
!

!MRG methodsFor: 'printing'!
 printHaskellOn: fd
  "Print my haskell representation."
  '(mrg2 ' putOn: fd.
  lhs printHaskellOn: fd.
  rhs printHaskellOn: fd.
  ')' putOn: fd
 !
!

!Mix methodsFor: 'printing'!
 printHaskellOn: fd
  "Print my haskell representation."
  '(mix ' putOn: fd.
  source printHaskellOn: fd.
  ')' putOn: fd
 !
!

!Fraction methodsFor: 'printing'!
 printHaskellOn: fd
  "Print my haskell representation."
  (self asFloat asString) putOn: fd
 !
!

!Number methodsFor: 'printing'!
 printHaskellOn: fd
  "Print my haskell representation."
  self asString putOn: fd
 !
!

!Array methodsFor: 'printing'!
 printHaskellArrayOn: fd
  "Print my haskell representation."
  '[' putOn: fd.
  self do: [:x | x printHaskellOn: fd. ] separatedBy: [',' putOn: fd].
  ']' putOn: fd
 !
 printHaskellOn: fd
  "Print my hsc3 MCE representation."
  '(mce ' putOn: fd.
  self printHaskellArrayOn: fd.
  ')' putOn: fd
 !
!

!String methodsFor: 'printing'!
 printHaskellOn: fd
  "Print my haskell representation."
  $" putOn: fd.
  self putOn: fd.
  $" putOn: fd
 !
!

!Symbol methodsFor: 'printing'!
 printHaskellOn: fd
  "Print my haskell representation."
  self putOn: fd
 !
!

!Splay methodsFor: 'printing'!
 printHaskellOn: fd
  "Print my hsc3 representation."
 '(splay ' putOn: fd.
 #(#input #spread #level #center #levelComp) do: [:x| (self perform: x) printHaskellOn: fd] separatedBy: [' ' putOn: fd].
 ') ' putOn: fd
 !
!

!True methodsFor: 'printing'!
 printHaskellOn: fd
  "Print my haskell representation."
  'True' putOn: fd
 !
!

!False methodsFor: 'printing'!
 printHaskellOn: fd
  "Print my haskell representation."
  'False' putOn: fd
 !
!

!Control methodsFor: 'printing'!
 printHaskellOn: fd
  "Print my haskell representation."
  '(control KR ' putOn: fd.
  self name asString printHaskellOn: fd.
  fd space.
  self init asString putOn: fd.
  ') ' putOn: fd
 !
!

!Constant methodsFor: 'printing'!
 printHaskellOn: fd
  "Print my haskell representation."
  '(constant ' putOn: fd.
  self k asString putOn: fd.
  ') ' putOn: fd
 !
!