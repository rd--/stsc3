!AbstractUgen methodsFor: 'printing'!
  writeHaskellTo: fn
    "Write my hsc3 representation to a file."
    | c |
    c := self ugenSet.
    c asArray with: (1 to: c size) do: [ :u :i | u ugenId: i ]. "<- assign id to Ugens for serialisation"
    fn writeTextFile: [ :fd |
     'let {' putOn: fd.
     c do: [:x | x printHaskellOn: fd. ' = ' putOn: fd. x printHaskellDefinitionOn: fd.] separatedBy: [fd cr. '; ' putOn: fd].
     '} in ' putOn: fd.
     self printHaskellOn: fd.
     fd cr]
  !
  play
    "Write my hsc3 representation to a temporary file and run hsc3-graphs to play me."
    self writeHaskellTo: '/tmp/st.hs'.
    'hsc3-graphs fragments hs min play /tmp/st.hs' systemCommand
  !
  draw
    "Write my hsc3 representation to a temporary file and run hsc3-graphs to draw me."
    self writeHaskellTo: '/tmp/st.hs'.
    'hsc3-graphs fragments hs min draw /tmp/st.hs' systemCommand
  !
  printUgens
    "Write my hsc3 representation to a temporary file and run hsc3-graphs to print me."
    self writeHaskellTo: '/tmp/st.hs'.
    'hsc3-graphs fragments hs min dump-ugens /tmp/st.hs' systemCommand
  !
!

!Ugen methodsFor: 'printing'!
  printHaskellOn: fd
    "Print the name I am bound to in hsc3."
    '_u_', self ugenId asString putOn: fd
  !
  printHaskellDefinitionOn: fd
    "Print my hsc3 definition."
    | mce nc |
    '(mkUGen Nothing all_rates ' putOn: fd.
    ugenRate isArray
      ifTrue: ['(Right ' putOn: fd. ugenRate printHaskellArrayOn: fd. ') ' putOn: fd]
      ifFalse: ['(Left ' putOn: fd. ugenRate asString putOn: fd. ') ' putOn: fd].
    ugenName asString printHaskellOn: fd.
    fd space.
    '[' putOn: fd.
    paramValues do: [ :item | item printHaskellOn: fd. fd space ] separatedBy: [ ',' putOn: fd ].
    '] ' putOn: fd.
    mce := ['(Just [' putOn: fd.
            mceValues do: [ :item | item printHaskellOn: fd. fd space ] separatedBy: [ ',' putOn: fd].
            ']) ' putOn: fd].
    mceNames isEmpty ifTrue: ['Nothing ' putOn: fd] ifFalse: mce.
    nc := ['(length (mceChannels ' putOn: fd.
           (mceValues at: 1) printHaskellOn: fd.
           '))' putOn: fd].
    numChan = #mceInput ifTrue: nc ifFalse: [numChan asString putOn: fd].
    fd space.
    '(Special ' putOn: fd.
    specialIndex notNil ifTrue: [specialIndex asString putOn: fd] ifFalse: ['0' putOn: fd]
  . ') ' putOn: fd.
    '(UId ' putOn: fd.
    self ugenId asString putOn: fd.
    ')' putOn: fd.
    ')' putOn: fd.
  !
!

!Rate methodsFor: 'printing'!
  printHaskellOn: fd
    "Print my hsc3 representation."
    '(rewriteToRate ' putOn: fd.
    rate asString putOn: fd.
    fd space.
    source printHaskellOn: fd.
    ')' putOn: fd
  !
!

!Mce methodsFor: 'printing'!
  printHaskellOn: fd
    "Print my hsc3 representation."
    '(' putOn: fd.
    transform notNil ifTrue: [ transform do: [ :x | x printHaskellOn: fd. fd space ] ].
    source printHaskellOn: fd.
    ')' putOn: fd
  !
!

!Mrg methodsFor: 'printing'!
  printHaskellOn: fd
    "Print my hsc3 representation."
    '(mrg2 ' putOn: fd.
    lhs printHaskellOn: fd.
    ' ' putOn: fd.
    rhs printHaskellOn: fd.
    ')' putOn: fd
  !
!

!Mix methodsFor: 'printing'!
  printHaskellOn: fd
    "Print my hsc3 representation."
    '(mix ' putOn: fd.
    source printHaskellOn: fd.
    ')' putOn: fd
  !
!

!Fraction methodsFor: 'printing'!
  printHaskellOn: fd
    "Print my hsc3 representation."
    (self asFloat asString) putOn: fd
  !
!

!Number methodsFor: 'printing'!
  printHaskellOn: fd
    "Print my hsc3 representation."
    self asString putOn: fd
  !
!

!Float methodsFor: 'printing'!
  printHaskellOn: fd
    "Print my hsc3 representation.  Special cased for Infinity"
    | txt |
    txt := (self = Float infinity) ifTrue: [ 'dinf' ] ifFalse: [ self asString ].
    txt putOn: fd
  !
!

!Array methodsFor: 'printing'!
  printHaskellArrayOn: fd
    "Print my hsc3 representation."
    '[' putOn: fd.
    self do: [ :x | x printHaskellOn: fd. ] separatedBy: [ ',' putOn: fd ].
    ']' putOn: fd
  !
  printHaskellOn: fd
    "Print my hsc3 Mce representation."
    '(mce ' putOn: fd.
    self printHaskellArrayOn: fd.
    ')' putOn: fd
  !
  writeHaskellTo: fn
    "Allow Array to be root of Ugen graph"
    self asUgen writeHaskellTo: fn
  !
  play
    "Allow Array to be root of Ugen graph"
    self asUgen play.
  !
  draw
    "Allow Array to be root of Ugen graph"
    self asUgen draw.
  !
!

!Interval methodsFor: 'printing'!
  printHaskellOn: fd
    "Print my hsc3 Mce representation."
    self asArray printHaskellOn: fd
  !
!

!String methodsFor: 'printing'!
  asHaskell
    ^'"' , self , '"'
  !
  printHaskellOn: fd
    "Print my hsc3 representation."
    fd
      nextPut: $";
      nextPutAll: self;
      nextPut: $"
  !
!

!Symbol methodsFor: 'printing'!
  printHaskellOn: fd
    "Print my hsc3 representation."
    self putOn: fd
  !
!

!Splay methodsFor: 'printing'!
  printHaskellOn: fd
    "Print my hsc3 representation."
    '(splay ' putOn: fd.
    paramValues do: [ :item | item printHaskellOn: fd ] separatedBy: [ ' ' putOn: fd ].
    ') ' putOn: fd
  !
!

!True methodsFor: 'printing'!
  asHaskell
    'True'
  !
  printHaskellOn: fd
    "Print my hsc3 representation."
    'True' putOn: fd
  !
!

!False methodsFor: 'printing'!
  asHaskell
    'False'
  !
  printHaskellOn: fd
    "Print my hsc3 representation."
    'False' putOn: fd
  !
!

!Control methodsFor: 'printing'!
  printHaskellOn: fd
    "Print my hsc3 representation."
    fd
      nextPutAll: '(control kr ';
      nextPutAll: self name asString asHaskell;
      space;
      nextPutAll: self init asString;
      nextPutAll: ') '
  !
!

!Constant methodsFor: 'printing'!
  printHaskellOn: fd
    "Print my hsc3 representation."
    fd
      nextPutAll: '(constant (';
      nextPutAll: self k asString;
      nextPutAll: ')) '
  !
!
