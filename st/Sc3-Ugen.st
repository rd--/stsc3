AbstractUgen subclass: #Ugen
  instanceVariableNames: 'ugenName paramNames mceNames numChan ugenRate specialIndex ugenId'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
!

Ugen comment: 'Unit Generator.
ugenId is assigned during graph serialisation since identityHash is not unique.
All Ugens have mul: and add: methods but there are no variables associated with these.
'!

!Ugen methodsFor: 'initialising'!
ugenName: p1 paramNames: p2 mceNames: p3 numChan: p4 ugenRate: p5 specialIndex: p6 ugenId: p7
    ugenName := p1.
    paramNames := p2.
    mceNames := p3.
    numChan := p4.
    ugenRate := p5.
    specialIndex := p6.
    ugenId := p7.
!
!

!Ugen methodsFor: 'accessing'!
  mceNames ^mceNames!
  mceNames: x mceNames := x!
  numChan ^numChan!
  numChan: x numChan := x!
  paramNames ^paramNames!
  paramNames: x paramNames := x!
  specialIndex ^specialIndex!
  specialIndex: x specialIndex := x!
  ugenName ^ugenName!
  ugenName: x ugenName := x!
  ugenRate ^ugenRate!
  ugenRate: x ugenRate := x!
  ugenId ^ugenId!
  ugenId: x ugenId := x!
!

!Ugen methodsFor: 'collecting'!
  addToUgenSet: c
    | addFunc |
    addFunc := [ :x | (self perform: x) addToUgenSet: c ].
    c add: self.
    paramNames do: addFunc.
    mceNames do: addFunc.
  !
!

!Ugen methodsFor: 'arithmetic'!
  mul: x
     "((p mul: q) mul: r) is (p * q * r) not (p * r)."
      ^x = 1 ifTrue: [ self ] ifFalse: [ self * x ]
  !
  add: x
     "((p add: q) add: r) is (p + q + r) not (p + r)."
      ^x = 0 ifTrue: [ self ] ifFalse: [ self + x ]
  !
  mul: mul add: add
    ^(self mul: mul) add: add
  !
  mulAdd: mul value: add
    ".stc alias for mul:add:"
    ^self mul: mul add: add
  !
!
