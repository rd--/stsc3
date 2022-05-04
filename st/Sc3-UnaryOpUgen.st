Ugen subclass: #UnaryOpUgen
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
!
UnaryOpUgen comment: 'Unary Operators'!

!UnaryOpUgen methodsFor: 'initialization'!
specialIndex: p1 a: p2
    ugenName :=#UnaryOpUGen. "note case"
    paramNames :=#(#a).
    mceNames :=#().
    ugenRate :=#(0).
    numChan :=1.
    specialIndex :=p1.
    ugenId :=nil.
    paramValues :={p2}.
    mceValues :={}.
!!

!UnaryOpUgen class methodsFor: 'instance creation'!
specialIndex: p1 a: p2
    "Create unary operator Ugen"
    ^self new specialIndex: p1 a: p2
!!
