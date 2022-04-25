Object subclass: #ScPrimitive
  instanceVariableNames: 'ugenName numChan ugenRate specialIndex ugenId inputValues mrg'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

ScPrimitive comment: 'SuperCollider primitive, i.e. a unit generator.'!

!ScPrimitive methodsFor: 'Syndef'!
  inputRate
      ^ugenRate
  !
!

Object subclass: #ScPort
  instanceVariableNames: 'primitive index'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

ScPort comment: 'Output port for multiple-channel ScPrimitive.'!

!ScPort methodsFor: 'Syndef'!
  inputRate
      ^primitive inputRate
  !
!

!Number methodsFor: 'Syndef'!
  inputRate
      ^#ir
  !
!


"

o := LFSaw freq: #(8 7.23) iphase: 0 mul: 3 add: 80.
f := LFSaw freq: 0.4 iphase: 0 mul: 24 add: o.
s := SinOsc freq: (f midiCps kr) phase: 0 mul: 0.04.
x := CombN in: s maxdelaytime: 0.2 delaytime: 0.2 decaytime: 4 mul: 0.1

o inspect
o ugenSet

x := SinOsc freq: 440 phase: 0
x ugenSet

"
