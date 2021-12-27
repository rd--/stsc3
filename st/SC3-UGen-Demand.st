UGen subclass: #DmdFor instanceVariableNames: 'dur reset level' classVariableNames: '' poolDictionaries: '' category: 'Sound-SC3'!

DmdFor comment: 'Demand with duration input (Duty).'!

!DmdFor class methodsFor: 'instance creation'!
  dur: dur reset: reset level: level
    ^Duty dur: dur reset: reset doneAction: 0 level: level
  !
  primaryFactoryMethod
    ^#dur:reset:level:
  !
!

UGen subclass: #TDmdFor instanceVariableNames: 'dur reset level' classVariableNames: '' poolDictionaries: '' category: 'Sound-SC3'!

TDmdFor comment: 'Demand with duration input (Duty).'!

!TDmdFor class methodsFor: 'instance creation'!
  dur: dur reset: reset level: level
    ^TDuty dur: dur reset: reset doneAction: 0 level: level gapFirst: 0
  !
  primaryFactoryMethod
    ^#dur:reset:level:
  !
!

UGen subclass: #DmdOn instanceVariableNames: 'trig reset demandUGens' classVariableNames: '' poolDictionaries: '' category: 'Sound-SC3'!

DmdOn comment: 'Demand with trigger input (Demand).'!

!DmdOn class methodsFor: 'instance creation'!
  trig: trig reset: reset demandUGens: demandUGens
    ^Demand trig: trig reset: reset demandUGens: demandUGens
  !
  primaryFactoryMethod
    ^#trig:reset:demandUGens:
  !
!

UGen subclass: #Choose
  instanceVariableNames: 'repeats list'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Choose comment: 'Random sequence generator.'!

!Choose methodsFor: 'accessing'!
  repeats ^repeats!
  repeats: x repeats := x!
  list ^list!
  list: x list := x!
!

!Choose methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Drand.
    paramNames := #(#repeats).
    mceNames := #(#list).
    ugenRate := #dr.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    repeats := 1.0!
!

!Choose class methodsFor: 'instance creation'!
  repeats: repeats list: list
    |u|
    u := self new.
    u repeats: repeats.
    u list: list.
    ^u!
  repeats: repeats list: list mul: mul ^(Choose repeats: repeats list: list) * mul!
  repeats: repeats list: list mul: mul add: add ^(Choose repeats: repeats list: list) * mul + add!
  primaryFactoryMethod ^#repeats:list:!
!

UGen subclass: #Seq
  instanceVariableNames: 'repeats list'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Seq comment: 'Sequence generator.'!

!Seq methodsFor: 'accessing'!
  repeats ^repeats!
  repeats: x repeats := x!
  list ^list!
  list: x list := x!
!

!Seq methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Dseq.
    paramNames := #(#repeats).
    mceNames := #(#list).
    ugenRate := #dr.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    repeats := 1.0!
!

!Seq class methodsFor: 'instance creation'!
  repeats: repeats list: list
    |u|
    u := self new.
    u repeats: repeats.
    u list: list.
    ^u!
  repeats: repeats list: list mul: mul ^(Seq repeats: repeats list: list) * mul!
  repeats: repeats list: list mul: mul add: add ^(Seq repeats: repeats list: list) * mul + add!
  primaryFactoryMethod ^#repeats:list:!
!

UGen subclass: #Shuf
  instanceVariableNames: 'repeats list'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Shuf comment: 'Demand rate random sequence generator'!

!Shuf methodsFor: 'accessing'!
  repeats ^repeats!
  repeats: x repeats := x!
  list ^list!
  list: x list := x!
!

!Shuf methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Dshuf.
    paramNames := #(#repeats).
    mceNames := #(#list).
    ugenRate := #dr.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    repeats := 1.0!
!

!Shuf class methodsFor: 'instance creation'!
  repeats: repeats list: list
    |u|
    u := self new.
    u repeats: repeats.
    u list: list.
    ^u!
  repeats: repeats list: list mul: mul ^(Shuf repeats: repeats list: list) * mul!
  repeats: repeats list: list mul: mul add: add ^(Shuf repeats: repeats list: list) * mul + add!
  primaryFactoryMethod ^#repeats:list:!
!
