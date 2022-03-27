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
  primaryFactoryMethod ^#repeats:list:!
!

UGen subclass: #Seq
  instanceVariableNames: 'repeats array'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Seq comment: 'Sequence generator.'!

!Seq methodsFor: 'accessing'!
  repeats ^repeats!
  repeats: x repeats := x!
  array ^array!
  array: x array := x!
!

!Seq methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Dseq.
    paramNames := #(#repeats).
    mceNames := #(#array).
    ugenRate := #dr.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    repeats := 1.0!
!

!Seq class methodsFor: 'instance creation'!
  repeats: repeats array: array
    |u|
    u := self new.
    u repeats: repeats.
    u array: array.
    ^u!
  primaryFactoryMethod ^#repeats:array:!
!

UGen subclass: #Ser
  instanceVariableNames: 'length start step'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Ser comment: 'Demand rate arithmetic series UGen.'!

!Ser methodsFor: 'accessing'!
  length ^length!
  length: x length := x!
  start ^start!
  start: x start := x!
  step ^step!
  step: x step := x!
!

!Ser methodsFor: 'initialization'!
  initialize
    super initialize.
    ugenName := #Dseries.
    paramNames := #(#length #start #step).
    mceNames := #().
    ugenRate := #dr.
    numChan := 1.
    specialIndex := 0.
    ugenId := nil.
    length := 100000000.0. start := 1.0. step := 1.0!
!

!Ser class methodsFor: 'instance creation'!
  length: length start: start step: step
    |u|
    u := self new.
    u length: length.
    u start: start.
    u step: step.
    ^u!
  primaryFactoryMethod ^#length:start:step:!
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
  primaryFactoryMethod ^#repeats:list:!
!
