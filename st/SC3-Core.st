SystemOrganization addCategory: #'SC3'!

Object subclass: #UGen
 instanceVariableNames: 'ugenName paramNames mceNames numChan ugenRate specialIndex'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'
!
!UGen commentStamp: '<historical>' prior: 0! Unit Generator!

!UGen methodsFor: 'accessing'!
 mceNames ^mceNames !
 mceNames: x mceNames := x !
 numChan ^numChan !
 numChan: x numChan := x !
 paramNames ^paramNames !
 paramNames: x paramNames := x !
 specialIndex ^specialIndex !
 specialIndex: x specialIndex := x !
 ugenName ^ugenName !
 ugenName: x ugenName := x !
 ugenRate ^ugenRate !
 ugenRate: x ugenRate := x !
!

UGen subclass: #BinaryOpUGen
 instanceVariableNames: 'a b'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'
!
!BinaryOpUGen commentStamp: '<historical>' prior: 0! Binary Operators !

!BinaryOpUGen methodsFor: 'accessing'!
 a ^a !
 a: x a := x !
 b ^b !
 b: x b := x !
!

!BinaryOpUGen class methodsFor: 'instance creation'!
specialIndex: specialIndex a: a b: b |u| u := self new . u ugenName: #BinaryOpUGen . u paramNames: #(a b) . u mceNames: #() . u ugenRate: #ar . u numChan: 1 . u specialIndex: specialIndex . u a: a . u b: b . ^u !
!

UGen subclass: #UnaryOpUGen
 instanceVariableNames: 'a'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'
!
!UnaryOpUGen commentStamp: '<historical>' prior: 0! Unary Operators !

!UnaryOpUGen methodsFor: 'accessing'!
 a ^a !
 a: x a := x !
!

!UnaryOpUGen class methodsFor: 'instance creation'!
specialIndex: specialIndex a: a |u| u := self new . u ugenName: #UnaryOpUGen . u paramNames: #(a) . u mceNames: #() . u ugenRate: #ar . u numChan: 1 . u specialIndex: specialIndex . u a: a . ^u !
!

UGen subclass: #Rate
 instanceVariableNames: 'source rate'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'
!
!Rate commentStamp: '<historical>' prior: 0! Audio Rate !

!Rate methodsFor: 'accessing'!
 rate ^ rate !
 rate: x rate := x !
 source ^ source !
 source: x source := x !
!

!Rate class methodsFor: 'instance creation'!
 source: p1 rate: p2 |r| r := self new . r source: p1 . r rate: p2 . ^r !
!

!UGen methodsFor: 'rate control'!
 ar ^ Rate source: self rate: #ar!
 kr ^ Rate source: self rate: #kr!
 ir ^ Rate source: self rate: #ir!
 dr ^ Rate source: self rate: #dr!
!

UGen subclass: #MCE
 instanceVariableNames: 'source'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'
!
!MCE commentStamp: '<historical>' prior: 0! Multiple Channel Expansion!

!MCE methodsFor: 'accessing'!
 source: x source := x !
 source ^ source !
 size ^ source size !
!

!MCE class methodsFor: 'instance creation'!
 source: x ^ self new source: x !
!

!Array methodsFor: 'multi-channel expansion'!
 mce ^ MCE source: self !
!

!Array methodsFor: 'rearranging'!
 transpose ^ (1 to: self first size) collect: [:c | self collect: [:r | r at: c]] !
!

!Number methodsFor: 'enumerating'!
 to: n collect: f ^ ((self to: n) asArray) collect: [:i| f value: i] !
 arrayFill: f ^ 1 to: self collect: [:i| f value: i] !
 replicate: x ^ self arrayFill: [:_i| x] !
 mceFill: f ^ (self arrayFill: f) mce !
 mixFill: f ^ (self arrayFill: f) sum !
!

UGen subclass: #Mix
 instanceVariableNames: 'source'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'
!
!Mix commentStamp: '<historical>' prior: 0! UGen Mixer!

!Mix methodsFor: 'accessing'!
 source ^ source !
 source: x source := x !
!

!Mix class methodsFor: 'instance creation'!
 source: x ^ self new source: x !
!

!UGen methodsFor: 'arithmetic'!
 mul: mul add: add ^ MulAdd in: self mul: mul add: add !
 mix ^ Mix source: self !
 round ^ self round: 1.0 !
!

Object subclass: #SC3
 instanceVariableNames: ''
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'
!
!SC3 commentStamp: '<historical>' prior: 0! SuperCollider3!

!SC3 class methodsFor: 'server control'!
 reset OSProcess command: 'hsc3-scsynth reset' !
!

UGen subclass: #MRG
 instanceVariableNames: 'lhs rhs'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'
!
!MRG commentStamp: '<historical>' prior: 0! Multiple Root Graph!

!MRG methodsFor: 'accessing'!
 lhs ^ lhs !
 lhs: x lhs := x !
 rhs ^ rhs !
 rhs: x rhs := x !
!

!MRG class methodsFor: 'instance creation'!
 lhs: x rhs: y |u| u := self new . u lhs: x . u rhs: y . ^ u !
!

UGen subclass: #Control
 instanceVariableNames: 'name init'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'
!
!Control commentStamp: '<historical>' prior: 0! Control Input!

!Control methodsFor: 'accessing'!
 name ^name !
 name: x name := x !
 init ^init !
 init: x init := x !
!

!Control class methodsFor: 'instance creation'!
 name: p1 init: p2 |u| u := self new . u name: p1 . u init: p2 . ^ u !
!
