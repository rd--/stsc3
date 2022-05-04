AbstractUgen subclass: #LinSeg
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LinSeg comment: 'Line Segment Envelope'!

!LinSeg class methodsFor: 'instance creation'!
gate: g coord: c
    | l t |
    l := (1 to: c size by: 2) collect: [ :i | c at: i ].
    t := (2 to: c size by: 2) collect: [ :i | c at: i ].
    ^(EnvSpec levels: l times: t curves: #(#lin)) asEnvGen: g
!
primaryFactoryMethod
    ^#gate:coord:!
!



