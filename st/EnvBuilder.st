Object subclass: #EnvBuilder
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

EnvBuilder comment: 'Envelope types'!

!EnvBuilder methodsFor: 'envelope specification'!
asEnvSpec
    "Myself as an EnvSpec"
    self subclassResponsibility!
asArray
    "Myself as a coordinary array suitable for EnvGen "
    ^self asEnvSpec coordinateArrayForEnvGen!
asEnvGen: g withDoneAction: d
    "Myself as an EnvGen instance with indicated gate and doneAction"
    ^(self asEnvSpec) asEnvGen: g withDoneAction: d!
asEnvGen: g
    "Myself as an EnvGen instance with indicated gate and default doneAction (0=doNothing)"
    ^self asEnvGen: g withDoneAction: 0!
asEnvGen
    "Myself as an EnvGen instance with default gate (1) and doneAction (0=doNothing)"
    ^self asEnvGen: 1 withDoneAction: 0!
!
