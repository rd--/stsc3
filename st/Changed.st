AbstractUgen subclass: #Changed
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Changed comment: 'Triggers when a value changes'!

!Changed class methodsFor: 'instance creation'!
input: input threshold: threshold
    ^(HPZ1 in: input) abs > threshold!
primaryFactoryMethod
    ^#input:threshold:!
!



