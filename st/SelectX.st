AbstractUgen subclass: #SelectX
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

SelectX comment: 'Mix one output from many sources'!

!SelectX class methodsFor: 'instance creation'!
which: which array: array
    ^XFade2 inA: (Select which: (which roundTo: 2) array: array)
              inB: (Select which: (which truncateTo: 2) + 1 array: array)
              pan: ((which * 2 - 1) fold2: 1)
              level: 1!
primaryFactoryMethod
    ^#which:array:!
!



