PsuedoUgen subclass: #BufRec
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BufRec comment: 'Record signal into a Buffer'!

!BufRec class methodsFor: 'instance creation'!
bufnum: bufnum reset: reset inputArray: inputArray
    ^RecordBuf bufnum: bufnum offset: 0 recLevel: 1 preLevel: 0 run: 1 loop: 1 trigger: reset doneAction: 0 inputArray: inputArray!
primaryFactoryMethod
    ^#bufnum:reset:inputArray:!
!



