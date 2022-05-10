!Number methodsFor: 'stsc3'!
adaptToFloat: rcvr andSend: selector
    ^ rcvr perform: selector with: self asFloat!
adaptToInteger: rcvr andSend: selector
    ^ self subclassResponsibility!
adaptToCollection: rcvr andSend: selector
    ^rcvr collect: [:element | element perform: selector with: self]!
isZero
    ^self = 0!
** aNumber
    ^BinaryOpUgen selector: #raisedTo: a: self b: aNumber!
!
