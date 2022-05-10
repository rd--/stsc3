!Object methodsFor: 'stsc3'!
adaptToFloat: rcvr andSend: selector
    ^ self adaptToNumber: rcvr andSend: selector!
adaptToFloat: rcvr andCompare: selector
    ^ self adaptToFloat: rcvr andSend: selector!
adaptToInteger: rcvr andSend: selector
    ^ self adaptToNumber: rcvr andSend: selector!
isCollection
    ^false!
value
    ^self!
!
