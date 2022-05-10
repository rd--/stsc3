!Float class methodsFor: 'stsc3'!
infinity
    ^FloatD infinity!
!

!Float methodsFor: 'stsc3'!
adaptToInteger: rcvr andSend: selector
    ^ rcvr asFloat perform: selector with: self!
!
