!SequenceableCollection methodsFor: 'stsc3'!
adaptToCollection: rcvr andSend: selector
    ^rcvr withExtending: self collect: [ :rcvrElement :myElement | rcvrElement perform: selector with: myElement ]!
!
