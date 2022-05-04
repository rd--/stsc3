AbstractUgen subclass: #DynKlank
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

DynKlank comment: 'Dynamic klank, set of non-fixed resonating filters'!

!DynKlank class methodsFor: 'instance creation'!
input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef
    | gen |
    gen := [ :ix |
        | f a d |
        f := specificationsArrayRef at: ix.
        a := specificationsArrayRef at: (ix + 1).
        d := specificationsArrayRef at: (ix + 2).
        Ringz in: input freq: f * freqscale + freqoffset decaytime: d * decayscale mul: a ].
    ^((1 to: specificationsArrayRef size by: 3) collect: gen) sum!
input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef mul: mul
    ^(self input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef) * mul!
primaryFactoryMethod
    ^#input:freqscale:freqoffset:decayscale:specificationsArrayRef:!
!



